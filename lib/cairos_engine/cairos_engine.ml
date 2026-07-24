module Trade = struct
  type t = {
    entry_timestamp : Ptime.t;
    exit_timestamp : Ptime.t;
    instrument : string;
    entry_price : float;
    exit_price : float;
    pnl : float;
    holding_period_bars : int;
  }
end

module Backtest = struct
  type calendar_violation =
    | Precedes_first_bar of { timestamp : Ptime.t }
    | No_matching_row of { timestamp : Ptime.t }
    | Last_bar_no_next_open of { timestamp : Ptime.t }

  type err =
    | Index_mismatch
    | Column_mismatch of { price : string list; signal : string list }
    | Empty_rebalance_index
    | Calendar_violations of calendar_violation Cairos.Nonempty.t
    | No_nonzero_target_weight
    | Nan_signal_at_rebalance of {
        cells : (Ptime.t * string) Cairos.Nonempty.t;
      }
    | Invalid_price of { cells : (Ptime.t * string * float) Cairos.Nonempty.t }

  let calendar_violation_to_string = function
    | Precedes_first_bar { timestamp } ->
        Printf.sprintf "rebalance date %s precedes price frame's first bar"
          (Ptime.to_rfc3339 timestamp)
    | No_matching_row { timestamp } ->
        Printf.sprintf "rebalance date %s does not match any price-frame row"
          (Ptime.to_rfc3339 timestamp)
    | Last_bar_no_next_open { timestamp } ->
        Printf.sprintf
          "rebalance date %s is the last bar — no T+1 open available"
          (Ptime.to_rfc3339 timestamp)

  let err_to_string = function
    | Index_mismatch ->
        "Backtest.run: price and signal frames have different indices"
    | Column_mismatch { price; signal } ->
        Printf.sprintf
          "Backtest.run: price and signal frames have different columns (%s vs \
           %s)"
          (String.concat ", " price)
          (String.concat ", " signal)
    | Empty_rebalance_index -> "Backtest.run: rebalance index is empty"
    | Calendar_violations vs ->
        "Backtest.run: calendar precondition failures:\n  - "
        ^ String.concat "\n  - "
            (List.map calendar_violation_to_string (Cairos.Nonempty.to_list vs))
    | No_nonzero_target_weight ->
        "Backtest.run: no non-zero target weight at any rebalance date"
    | Nan_signal_at_rebalance { cells } ->
        "Backtest.run: NaN signal at rebalance date:\n  - "
        ^ String.concat "\n  - "
            (List.map
               (fun (t, inst) ->
                 Printf.sprintf "%s %s" (Ptime.to_rfc3339 t) inst)
               (Cairos.Nonempty.to_list cells))
    | Invalid_price { cells } ->
        "Backtest.run: price is not strictly positive and finite where the \
         loop reads it:\n\
        \  - "
        ^ String.concat "\n  - "
            (List.map
               (fun (t, inst, v) ->
                 Printf.sprintf "%s %s = %g" (Ptime.to_rfc3339 t) inst v)
               (Cairos.Nonempty.to_list cells))

  type 'freq result = {
    equity_curve : ('freq, (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t;
    returns : ('freq, (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t;
    weights : 'freq Cairos.Frame.t;
    trades : Trade.t Cairos.Nonempty.t;
  }

  (* In-flight trade record. The instrument is identified by its column
     index in the price frame; entry fields are immutable post-inception;
     [pnl_acc] accumulates per-bar MTM contributions and
     per-rebalance cost charges over the trade's lifetime. *)
  type in_flight = {
    entry_timestamp : Ptime.t;
    entry_bar : int;
    entry_price : float;
    mutable pnl_acc : float;
  }

  let frame_index frame =
    match Cairos.Frame.columns frame with
    | [] ->
        (* Unreachable: [Cairos.Frame.t] is non-empty by construction
           ([Frame.of_series] takes a [Nonempty.t] of pairs). *)
        assert false
    | name :: _ -> (
        match Cairos.Frame.get name frame with
        | Some s -> Cairos.Series.index s
        | None ->
            (* Unreachable: [name] came from [Frame.columns frame]. *)
            assert false)

  let frame_data frame names =
    Array.of_list
      (List.map
         (fun name ->
           match Cairos.Frame.get name frame with
           | Some s -> Nx.to_array (Cairos.Series.values s)
           | None ->
               (* Unreachable: [name] came from [Frame.columns frame]. *)
               assert false)
         names)

  let indices_match a b =
    let ta = Cairos.Index.timestamps a in
    let tb = Cairos.Index.timestamps b in
    let la = Array.length ta in
    let lb = Array.length tb in
    if la <> lb then false
    else
      let rec loop i = i >= la || (Ptime.equal ta.(i) tb.(i) && loop (i + 1)) in
      loop 0

  let find_row price_ts t =
    let n = Array.length price_ts in
    let rec loop i =
      if i >= n then None
      else if Ptime.equal price_ts.(i) t then Some i
      else loop (i + 1)
    in
    loop 0

  (* Everything [validate_inputs] pre-computes for [run_loop], so the loop
     performs no timestamp lookups and carries no second model of the held
     weights. [held.(j).(t)] is the target weight of instrument [j] in force
     at bar [t] — the target from the most recent rebalance date <= [t], or
     [0.0] before the first. It is both what step 9 scans against and what
     the loop reads at every bar, so validation and execution cannot drift:
     a divergence between the two would fail open, letting an unvalidated
     price reach NAV. It is also the frame returned as [result.weights]. *)
  type validated = {
    rebalance_bars : int array;
    is_rebalance : bool array;
    held : float array array;
  }

  (* Two-tier entrypoint validation, in order, steps 1–9:

     Tier 1 (shape, fail-fast):
       1. Price and signal frames have the same [Index.t].
       2. Price and signal frames have the same columns in the same order.
       3. Rebalance index is non-empty.

     Tier 2 (calendar, aggregate every offending date):
       4. Every rebalance date >= price frame's first timestamp.
       5. Every rebalance date matches a row in the price frame.
       6. Every rebalance date has a T+1 open available.

     Tier 3 (fail-fast):
       7. No signal cell on a rebalance row is NaN. Ordered before step 8
          because an all-NaN rebalance row also has no non-zero weight, and
          the NaN is the cause rather than the consequence.
       8. At least one (rebalance, instrument) target weight != 0.0
          (structural justification for [Trade.t Nonempty.t]).
       9. Every price cell the loop reads at non-zero exposure is strictly
          positive and finite. Scoped to the read set rather than the whole
          frame, so an instrument that never trades may carry any prices:
          a cell is read as a T+1 execution price when the instrument's
          weight delta at a rebalance date is non-zero, and as a
          mark-to-market price when [held.(t) <> 0.0 || held.(t-1) <> 0.0]
          — two-sided because mark-to-market at bar [t] applies
          [held.(t-1)] to both [price.(t)] and [price.(t-1)], so an exit
          bar carries zero held weight yet is still read at full exposure.
          Ordered last: it needs the held-weight path, which step 7 must
          first make NaN-free.

     On success, returns the [validated] record the loop runs on. *)
  let validate_inputs ~price_idx ~signal_idx ~price_columns ~signal_columns
      ~rebalance_index ~price_data ~signal_data =
    let ( let* ) = Result.bind in
    (* [reject cond err] fails the tier when [cond] holds; [reject_any cells
       f] fails it when the offender list is non-empty, lifting that list —
       already in reporting order — into the [Nonempty.t] the variant
       demands. The [None] arm is the tier passing, not an unreachable
       case. *)
    let reject cond err = if cond then Error err else Ok () in
    let reject_any cells f =
      match Cairos.Nonempty.of_list cells with
      | Some ne -> Error (f ne)
      | None -> Ok ()
    in
    let n_bars = Cairos.Index.length price_idx in
    let n_rebal = Cairos.Index.length rebalance_index in
    let n_cols = Array.length signal_data in
    let* () =
      reject (not (indices_match price_idx signal_idx)) Index_mismatch
    in
    let* () =
      reject
        (price_columns <> signal_columns)
        (Column_mismatch { price = price_columns; signal = signal_columns })
    in
    let* () = reject (n_rebal = 0) Empty_rebalance_index in
    let price_ts = Cairos.Index.timestamps price_idx in
    let rebal_ts = Cairos.Index.timestamps rebalance_index in
    let columns_arr = Array.of_list price_columns in
    let bar_indices = Array.make n_rebal (-1) in
    let cal_errs = ref [] in
    let last_bar_idx = n_bars - 1 in
    let first_ts = price_ts.(0) in
    Array.iteri
      (fun i t ->
        if Ptime.compare t first_ts < 0 then
          cal_errs := Precedes_first_bar { timestamp = t } :: !cal_errs
        else
          match find_row price_ts t with
          | None -> cal_errs := No_matching_row { timestamp = t } :: !cal_errs
          | Some idx ->
              bar_indices.(i) <- idx;
              if idx = last_bar_idx then
                cal_errs := Last_bar_no_next_open { timestamp = t } :: !cal_errs)
      rebal_ts;
    let* () =
      reject_any (List.rev !cal_errs) (fun vs -> Calendar_violations vs)
    in
    let nan_cells = ref [] in
    let any_nonzero = ref false in
    Array.iter
      (fun bar_idx ->
        for j = 0 to n_cols - 1 do
          let w = signal_data.(j).(bar_idx) in
          if Float.is_nan w then
            nan_cells := (price_ts.(bar_idx), columns_arr.(j)) :: !nan_cells
          else if not (Float.equal w 0.0) then any_nonzero := true
        done)
      bar_indices;
    let* () =
      reject_any (List.rev !nan_cells) (fun cells ->
          Nan_signal_at_rebalance { cells })
    in
    let* () = reject (not !any_nonzero) No_nonzero_target_weight in
    let is_rebalance = Array.make n_bars false in
    Array.iter (fun bar_idx -> is_rebalance.(bar_idx) <- true) bar_indices;
    let held = Array.make_matrix n_cols n_bars 0.0 in
    for j = 0 to n_cols - 1 do
      let w = ref 0.0 in
      for t = 0 to n_bars - 1 do
        if is_rebalance.(t) then w := signal_data.(j).(t);
        held.(j).(t) <- !w
      done
    done;
    (* Scanned column-major, matching the frame's layout; the offender list
       is sorted back into bar-then-column order at the end. It is empty on
       every accepted run, so the sort costs nothing on the happy path. *)
    let bad_prices = ref [] in
    for j = n_cols - 1 downto 0 do
      for t = n_bars - 1 downto 0 do
        let held_now = held.(j).(t) in
        let held_prev = if t = 0 then 0.0 else held.(j).(t - 1) in
        let mtm_read =
          (not (Float.equal held_now 0.0)) || not (Float.equal held_prev 0.0)
        in
        let exec_read =
          t > 0
          && is_rebalance.(t - 1)
          &&
          (* The loop's own test, spelled the same way: [dw = target -. w_old]
             against zero, not [target] against [w_old]. The two agree for
             every finite weight and diverge for an infinite one, where
             [inf -. inf] is NaN and the loop therefore executes a trade. *)
          let w_old = if t >= 2 then held.(j).(t - 2) else 0.0 in
          not (Float.equal (held_prev -. w_old) 0.0)
        in
        if mtm_read || exec_read then begin
          let p = price_data.(j).(t) in
          if not (Float.is_finite p && p > 0.0) then
            bad_prices := (t, j, p) :: !bad_prices
        end
      done
    done;
    let* () =
      reject_any
        (List.map
           (fun (t, j, p) -> (price_ts.(t), columns_arr.(j), p))
           (List.stable_sort
              (fun (t1, _, _) (t2, _, _) -> Int.compare t1 t2)
              !bad_prices))
        (fun cells -> Invalid_price { cells })
    in
    Ok { rebalance_bars = bar_indices; is_rebalance; held }

  (* Loop body. Entrypoint validation has already pre-computed everything in
     [validated] — the rebalance bar indices, the rebalance mask, and the
     held-weight path — so the loop performs no timestamp lookups and builds
     no second model of the weights. [held] is read here and returned as
     [result.weights]; see [validated]'s comment for why the two must be one
     array. *)
  let run_loop ~price_idx ~price_columns ~price_data ~validated ~commission
      ~slippage =
    let { rebalance_bars = _; is_rebalance; held } = validated in
    let n_cols = Array.length price_data in
    let n_bars = Cairos.Index.length price_idx in
    let price_ts = Cairos.Index.timestamps price_idx in
    let columns_arr = Array.of_list price_columns in

    (* The weight in force at bar [t], i.e. before bar [t]'s own rebalance
       takes effect: [0.0] at bar 0, [held.(j).(t-1)] after. *)
    let weight_before j t = if t = 0 then 0.0 else held.(j).(t - 1) in

    let current_nav = ref 1.0 in
    let in_flight : in_flight option array = Array.make n_cols None in
    let trade_acc : Trade.t list ref = ref [] in
    let push_trade tr = trade_acc := tr :: !trade_acc in

    let equity_buf = Array.make n_bars 0.0 in

    let cs = commission +. slippage in
    let dws = Array.make n_cols 0.0 in
    let costs = Array.make n_cols 0.0 in

    for t = 0 to n_bars - 1 do
      (* Mark-to-market step.

         The compounding formula
         [nav_t = nav_{t-1} *. sum_j (w_j *. p_t /. p_{t-1})] is
         well-defined only when [sum_j w_j = 1.0]. The standard
         fractional-weight extension
         [nav_t = nav_{t-1} *. (1.0 +. sum_j (w_j *. (p_t /. p_{t-1} -. 1.0)))]
         is equivalent at full investment and produces realistic
         loss-on-price-up for shorts. *)
      if t > 0 then begin
        let nav_pre = !current_nav in
        let total_ret = ref 0.0 in
        for j = 0 to n_cols - 1 do
          let w = weight_before j t in
          if not (Float.equal w 0.0) then begin
            let p_now = price_data.(j).(t) in
            let p_prev = price_data.(j).(t - 1) in
            let r = (p_now /. p_prev) -. 1.0 in
            let dpnl_j = w *. nav_pre *. r in
            total_ret := !total_ret +. (w *. r);
            match in_flight.(j) with
            | None -> ()
            | Some ift -> ift.pnl_acc <- ift.pnl_acc +. dpnl_j
          end
        done;
        current_nav := nav_pre *. (1.0 +. !total_ret)
      end;

      (* Rebalance step. *)
      if is_rebalance.(t) then begin
        let exec_bar = t + 1 in
        let nav_after_mtm = !current_nav in
        let total_cost = ref 0.0 in
        for j = 0 to n_cols - 1 do
          let target = held.(j).(t) in
          let w_old = weight_before j t in
          let dw = target -. w_old in
          (* Cost is turnover (|Δw|) as a fraction of pre-cost NAV, not of
             price level. *)
          let cost = cs *. Float.abs dw *. nav_after_mtm in
          dws.(j) <- dw;
          costs.(j) <- cost;
          total_cost := !total_cost +. cost
        done;
        let nav_after_cost = nav_after_mtm -. !total_cost in

        for j = 0 to n_cols - 1 do
          let dw = dws.(j) in
          if not (Float.equal dw 0.0) then begin
            let target = held.(j).(t) in
            let w_old = weight_before j t in
            let cost_j = costs.(j) in
            let inst = columns_arr.(j) in
            let exec_price = price_data.(j).(exec_bar) in
            let exec_ts = price_ts.(exec_bar) in
            if Float.equal w_old 0.0 then
              (* Inception. Full cost charged to the new trade. *)
              in_flight.(j) <-
                Some
                  {
                    entry_timestamp = exec_ts;
                    entry_bar = exec_bar;
                    entry_price = exec_price;
                    pnl_acc = -.cost_j;
                  }
            else if Float.equal target 0.0 then (
              (* Resolution. Full cost charged to the closing trade. *)
              match in_flight.(j) with
              | None ->
                  (* Unreachable: [w_old <> 0.0] implies an open trade. *)
                  assert false
              | Some ift ->
                  push_trade
                    {
                      entry_timestamp = ift.entry_timestamp;
                      exit_timestamp = exec_ts;
                      instrument = inst;
                      entry_price = ift.entry_price;
                      exit_price = exec_price;
                      pnl = ift.pnl_acc -. cost_j;
                      holding_period_bars = exec_bar - ift.entry_bar;
                    };
                  in_flight.(j) <- None)
            else if
              (w_old > 0.0 && target > 0.0) || (w_old < 0.0 && target < 0.0)
            then
              (* Same-direction size adjustment. The in-flight trade
                 absorbs the full cost; entry fields stay fixed. *)
              match in_flight.(j) with
              | None -> assert false
              | Some ift -> ift.pnl_acc <- ift.pnl_acc -. cost_j
            else begin
              (* Sign flip — close the in-flight trade and incept a new
                 one at the same execution bar. The single per-instrument
                 cost is split proportionally to each side's contribution
                 to [|weight_delta|]: for a sign flip [|weight_delta| =
                 |w_old| +. |w_new|], so [closing_share / cost_j = |w_old|
                 /. (|w_old| +. |w_new|)]. The total split sums to [cost_j],
                 preserving the sum-of-pnl identity. *)
              let abs_old = Float.abs w_old in
              let abs_new = Float.abs target in
              let denom = abs_old +. abs_new in
              let close_share = cost_j *. abs_old /. denom in
              let open_share = cost_j *. abs_new /. denom in
              (match in_flight.(j) with
              | None -> assert false
              | Some ift ->
                  push_trade
                    {
                      entry_timestamp = ift.entry_timestamp;
                      exit_timestamp = exec_ts;
                      instrument = inst;
                      entry_price = ift.entry_price;
                      exit_price = exec_price;
                      pnl = ift.pnl_acc -. close_share;
                      holding_period_bars = exec_bar - ift.entry_bar;
                    });
              in_flight.(j) <-
                Some
                  {
                    entry_timestamp = exec_ts;
                    entry_bar = exec_bar;
                    entry_price = exec_price;
                    pnl_acc = -.open_share;
                  }
            end
          end
        done;

        current_nav := nav_after_cost
      end;

      equity_buf.(t) <- !current_nav
    done;

    (* End-of-backtest force-close: every still-open trade resolves at the
       last bar's close with no exit cost. *)
    let last_bar = n_bars - 1 in
    let last_ts = price_ts.(last_bar) in
    for j = 0 to n_cols - 1 do
      match in_flight.(j) with
      | None -> ()
      | Some ift ->
          let exit_price = price_data.(j).(last_bar) in
          push_trade
            {
              entry_timestamp = ift.entry_timestamp;
              exit_timestamp = last_ts;
              instrument = columns_arr.(j);
              entry_price = ift.entry_price;
              exit_price;
              pnl = ift.pnl_acc;
              holding_period_bars = last_bar - ift.entry_bar;
            };
          in_flight.(j) <- None
    done;

    (* Output construction. The price frame's [Index.t] is shared
       physically across [equity_curve], [returns], and the [weights]
       frame. *)
    let equity_nx = Nx.create Nx.float64 [| n_bars |] equity_buf in
    let equity_curve = Cairos.Series.make_unsafe price_idx equity_nx in
    let returns = Cairos.Series.pct_change equity_curve in

    let weights_pairs =
      List.mapi
        (fun j name ->
          let nx = Nx.create Nx.float64 [| n_bars |] held.(j) in
          let s = Cairos.Series.make_unsafe price_idx nx in
          (name, s))
        price_columns
    in
    let weights =
      match weights_pairs with
      | [] ->
          (* Unreachable: [price_columns] is non-empty (Frame invariant). *)
          assert false
      | first :: rest -> (
          let ne = Cairos.Nonempty.make first rest in
          match Cairos.Frame.of_series ne with
          | Ok f -> f
          | Error _ ->
              (* Unreachable: all series share [price_idx] physically;
                 column names inherit unique-by-construction from the
                 input price frame. *)
              assert false)
    in

    (* Trade-list non-emptiness construction. *)
    let trades_list = List.rev !trade_acc in
    let trades =
      match Cairos.Nonempty.of_list trades_list with
      | Some ts -> ts
      | None ->
          (* Unreachable under the entrypoint contract: validation step 3
             guarantees the rebalance index is non-empty, and step 8
             guarantees at least one (rebalance, instrument) pair carries
             a non-zero target weight. The first such pair produces an
             inception, which becomes a trade either via a later
             resolution or via end-of-backtest force-close. *)
          assert false
    in

    { equity_curve; returns; weights; trades }

  let run ~price_frame ~signal_frame ~rebalance_index ~commission ~slippage =
    let price_idx = frame_index price_frame in
    let signal_idx = frame_index signal_frame in
    let price_columns = Cairos.Frame.columns price_frame in
    let signal_columns = Cairos.Frame.columns signal_frame in
    let price_data = frame_data price_frame price_columns in
    let signal_data = frame_data signal_frame signal_columns in
    match
      validate_inputs ~price_idx ~signal_idx ~price_columns ~signal_columns
        ~rebalance_index ~price_data ~signal_data
    with
    | Error e -> Error e
    | Ok validated ->
        Ok
          (run_loop ~price_idx ~price_columns ~price_data ~validated ~commission
             ~slippage)
end
