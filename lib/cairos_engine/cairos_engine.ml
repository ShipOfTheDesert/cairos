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

  (* Two-tier entrypoint validation, in order, steps 1–7:

     Tier 1 (shape, fail-fast):
       1. Price and signal frames have the same [Index.t].
       2. Price and signal frames have the same columns in the same order.
       3. Rebalance index is non-empty.

     Tier 2 (calendar, aggregate every offending date):
       4. Every rebalance date >= price frame's first timestamp.
       5. Every rebalance date matches a row in the price frame.
       6. Every rebalance date has a T+1 open available.

     Tier 3 (fail-fast):
       7. At least one (rebalance, instrument) target weight != 0.0
          (structural justification for [Trade.t Nonempty.t]).

     On success, returns the [int array] mapping each rebalance row to its
     bar index in the price frame — pre-computed here to avoid timestamp
     lookups inside the loop. *)
  let validate_inputs ~price_idx ~signal_idx ~price_columns ~signal_columns
      ~rebalance_index ~signal_data =
    let n_bars = Cairos.Index.length price_idx in
    let n_rebal = Cairos.Index.length rebalance_index in
    if not (indices_match price_idx signal_idx) then
      Error "Backtest.run: price and signal frames have different indices"
    else if price_columns <> signal_columns then
      Error
        ("Backtest.run: price and signal frames have different columns ("
        ^ String.concat ", " price_columns
        ^ " vs "
        ^ String.concat ", " signal_columns
        ^ ")")
    else if n_rebal = 0 then Error "Backtest.run: rebalance index is empty"
    else
      let price_ts = Cairos.Index.timestamps price_idx in
      let rebal_ts = Cairos.Index.timestamps rebalance_index in
      let bar_indices = Array.make n_rebal (-1) in
      let cal_errs = ref [] in
      let last_bar_idx = n_bars - 1 in
      let first_ts = price_ts.(0) in
      Array.iteri
        (fun i t ->
          if Ptime.compare t first_ts < 0 then
            cal_errs :=
              Printf.sprintf
                "rebalance date %s precedes price frame's first bar"
                (Ptime.to_rfc3339 t)
              :: !cal_errs
          else
            match find_row price_ts t with
            | None ->
                cal_errs :=
                  Printf.sprintf
                    "rebalance date %s does not match any price-frame row"
                    (Ptime.to_rfc3339 t)
                  :: !cal_errs
            | Some idx ->
                bar_indices.(i) <- idx;
                if idx = last_bar_idx then
                  cal_errs :=
                    Printf.sprintf
                      "rebalance date %s is the last bar — no T+1 open \
                       available"
                      (Ptime.to_rfc3339 t)
                    :: !cal_errs)
        rebal_ts;
      if !cal_errs <> [] then
        Error
          ("Backtest.run: calendar precondition failures:\n  - "
          ^ String.concat "\n  - " (List.rev !cal_errs))
      else
        let n_cols = Array.length signal_data in
        let any_nonzero = ref false in
        Array.iter
          (fun bar_idx ->
            for j = 0 to n_cols - 1 do
              let w = signal_data.(j).(bar_idx) in
              if (not (Float.is_nan w)) && not (Float.equal w 0.0) then
                any_nonzero := true
            done)
          bar_indices;
        if not !any_nonzero then
          Error
            "Backtest.run: no non-zero target weight at any rebalance date \
             (PRD 0053 FR-11)"
        else Ok bar_indices

  (* Loop body. Entrypoint validation has already pre-computed
     [rebalance_bars] (the row index in the price frame for each rebalance
     date) so the loop performs no timestamp lookups. *)
  let run_loop ~price_idx ~price_columns ~price_data ~signal_data
      ~rebalance_bars ~commission ~slippage =
    let n_cols = Array.length price_data in
    let n_bars = Cairos.Index.length price_idx in
    let price_ts = Cairos.Index.timestamps price_idx in
    let columns_arr = Array.of_list price_columns in

    let is_rebalance = Array.make n_bars false in
    Array.iter (fun bar_idx -> is_rebalance.(bar_idx) <- true) rebalance_bars;

    let current_weights = Array.make n_cols 0.0 in
    let current_nav = ref 1.0 in
    let in_flight : in_flight option array = Array.make n_cols None in
    let trade_acc : Trade.t list ref = ref [] in
    let push_trade tr = trade_acc := tr :: !trade_acc in

    let equity_buf = Array.make n_bars 0.0 in
    let weights_buf = Array.make_matrix n_cols n_bars 0.0 in

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
          let w = current_weights.(j) in
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
          let target = signal_data.(j).(t) in
          let w_old = current_weights.(j) in
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
            let target = signal_data.(j).(t) in
            let w_old = current_weights.(j) in
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

        for j = 0 to n_cols - 1 do
          current_weights.(j) <- signal_data.(j).(t)
        done;
        current_nav := nav_after_cost
      end;

      equity_buf.(t) <- !current_nav;
      for j = 0 to n_cols - 1 do
        weights_buf.(j).(t) <- current_weights.(j)
      done
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
          let nx = Nx.create Nx.float64 [| n_bars |] weights_buf.(j) in
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
             guarantees the rebalance index is non-empty, and step 7
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
        ~rebalance_index ~signal_data
    with
    | Error e -> Error e
    | Ok rebalance_bars ->
        Ok
          (run_loop ~price_idx ~price_columns ~price_data ~signal_data
             ~rebalance_bars ~commission ~slippage)
end
