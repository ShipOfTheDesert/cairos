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

  (* Per-instrument position state as the loop carries it through the bars.

     [Open] holds the weight in force together with the leg that weight
     opened, so "non-zero held weight with no in-flight leg" has no
     representation. Three unreachable error branches existed only to rule
     that combination out by comment; the rebalance branch now matches on
     this value instead of testing [Float.equal w_old 0.0] against a
     separate [in_flight option array], so the leg arrives bound rather than
     as an [option] whose [None] arm could not be taken.

     The weight is not a second model of the held weights. It is
     never derived here: every [Open] is written with a [target] read
     straight out of [validated.held], which remains the array validation
     scans in step 9 and the source of the [result.weights] frame. The two
     agree at every bar because a position is rewritten exactly when the
     weight changes — at a non-zero [weight_delta] — and [held] carries the
     same value forward between rebalances.

     [Open]'s weight is never [0.0]: a [target] of [0.0] at a non-zero
     [weight_delta] takes the resolution branch, which writes [Flat]. *)
  type position = Flat | Open of { weight : float; leg : in_flight }

  (* The loop's trade log, in the two states it can be in from the witness
     bar onward. It is created at that bar rather than before the loop,
     which is what removes the empty state a final [Nonempty.of_list]
     would have had to rule out by comment.

     [Awaiting] is "no trade has closed yet", and it carries the witness
     leg — the trade the loop opened unconditionally at the witness bar.
     Nothing has closed, so that leg has not been resolved and is still
     [positions.(witness.col)]; the end-of-backtest force close turns it
     into the trade that makes the log non-empty. A size adjustment
     rewrites the position around the same physically shared leg, so the
     one the log holds stays the live one.

     [Closed] is "at least one trade has closed", newest first — a cons
     accumulator, reversed once at the end. Its non-emptiness is the
     record's, not a list's. *)
  type trade_log =
    | Awaiting of in_flight
    | Closed of { newest : Trade.t; older : Trade.t list }

  (* Column-major projection of a frame's values, in [Frame.columns] order.
     Total: [Frame.to_series] hands back every column the frame owns, so
     there is no name to look up and no lookup to fail. It is the only
     bulk copy [run] performs, and [validate_inputs] does not reach it
     until the shape tier has accepted both frames. *)
  let frame_data frame =
    Array.of_list
      (List.map
         (fun (_, s) -> Nx.to_array (Cairos.Series.values s))
         (Cairos.Nonempty.to_list (Cairos.Frame.to_series frame)))

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

  (* Steps 4–6 for one rebalance date, and the outcome the whole-frame scan
     needs from it: either the price-frame bar the loop will rebalance on,
     or the precondition the date violates.

     The last-bar case resolves to a row and still violates, so it is a
     [Violation] carrying no bar rather than a [Bar]: the scan discards
     every resolved bar the moment any date violates, so a bar index that
     only a rejected run could read is one the type need not carry. *)
  type rebalance_resolution = Bar of int | Violation of calendar_violation

  let resolve_rebalance_date ~price_ts ~first_ts ~last_bar_idx t =
    if Ptime.compare t first_ts < 0 then
      Violation (Precedes_first_bar { timestamp = t })
    else
      match find_row price_ts t with
      | None -> Violation (No_matching_row { timestamp = t })
      | Some idx ->
          if idx = last_bar_idx then
            Violation (Last_bar_no_next_open { timestamp = t })
          else Bar idx

  (* Steps 7 and 8 for one rebalance bar: the bar's NaN signal cells in
     frame-column order, and its lowest-indexed column carrying a non-zero
     target weight, if any.

     Step 8 reports the column rather than a bare "some column did":
     [run_loop] opens its first trade at the whole-frame minimum of these,
     and needs the column to do it. The scan below walks columns in
     descending order, so the last write wins and the answer is the
     lowest-indexed one.

     Called with [acc = []] this is exactly that bar's offenders, which is
     the unit Post-MVP v1's shared execution core validates a single
     incoming bar with. The whole-frame scan passes the bars it has already
     visited instead, and visits bars in reverse, so the aggregate comes
     out in bar-then-column order with no final reversal and no re-traversal
     of the accumulator. *)
  let validate_rebalance_bar ~signal_data ~columns_arr ~timestamp ~bar acc =
    let rec loop j cells nonzero_col =
      if j < 0 then (cells, nonzero_col)
      else
        let w = signal_data.(j).(bar) in
        if Float.is_nan w then
          loop (j - 1) ((timestamp, columns_arr.(j)) :: cells) nonzero_col
        else if Float.equal w 0.0 then loop (j - 1) cells nonzero_col
        else loop (j - 1) cells (Some j)
    in
    loop (Array.length signal_data - 1) acc None

  (* Step 9 for one bar: the bar's price cells that the loop reads at
     non-zero exposure and that are not strictly positive and finite, in
     frame-column order. Same accumulator contract as
     [validate_rebalance_bar].

     Reads [held] rather than a per-bar slice of it: the bar's read set
     spans [bar-2 .. bar], and the array is the one model validation and
     execution share. *)
  let validate_price_bar ~price_data ~held ~is_rebalance ~columns_arr ~timestamp
      ~bar acc =
    let rec loop j cells =
      if j < 0 then cells
      else
        let held_now = held.(j).(bar) in
        let held_prev = if bar = 0 then 0.0 else held.(j).(bar - 1) in
        let mtm_read =
          (not (Float.equal held_now 0.0)) || not (Float.equal held_prev 0.0)
        in
        let exec_read =
          bar > 0
          && is_rebalance.(bar - 1)
          &&
          (* The loop's own test, spelled the same way: [dw = target -. w_old]
             against zero, not [target] against [w_old]. The two agree for
             every finite weight and diverge for an infinite one, where
             [inf -. inf] is NaN and the loop therefore executes a trade. *)
          let w_old = if bar >= 2 then held.(j).(bar - 2) else 0.0 in
          not (Float.equal (held_prev -. w_old) 0.0)
        in
        if mtm_read || exec_read then
          let p = price_data.(j).(bar) in
          if not (Float.is_finite p && p > 0.0) then
            loop (j - 1) ((timestamp, columns_arr.(j), p) :: cells)
          else loop (j - 1) cells
        else loop (j - 1) cells
    in
    loop (Array.length price_data - 1) acc

  (* The first (rebalance bar, column) pair carrying a non-zero target
     weight, in bar-then-column order — the witness that makes step 8's
     rejection an [Error] rather than a promise. Nothing is held before
     [bar], so the loop opens its first trade there unconditionally:
     [col]'s target is non-zero and its position is [Flat], which is the
     inception case with no test to make. That unconditional inception is
     what lets [run_loop]'s trade log be born holding a leg, and therefore
     what makes [result.trades] a [Nonempty.t] by construction. *)
  type witness = { bar : int; col : int }

  (* Everything [validate_inputs] pre-computes for [run_loop], so the loop
     performs no timestamp lookups and carries no second model of the held
     weights. [held.(j).(t)] is the target weight of instrument [j] in force
     at bar [t] — the target from the most recent rebalance date <= [t], or
     [0.0] before the first. It is both what step 9 scans against and what
     the loop reads at every bar, so validation and execution cannot drift:
     a divergence between the two would fail open, letting an unvalidated
     price reach NAV. It is also the frame returned as [result.weights].

     [price_idx], [price_columns] and [price_data] are the price frame's
     projections. They live here rather than being re-derived by [run]
     because [run] no longer touches either frame before validation,
     and re-projecting [price_data] afterwards would be a second
     bulk copy of the whole frame. *)
  type 'freq validated = {
    price_idx : 'freq Cairos.Index.t;
    price_columns : string Cairos.Nonempty.t;
    price_data : float array array;
    rebalance_bars : int array;
    is_rebalance : bool array;
    held : float array array;
    witness : witness;
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

     Takes the frames rather than their projections: tier 1 needs only the
     index and the column names, both of which a [Frame.t] owns outright,
     so the bulk projection happens between tier 1 and tier 2 rather than
     before the function is entered.

     Tiers 2 and 3 are whole-frame scans over per-unit checks:
     [resolve_rebalance_date] decides steps 4–6 for one date,
     [validate_rebalance_bar] steps 7 and 8 for one rebalance bar, and
     [validate_price_bar] step 9 for one bar. This function is the scan
     that applies them and accumulates; none of the three fails fast, so
     the aggregate offender lists are the concatenation of the
     units' outputs in visit order. Only the held-weight path between
     steps 8 and 9 is genuinely whole-frame: [held.(j).(t)] carries
     forward from [t-1], so it is built by one sweep and read by every
     unit, which is also what keeps validation and execution on one model.

     On success, returns the [validated] record the loop runs on. *)
  let validate_inputs ~price_frame ~signal_frame ~rebalance_index =
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
    let price_idx = Cairos.Frame.index price_frame in
    let signal_idx = Cairos.Frame.index signal_frame in
    let price_columns = Cairos.Frame.columns price_frame in
    let signal_columns = Cairos.Frame.columns signal_frame in
    let n_bars = Cairos.Index.length price_idx in
    let n_rebal = Cairos.Index.length rebalance_index in
    let* () =
      reject (not (indices_match price_idx signal_idx)) Index_mismatch
    in
    let* () =
      (* [Column_mismatch]'s payload is for human diagnosis rather than an
         invariant, so it stays [string list] while the columns themselves
         arrive as [Nonempty.t]. The conversion belongs here at the
         construction site, not upstream. *)
      reject
        (price_columns <> signal_columns)
        (Column_mismatch
           {
             price = Cairos.Nonempty.to_list price_columns;
             signal = Cairos.Nonempty.to_list signal_columns;
           })
    in
    let* () = reject (n_rebal = 0) Empty_rebalance_index in
    (* Tier 1 has accepted both frames; only now is either one projected. *)
    let price_data = frame_data price_frame in
    let signal_data = frame_data signal_frame in
    let n_cols = Array.length signal_data in
    let price_ts = Cairos.Index.timestamps price_idx in
    let rebal_ts = Cairos.Index.timestamps rebalance_index in
    let columns_arr = Array.of_list (Cairos.Nonempty.to_list price_columns) in
    let last_bar_idx = n_bars - 1 in
    let first_ts = price_ts.(0) in
    (* [fold_right] over the dates so both lists come out in rebalance-index
       order without a reversal, and so the resolved bars are read off the
       [Bar] constructor rather than out of a sentinel-initialised array
       whose unresolved slots only a rejected run could reach. *)
    let bars, cal_errs =
      Array.fold_right
        (fun t (bars, errs) ->
          match resolve_rebalance_date ~price_ts ~first_ts ~last_bar_idx t with
          | Bar b -> (b :: bars, errs)
          | Violation v -> (bars, v :: errs))
        rebal_ts ([], [])
    in
    let* () = reject_any cal_errs (fun vs -> Calendar_violations vs) in
    let bar_indices = Array.of_list bars in
    (* Rebalance bars visited in reverse so each bar's offenders prepend onto
       the bars after it: the aggregate is in rebalance-index order, and
       column order within each, by construction rather than by a final
       sort or reversal. *)
    let nan_cells = ref [] in
    (* The earliest bar wins, compared rather than assumed from the visit
       order: [rebalance_bars] is in rebalance-index order, which is the
       date order of an [Index.t] but not necessarily ascending in
       price-frame bars. *)
    let witness = ref None in
    for i = Array.length bar_indices - 1 downto 0 do
      let bar = bar_indices.(i) in
      let cells, nonzero_col =
        validate_rebalance_bar ~signal_data ~columns_arr
          ~timestamp:price_ts.(bar) ~bar !nan_cells
      in
      nan_cells := cells;
      match (nonzero_col, !witness) with
      | None, _ -> ()
      | Some _, Some w when w.bar < bar -> ()
      | Some col, _ -> witness := Some { bar; col }
    done;
    let* () =
      reject_any !nan_cells (fun cells -> Nan_signal_at_rebalance { cells })
    in
    (* The [None] arm is step 8 rejecting, not an unreachable case. *)
    let* witness =
      match !witness with
      | Some w -> Ok w
      | None -> Error No_nonzero_target_weight
    in
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
    (* Bars visited in reverse, as above: bar-then-column order falls out of
       the traversal, so there is no intermediate [(bar, column)] encoding
       to sort back and no reversal to drop. *)
    let bad_prices = ref [] in
    for t = n_bars - 1 downto 0 do
      bad_prices :=
        validate_price_bar ~price_data ~held ~is_rebalance ~columns_arr
          ~timestamp:price_ts.(t) ~bar:t !bad_prices
    done;
    let* () = reject_any !bad_prices (fun cells -> Invalid_price { cells }) in
    Ok
      {
        price_idx;
        price_columns;
        price_data;
        rebalance_bars = bar_indices;
        is_rebalance;
        held;
        witness;
      }

  (* Loop body. Entrypoint validation has already pre-computed everything in
     [validated] — the rebalance bar indices, the rebalance mask, the
     held-weight path, and the witness — so the loop performs no timestamp
     lookups and builds no second model of the weights. [held] is read here
     and returned as [result.weights]; see [validated]'s comment for why the
     two must be one array.

     The bars are walked in three stretches rather than one, so that the
     trade log's first leg is created by straight-line code:

       [0 .. witness.bar - 1]  nothing is held and nothing can be, so only
                               a cost charge can move NAV;
       [witness.bar]           every position is [Flat] and [witness.col]'s
                               target is non-zero, so its inception needs
                               no test — this is where the log is born;
       [witness.bar + 1 ..]    the general body.

     Validation step 8 is what makes the middle stretch unconditional, and
     the middle stretch is what makes [result.trades] non-empty by
     construction. *)
  let run_loop ~validated ~price_frame ~commission ~slippage =
    let {
      price_idx;
      price_columns;
      price_data;
      rebalance_bars = _;
      is_rebalance;
      held;
      witness;
    } =
      validated
    in
    let n_cols = Array.length price_data in
    let n_bars = Cairos.Index.length price_idx in
    let price_ts = Cairos.Index.timestamps price_idx in
    let columns_arr = Array.of_list (Cairos.Nonempty.to_list price_columns) in

    let current_nav = ref 1.0 in
    (* Every instrument starts flat. This is the [held.(j).(-1)] the weight
       in force at bar [t] — [held.(j).(t-1)] — would need at [t = 0]. *)
    let positions : position array = Array.make n_cols Flat in

    let equity_buf = Array.make n_bars 0.0 in

    let cs = commission +. slippage in
    let dws = Array.make n_cols 0.0 in
    let costs = Array.make n_cols 0.0 in

    (* Pass 1 of the rebalance step at [bar]: fills [dws] and [costs] and
       returns the total charge. Cost is turnover (|Δw|) as a fraction of
       pre-cost NAV, not of price level. Shared by the witness bar and the
       general body so the two cannot compute it differently. *)
    let rebalance_costs ~bar ~nav =
      let total = ref 0.0 in
      for j = 0 to n_cols - 1 do
        let target = held.(j).(bar) in
        let w_old =
          match positions.(j) with
          | Flat -> 0.0
          | Open { weight; _ } -> weight
        in
        let dw = target -. w_old in
        let cost = cs *. Float.abs dw *. nav in
        dws.(j) <- dw;
        costs.(j) <- cost;
        total := !total +. cost
      done;
      !total
    in

    (* Opens a leg on instrument [j] and returns it. Shared by the three
       inception sites — the witness bar, the general inception branch, and
       the far side of a sign flip — so the leg's entry fields are written
       in one place. *)
    let incept j ~target ~exec_bar ~exec_ts ~exec_price ~cost_j =
      let leg =
        {
          entry_timestamp = exec_ts;
          entry_bar = exec_bar;
          entry_price = exec_price;
          pnl_acc = -.cost_j;
        }
      in
      positions.(j) <- Open { weight = target; leg };
      leg
    in

    (* Bars before the witness bar. Every position is [Flat] through them:
       the first non-zero target weight is at [witness.bar], so no earlier
       rebalance has a non-zero weight delta to act on. That makes the
       mark-to-market step a no-op — [nav *. (1.0 +. 0.0)] is [nav] for
       every float, non-finite ones included — and the rebalance step's
       execution pass empty. Only the cost charge survives, and it is
       charged rather than assumed away: it is zero for a finite cost model
       but [nan] for a non-finite one, since [nan *. 0.0] is [nan]. *)
    for t = 0 to witness.bar - 1 do
      if is_rebalance.(t) then
        current_nav := !current_nav -. rebalance_costs ~bar:t ~nav:!current_nav;
      equity_buf.(t) <- !current_nav
    done;

    (* The witness bar. Mark-to-market is still a no-op, and the execution
       pass can only incept, so it is spelled out here instead of running
       the general body: [witness.col] is taken out of the loop and opened
       unconditionally, which is what gives the log a leg to be born with.
       The other columns' inceptions are order-independent — no trade
       closes at this bar, so nothing observes the sequence. *)
    let nav_after_mtm = !current_nav in
    let total_cost = rebalance_costs ~bar:witness.bar ~nav:nav_after_mtm in
    current_nav := nav_after_mtm -. total_cost;
    let exec_bar = witness.bar + 1 in
    let exec_ts = price_ts.(exec_bar) in
    for j = 0 to n_cols - 1 do
      if j <> witness.col && not (Float.equal dws.(j) 0.0) then
        ignore
          (incept j
             ~target:held.(j).(witness.bar)
             ~exec_bar ~exec_ts
             ~exec_price:price_data.(j).(exec_bar)
             ~cost_j:costs.(j))
    done;
    let log =
      ref
        (Awaiting
           (incept witness.col
              ~target:held.(witness.col).(witness.bar)
              ~exec_bar ~exec_ts
              ~exec_price:price_data.(witness.col).(exec_bar)
              ~cost_j:costs.(witness.col)))
    in
    let push_trade tr =
      log :=
        match !log with
        | Awaiting _ -> Closed { newest = tr; older = [] }
        | Closed { newest; older } ->
            Closed { newest = tr; older = newest :: older }
    in
    equity_buf.(witness.bar) <- !current_nav;

    for t = witness.bar + 1 to n_bars - 1 do
      (* Mark-to-market step.

         The compounding formula
         [nav_t = nav_{t-1} *. sum_j (w_j *. p_t /. p_{t-1})] is
         well-defined only when [sum_j w_j = 1.0]. The standard
         fractional-weight extension
         [nav_t = nav_{t-1} *. (1.0 +. sum_j (w_j *. (p_t /. p_{t-1} -. 1.0)))]
         is equivalent at full investment and produces realistic
         loss-on-price-up for shorts.

         Unconditional: this stretch starts at [witness.bar + 1] and
         [witness.bar >= 0], so [t >= 1] and [price_data.(j).(t - 1)] is
         always in range. The bars that cannot mark to market are the ones
         before the witness bar, and they are a separate loop. *)
      let nav_pre = !current_nav in
      let total_ret = ref 0.0 in
      for j = 0 to n_cols - 1 do
        (* [Flat] is exactly the old [Float.equal w 0.0] skip: an [Open]
           weight is never zero, so the set of instruments marked to
           market — and therefore the set of price cells read, which
           validation step 9 scopes itself to — is unchanged. *)
        match positions.(j) with
        | Flat -> ()
        | Open { weight = w; leg } ->
            let p_now = price_data.(j).(t) in
            let p_prev = price_data.(j).(t - 1) in
            let r = (p_now /. p_prev) -. 1.0 in
            let dpnl_j = w *. nav_pre *. r in
            total_ret := !total_ret +. (w *. r);
            leg.pnl_acc <- leg.pnl_acc +. dpnl_j
      done;
      current_nav := nav_pre *. (1.0 +. !total_ret);

      (* Rebalance step. *)
      if is_rebalance.(t) then begin
        let exec_bar = t + 1 in
        let nav_after_mtm = !current_nav in
        let total_cost = rebalance_costs ~bar:t ~nav:nav_after_mtm in
        let nav_after_cost = nav_after_mtm -. total_cost in

        for j = 0 to n_cols - 1 do
          let dw = dws.(j) in
          if not (Float.equal dw 0.0) then begin
            let target = held.(j).(t) in
            let cost_j = costs.(j) in
            let inst = columns_arr.(j) in
            let exec_price = price_data.(j).(exec_bar) in
            let exec_ts = price_ts.(exec_bar) in
            (* Branching on the position rather than on [Float.equal w_old
               0.0] preserves the old order of cases — [Flat] is exactly the
               zero-weight case — and binds the leg in the three that need
               it. *)
            match positions.(j) with
            | Flat ->
                (* Inception. [dw <> 0.0] at a zero weight means
                   [target <> 0.0]. Full cost charged to the new trade. *)
                ignore (incept j ~target ~exec_bar ~exec_ts ~exec_price ~cost_j)
            | Open { weight = w_old; leg } ->
                if Float.equal target 0.0 then begin
                  (* Resolution. Full cost charged to the closing trade. *)
                  push_trade
                    {
                      entry_timestamp = leg.entry_timestamp;
                      exit_timestamp = exec_ts;
                      instrument = inst;
                      entry_price = leg.entry_price;
                      exit_price = exec_price;
                      pnl = leg.pnl_acc -. cost_j;
                      holding_period_bars = exec_bar - leg.entry_bar;
                    };
                  positions.(j) <- Flat
                end
                else if
                  (w_old > 0.0 && target > 0.0) || (w_old < 0.0 && target < 0.0)
                then begin
                  (* Same-direction size adjustment. The in-flight trade
                     absorbs the full cost; entry fields stay fixed, so the
                     same leg carries over under the new weight. *)
                  leg.pnl_acc <- leg.pnl_acc -. cost_j;
                  positions.(j) <- Open { weight = target; leg }
                end
                else begin
                  (* Sign flip — close the in-flight trade and incept a new
                     one at the same execution bar. The single per-instrument
                     cost is split proportionally to each side's contribution
                     to [|weight_delta|]: for a sign flip [|weight_delta| =
                     |w_old| +. |w_new|], so [closing_share / cost_j = |w_old|
                     /. (|w_old| +. |w_new|)]. The total split sums to
                     [cost_j], preserving the sum-of-pnl identity. *)
                  let abs_old = Float.abs w_old in
                  let abs_new = Float.abs target in
                  let denom = abs_old +. abs_new in
                  let close_share = cost_j *. abs_old /. denom in
                  let open_share = cost_j *. abs_new /. denom in
                  push_trade
                    {
                      entry_timestamp = leg.entry_timestamp;
                      exit_timestamp = exec_ts;
                      instrument = inst;
                      entry_price = leg.entry_price;
                      exit_price = exec_price;
                      pnl = leg.pnl_acc -. close_share;
                      holding_period_bars = exec_bar - leg.entry_bar;
                    };
                  ignore
                    (incept j ~target ~exec_bar ~exec_ts ~exec_price
                       ~cost_j:open_share)
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
    let last_col = n_cols - 1 in
    let last_ts = price_ts.(last_bar) in
    let force_close j leg : Trade.t =
      {
        entry_timestamp = leg.entry_timestamp;
        exit_timestamp = last_ts;
        instrument = columns_arr.(j);
        entry_price = leg.entry_price;
        exit_price = price_data.(j).(last_bar);
        pnl = leg.pnl_acc;
        holding_period_bars = last_bar - leg.entry_bar;
      }
    in
    (* [force_closes ~lo ~hi acc] prepends the force-close trades of columns
       [lo .. hi] onto [acc] in descending column order — newest-first, the
       orientation [trade_log] carries. *)
    let rec force_closes ~lo ~hi acc =
      if lo > hi then acc
      else
        let acc =
          match positions.(lo) with
          | Flat -> acc
          | Open { weight = _; leg } -> force_close lo leg :: acc
        in
        force_closes ~lo:(lo + 1) ~hi acc
    in
    (* [chronological newest ~older ~onto] reverses the newest-first run
       [newest :: older] onto [onto]. The run arrives as a head and a list
       rather than as a list, so the result is a [Nonempty.t] with no
       emptiness test to make. *)
    let rec chronological newest ~older ~onto =
      match older with
      | [] -> Cairos.Nonempty.make newest onto
      | x :: xs -> chronological x ~older:xs ~onto:(newest :: onto)
    in
    let trades =
      match !log with
      | Closed { newest; older } ->
          chronological newest ~older
            ~onto:(List.rev (force_closes ~lo:0 ~hi:last_col []))
      | Awaiting leg ->
          (* Nothing closed during the loop, so the witness leg was never
             resolved and is still [positions.(witness.col)]: force-closing
             it is the trade that makes [trades] non-empty. Neither column
             range covers it, so it is closed here and only here. *)
          chronological
            (force_close witness.col leg)
            ~older:(force_closes ~lo:0 ~hi:(witness.col - 1) [])
            ~onto:
              (List.rev (force_closes ~lo:(witness.col + 1) ~hi:last_col []))
    in

    (* Output construction. The price frame's [Index.t] is shared
       physically across [equity_curve], [returns], and the [weights]
       frame — [mapi_cells] carries the input frame's index through. *)
    let equity_nx = Nx.create Nx.float64 [| n_bars |] equity_buf in
    let equity_curve = Cairos.Series.make_unsafe price_idx equity_nx in
    let returns = Cairos.Series.pct_change equity_curve in

    (* The weights frame has the price frame's shape exactly — same columns
       in the same order, same index — and [held] is indexed by the same
       column positions, so it is a cell rewrite of the price frame rather
       than a fresh [of_series] whose duplicate-name and index-mismatch
       errors could not arise. *)
    let weights =
      Cairos.Frame.mapi_cells
        ~f:(fun ~col ~name:_ ~row _ -> held.(col).(row))
        price_frame
    in

    { equity_curve; returns; weights; trades }

  let run ~price_frame ~signal_frame ~rebalance_index ~commission ~slippage =
    match validate_inputs ~price_frame ~signal_frame ~rebalance_index with
    | Error e -> Error e
    | Ok validated ->
        Ok (run_loop ~validated ~price_frame ~commission ~slippage)
end
