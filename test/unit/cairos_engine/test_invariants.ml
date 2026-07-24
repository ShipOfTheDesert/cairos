(* Layer 3 — QCheck invariants for [Cairos_engine.Backtest.run].

   The ten properties (count = 200 each). Inputs come from a single
   [engine_inputs_arb] generator that produces (price [Frame], signal
   [Frame], rebalance [Index.t], commission, slippage) tuples
   structurally satisfying the nine entrypoint preconditions.
   Determinism is pinned via [Qcheck_gen.pin_seed_from_env].

   The properties exercise behaviour the type system does not pin:
   the loop's mark-to-market sign, the cost-deduction direction, the
   trade accumulator's count of round-trip resolutions, the
   weights-frame's between-rebalance constancy, the
   sum-of-pnl/equity reconciliation that [Trade.t.exit_timestamp]'s
   doc-comment promises, and the cost model's structural guarantees —
   price-scale invariance of the NAV-relative cost notional, cost
   monotonicity in the commission, and agreement with a
   contract-derived frictionless recursion in the zero-cost limit. *)

(* === Generator ===

   [engine_inputs_arb] produces inputs satisfying the nine
   preconditions structurally:
     1. price/signal share the same [Index.t] (built from the same
        synthetic timestamp array).
     2. price/signal share the same column list (built from the same
        [c0..c{n-1}] name array).
     3. rebalance index is non-empty (at least one bar selected).
     4-6. rebalance dates land in [1, n_bars-2] — in-range, matching
        a price-frame row, with T+1 available.
     7. signal cells are drawn from [float_range (-1.0) 1.0], so no
        rebalance row can carry a NaN.
     8. one (rebalance, instrument-0) cell is forced to ±1.0.
     9. prices are strictly-positive multiplicative random walks (see
        below), so every cell the loop reads is finite and > 0.0 —
        including under [scale_prices], whose factors are positive.

   Restrictions for the Layer 3 invariants to be well-defined:
     - target weights in [-1.0, 1.0] per instrument (leverage > 1.0 with
       adverse moves can drive NAV negative and
       [equity_curve_strictly_positive] becomes unsatisfiable).
     - rebalance bar indices in [1, n_bars-2]. The lower bound holds
       weights at zero on bar 0 so the [equity_curve_starts_at_one]
       invariant is structural. The upper bound
       satisfies precondition 6.
     - prices: strictly positive multiplicative random walks anchored
       at 1.0 with per-bar ratios in [0.97, 1.03]. Tight ratios keep
       the per-bar MTM factor [1 + sum w_j * (r_j - 1)] safely above
       zero for n_cols <= 3.
     - commission, slippage in [0.0, 0.001]. The cost formula is
       [(c+s) * |dw| * nav] (pre-cost NAV); tight bounds
       keep cumulative cost small enough that NAV stays positive over
       the loop.
     - n_cols in [1, 3], n_bars in [4, 8]. Small enough that QCheck
       at ~count:200 finishes promptly. *)

type engine_inputs = {
  price_frame : [ `Daily ] Cairos.Frame.t;
  signal_frame : [ `Daily ] Cairos.Frame.t;
  rebalance_index : [ `Daily ] Cairos.Index.t;
  rebalance_bars : int array;
  signal_targets : float array array;
  n_bars : int;
  n_cols : int;
  commission : float;
  slippage : float;
}

let make_daily_index n =
  (* Synthetic daily index of [n] bars starting at the shared epoch. *)
  let ts =
    Array.init n (fun i ->
        Qcheck_gen.epoch_2024_01_01_utc +. (float_of_int i *. 86_400.0))
  in
  match Cairos.Index.of_unix_floats Cairos.Freq.Day ts with
  | Ok idx -> idx
  | Error e ->
      (* Unreachable: synthetic strictly-increasing finite POSIX seconds. *)
      failwith
        ("test_invariants.make_daily_index: " ^ Cairos.Index.err_to_string e)

let frame_from_columns ~idx ~names ~values =
  (* values.(j) is column j's float array; names.(j) is its label. *)
  let n_cols = Array.length names in
  let pairs =
    List.init n_cols (fun j ->
        let n = Array.length values.(j) in
        let nx = Nx.create Nx.float64 [| n |] values.(j) in
        let s = Cairos.Series.make_unsafe idx nx in
        (names.(j), s))
  in
  match pairs with
  | [] ->
      failwith "test_invariants.frame_from_columns: empty columns (unreachable)"
  | first :: rest -> (
      let ne = Cairos.Nonempty.make first rest in
      match Cairos.Frame.of_series ne with
      | Ok f -> f
      | Error e -> failwith ("test_invariants.frame_from_columns: " ^ e))

let engine_inputs_gen =
  let open QCheck.Gen in
  let* n_cols = int_range 1 3 in
  let* n_bars = int_range 4 8 in
  let* commission = float_range 0.0 0.001 in
  let* slippage = float_range 0.0 0.001 in
  let* ratios_flat =
    array_size (return (n_cols * (n_bars - 1))) (float_range 0.97 1.03)
  in
  let prices_arr =
    Array.init n_cols (fun j ->
        let p = Array.make n_bars 0.0 in
        p.(0) <- 1.0;
        for t = 1 to n_bars - 1 do
          p.(t) <- p.(t - 1) *. ratios_flat.((j * (n_bars - 1)) + t - 1)
        done;
        p)
  in
  let* select_flags =
    array_size
      (return (n_bars - 2))
      (oneof_weighted [ (1, return true); (2, return false) ])
  in
  let chosen_indices =
    let acc = ref [] in
    Array.iteri (fun i flag -> if flag then acc := (i + 1) :: !acc) select_flags;
    List.rev !acc
  in
  let chosen_indices =
    match chosen_indices with
    | [] -> [ 1 ]
    | xs -> xs
  in
  let rebalance_bars = Array.of_list chosen_indices in
  let n_rebal = Array.length rebalance_bars in
  let* targets_flat =
    array_size (return (n_rebal * n_cols)) (float_range (-1.0) 1.0)
  in
  let* sign_choice = oneof_list [ -1.0; 1.0 ] in
  let targets =
    Array.init n_rebal (fun k ->
        Array.init n_cols (fun j -> targets_flat.((k * n_cols) + j)))
  in
  targets.(0).(0) <- sign_choice;
  let signals_arr =
    Array.init n_cols (fun j ->
        let s = Array.make n_bars 0.0 in
        Array.iteri (fun k bar -> s.(bar) <- targets.(k).(j)) rebalance_bars;
        s)
  in
  let names = Array.init n_cols (fun j -> Printf.sprintf "c%d" j) in
  let idx = make_daily_index n_bars in
  let price_frame = frame_from_columns ~idx ~names ~values:prices_arr in
  let signal_frame = frame_from_columns ~idx ~names ~values:signals_arr in
  let rebal_ts =
    Array.map
      (fun bar ->
        Qcheck_gen.epoch_2024_01_01_utc +. (float_of_int bar *. 86_400.0))
      rebalance_bars
  in
  let rebalance_index =
    match Cairos.Index.of_unix_floats Cairos.Freq.Day rebal_ts with
    | Ok i -> i
    | Error e ->
        failwith
          ("test_invariants.engine_inputs_gen: " ^ Cairos.Index.err_to_string e)
  in
  return
    {
      price_frame;
      signal_frame;
      rebalance_index;
      rebalance_bars;
      signal_targets = signals_arr;
      n_bars;
      n_cols;
      commission;
      slippage;
    }

let engine_inputs_print ei =
  Printf.sprintf "<engine inputs n_bars=%d n_cols=%d n_rebal=%d c=%g s=%g>"
    ei.n_bars ei.n_cols
    (Array.length ei.rebalance_bars)
    ei.commission ei.slippage

let engine_inputs_arb = QCheck.make ~print:engine_inputs_print engine_inputs_gen

(* === Helpers === *)

let run_with ~commission ~slippage ei =
  Cairos_engine.Backtest.run ~price_frame:ei.price_frame
    ~signal_frame:ei.signal_frame ~rebalance_index:ei.rebalance_index
    ~commission ~slippage

let run ei = run_with ~commission:ei.commission ~slippage:ei.slippage ei

(* Every property here runs on generated inputs that satisfy the entrypoint
   preconditions by construction, so an [Error] is a generator bug, not a
   counterexample — all of them report it the same way. *)
let fail_run e =
  QCheck.Test.fail_reportf "Backtest.run errored: %s"
    (Cairos_engine.Backtest.err_to_string e)

let column_values name frame =
  match Cairos.Frame.get name frame with
  | Some s -> Nx.to_array (Cairos.Series.values s)
  | None ->
      failwith (Printf.sprintf "test_invariants.column_values: %s missing" name)

(* Bar-indexed mask of the rebalance dates, [true] at each rebalance bar. *)
let rebal_mask ei =
  let is_rebal = Array.make ei.n_bars false in
  Array.iter (fun bar -> is_rebal.(bar) <- true) ei.rebalance_bars;
  is_rebal

(* Derive an [engine_inputs] whose price columns are all multiplied by [k],
   carrying every other field through unchanged. The generator does not retain
   the raw price array, so the column names and values are read back out of
   [price_frame] and the frame rebuilt; the index is regenerated by
   [make_daily_index], which is deterministic in [n_bars] and therefore
   reproduces the generator's index. *)
let scale_prices ~k ei =
  let names = Array.of_list (Cairos.Frame.columns ei.price_frame) in
  let values =
    Array.map
      (fun name ->
        Array.map (fun p -> p *. k) (column_values name ei.price_frame))
      names
  in
  let idx = make_daily_index ei.n_bars in
  { ei with price_frame = frame_from_columns ~idx ~names ~values }

(* Frictionless NAV recursion — an *independent* oracle for the zero-cost case.

   Provenance: authored from the execution contract as stated below, never
   from [lib/cairos_engine/cairos_engine.ml]. An oracle transliterated from
   the implementation cannot catch a modeling error. The contract clauses this
   encodes, in full so that the oracle is self-contained:

   - Initial state: all weights [0.0], NAV [1.0].
   - NAV-update ordering at a rebalance bar: mark to market through to the
     rebalance date's price-frame row, deduct the cost from that NAV, then
     apply the target weights to the reduced NAV. At
     [commission = slippage = 0.0] the deduction is identically zero, so this
     oracle carries no cost term and the cost notional never enters.
   - One equity-curve row per price-frame bar; the row at a rebalance bar is
     the post-cost, pre-*next*-bar-MTM NAV — that bar's own MTM has already
     been applied.
   - The MTM factor is the weighted-return form
     [1 +. sum_j w_j *. (p_t_j /. p_{t-1}_j -. 1.0)], not a literal product of
     price ratios (the product form yields a negative NAV on shorts and is
     undefined when the weights do not sum to 1.0).
   - The end-of-backtest force-close is cost-free, so it cannot move NAV and
     needs no term here.

   [equity.(0)] is the initial NAV verbatim and the loop runs from bar 1, which
   is sound only because the generator confines rebalance bars to
   [1, n_bars-2]. That assumption is enforced below rather than assumed: a
   rebalance at bar 0 would leave the weights unset and make the oracle
   silently wrong, reporting a contract violation that is not one.

   Columns are indexed by position, matching [signal_targets]'s own ordering. *)
let frictionless_nav ei =
  let names = Array.of_list (Cairos.Frame.columns ei.price_frame) in
  let prices =
    Array.map (fun name -> column_values name ei.price_frame) names
  in
  let is_rebal = rebal_mask ei in
  if is_rebal.(0) then
    failwith
      "test_invariants.frictionless_nav: rebalance at bar 0 — the generator's \
       [1, n_bars-2] confinement no longer holds, so the oracle's bar-0 \
       seeding must be revisited";
  let weights = Array.make ei.n_cols 0.0 in
  let nav = ref 1.0 in
  let equity = Array.make ei.n_bars 1.0 in
  equity.(0) <- !nav;
  for t = 1 to ei.n_bars - 1 do
    let factor = ref 1.0 in
    for j = 0 to ei.n_cols - 1 do
      factor :=
        !factor
        +. (weights.(j) *. ((prices.(j).(t) /. prices.(j).(t - 1)) -. 1.0))
    done;
    nav := !nav *. !factor;
    if is_rebal.(t) then
      for j = 0 to ei.n_cols - 1 do
        weights.(j) <- ei.signal_targets.(j).(t)
      done;
    equity.(t) <- !nav
  done;
  equity

(* Predict trade count from inputs by replaying the inception/resolution
   counter per instrument across rebalance bars.

   Inceptions across the loop become trades 1:1 — each inception is
   resolved either by a later rebalance (full close or sign flip) or by
   end-of-backtest force-close. *)
let expected_trade_count ei =
  let total = ref 0 in
  for j = 0 to ei.n_cols - 1 do
    let w_held = ref 0.0 in
    Array.iter
      (fun bar ->
        let target = ei.signal_targets.(j).(bar) in
        let old_w = !w_held in
        if Float.equal old_w 0.0 && not (Float.equal target 0.0) then incr total
        else if (not (Float.equal old_w 0.0)) && not (Float.equal target 0.0)
        then begin
          let same_dir =
            (old_w > 0.0 && target > 0.0) || (old_w < 0.0 && target < 0.0)
          in
          if not same_dir then incr total
        end;
        w_held := target)
      ei.rebalance_bars
  done;
  !total

(* === Properties === *)

(* The first equity-curve cell is exactly 1.0. The generator excludes
   bar 0 from rebalance bar indices, so the engine performs no
   cost-deduction or weight change at t=0; [equity_buf.(0)] is
   [current_nav = 1.0] verbatim. Catches a regression that mis-orders
   the loop body and writes anything other than the initial NAV at the
   first cell. *)
let equity_curve_starts_at_one =
  QCheck.Test.make ~count:200 ~name:"equity_curve_starts_at_one"
    engine_inputs_arb (fun ei ->
      match run ei with
      | Error e -> fail_run e
      | Ok result ->
          let arr = Nx.to_array (Cairos.Series.values result.equity_curve) in
          Float.equal arr.(0) 1.0)

(* Every cell of [equity_curve] is strictly positive. With per-instrument
   |w| <= 1.0 (generator restriction), per-bar price ratios in
   [0.97, 1.03] (generator restriction), and n_cols <= 3, the per-bar MTM
   factor [1 + sum_j w_j * (r_j - 1)] stays in [1 - 3*0.03, 1 + 3*0.03] =
   [0.91, 1.09]. Cumulative cost is bounded by tight (c+s) and price
   anchoring at 1.0. NAV cannot reach zero under these inputs. *)
let equity_curve_strictly_positive =
  QCheck.Test.make ~count:200 ~name:"equity_curve_strictly_positive"
    engine_inputs_arb (fun ei ->
      match run ei with
      | Error e -> fail_run e
      | Ok result ->
          let arr = Nx.to_array (Cairos.Series.values result.equity_curve) in
          Array.for_all (fun x -> x > 0.0) arr)

(* Transaction costs are non-negative. Under the turnover-notional
   convention the per-rebalance cost is
   [(commission +. slippage) *. |dw| *. nav], non-negative because all
   three factors are: [commission >= 0.0] and [slippage >= 0.0] by the
   generator, [|dw| >= 0.0] by construction, and [nav > 0.0] by the
   [equity_curve_strictly_positive] invariant above.

   [Trade.t] surfaces neither [|dw|] nor the pre-cost NAV at the trade's
   rebalance, so the exact cost cannot be reconstructed from a trade
   record here. We assert the weak proxy [(commission +. slippage) *.
   entry_price >= 0.0]: under A1 the cost dropped its price factor, so
   this no longer exercises the cost path — it only pins that costs are
   not trivially negative. The load-bearing guards are elsewhere: a cost
   sign error is caught by [trade_pnl_sum_plus_one_equals_final_equity]
   (pnl embeds [-. cost]), and price-scale invariance of the cost is
   pinned by [price_scale_invariance] below. *)
let transaction_costs_non_negative =
  QCheck.Test.make ~count:200 ~name:"transaction_costs_non_negative"
    engine_inputs_arb (fun ei ->
      match run ei with
      | Error e -> fail_run e
      | Ok result ->
          let cs = ei.commission +. ei.slippage in
          List.for_all
            (fun (t : Cairos_engine.Trade.t) ->
              let cost_lower_bound = cs *. t.entry_price in
              cost_lower_bound >= 0.0)
            (Cairos.Nonempty.to_list result.trades))

(* Trade count equals the count of round-trip resolutions implied by
   the input weight schedule: every inception (a
   transition from [w_held = 0] to [w_held != 0], or a sign-flip
   transition mid-holding) becomes one trade — resolved either by a
   later rebalance or by the end-of-backtest force-close.
   Same-direction size adjustments do not increment the count. *)
let trade_count_matches_round_trip_resolutions =
  QCheck.Test.make ~count:200 ~name:"trade_count_matches_round_trip_resolutions"
    engine_inputs_arb (fun ei ->
      match run ei with
      | Error e -> fail_run e
      | Ok result ->
          let expected = expected_trade_count ei in
          let actual = Cairos.Nonempty.length result.trades in
          expected = actual)

(* [Cairos_finance.max_drawdown s] equals [Float.abs (min (drawdown_series
   s))] per the [cairos_finance] internal cross-check (max_drawdown is
   defined as the negation of the minimum of the drawdown series).
   Both sides are applied to
   [result.equity_curve]. The cross-check catches engine bugs that
   produce equity curves violating the metrics' assumptions (e.g. an
   accidental NaN cell that makes the two implementations diverge). *)
let max_drawdown_equals_min_drawdown_series =
  QCheck.Test.make ~count:200 ~name:"max_drawdown_equals_min_drawdown_series"
    engine_inputs_arb (fun ei ->
      match run ei with
      | Error e -> fail_run e
      | Ok result ->
          let mdd = Cairos_finance.max_drawdown result.equity_curve in
          let dd_series = Cairos_finance.drawdown_series result.equity_curve in
          let dd_arr = Nx.to_array (Cairos.Series.values dd_series) in
          let min_dd =
            Array.fold_left
              (fun acc x -> if x < acc then x else acc)
              Float.infinity dd_arr
          in
          Qcheck_gen.float_approx_equal ~tol:1e-12 mdd (Float.abs min_dd))

(* For every non-rebalance bar [i > 0] and every column [j],
   [weights.(i).(j) = weights.(i-1).(j)] exactly ([Float.equal]).
   Catches a regression where MTM accidentally writes into the weights
   row instead of the equity-curve row, or where the loop body forgets
   to carry [current_weights] through non-rebalance bars. *)
let weights_constant_between_rebalances =
  QCheck.Test.make ~count:200 ~name:"weights_constant_between_rebalances"
    engine_inputs_arb (fun ei ->
      match run ei with
      | Error e -> fail_run e
      | Ok result ->
          let is_rebal = rebal_mask ei in
          let names = Cairos.Frame.columns result.weights in
          List.for_all
            (fun name ->
              let w = column_values name result.weights in
              let ok = ref true in
              for i = 1 to ei.n_bars - 1 do
                if (not is_rebal.(i)) && not (Float.equal w.(i) w.(i - 1)) then
                  ok := false
              done;
              !ok)
            names)

(* [1.0 +. sum(t.pnl) ≈ equity_curve.(N-1)] within tolerance
   [n_trades *. 1e-12]. This is the
   identity that [Trade.t.exit_timestamp]'s
   doc-comment promises. Catches: misordered cost deduction (deducted twice or
   zero times); wrong cost notional; wrong segment splitting in the
   round-trip accumulator; force-closing at the wrong price. *)
let trade_pnl_sum_plus_one_equals_final_equity =
  QCheck.Test.make ~count:200 ~name:"trade_pnl_sum_plus_one_equals_final_equity"
    engine_inputs_arb (fun ei ->
      match run ei with
      | Error e -> fail_run e
      | Ok result ->
          let trades = Cairos.Nonempty.to_list result.trades in
          let n_trades = List.length trades in
          let pnl_sum =
            List.fold_left
              (fun acc (t : Cairos_engine.Trade.t) -> acc +. t.pnl)
              0.0 trades
          in
          let equity_arr =
            Nx.to_array (Cairos.Series.values result.equity_curve)
          in
          let final_equity = equity_arr.(ei.n_bars - 1) in
          let tol = Float.max 1e-10 (float_of_int n_trades *. 1e-12) in
          Qcheck_gen.float_approx_equal ~tol (1.0 +. pnl_sum) final_equity)

(* [Backtest.run] is invariant under uniform price scaling: multiplying every
   price by a positive constant [k], with signals and costs held fixed, leaves
   every equity-curve bar and every trade's realised P&L unchanged. NAV starts
   at 1.0 and the per-bar MTM factor depends only on price *ratios*, which [k]
   cancels out of; the cost notional is NAV-relative, so it is price-independent
   too. [entry_price] / [exit_price] are excluded from the comparison — they
   scale with [k] by design.

   This guards a defect class that shipped once and was fixed: a cost notional
   that multiplies by an absolute price rather than NAV makes cost — and
   therefore equity and pnl — move with [k].

   Two cost settings are exercised per case: the generator's drawn pair, and a
   fixed [(0.005, 0.005)]. The drawn commission may land arbitrarily close to
   zero, where the cost term all but vanishes and the property degenerates into
   a test of mark-to-market scale-invariance alone; the fixed pair guarantees
   every case exercises a cost term that actually moves NAV. As in
   [commission_monotonic_in_final_nav], [0.005] exceeds the generator's
   [0, 0.001] cap without endangering NAV positivity — turnover is bounded.

   Every mismatch reports the scale factor, the cost setting, and the offending
   bar or trade: a bare boolean would name only the drawn input, leaving a
   regression to be re-derived by hand. Length checks structurally enclose the
   element-wise comparisons rather than merely preceding them in a conjunction,
   so no reordering can reintroduce an [Invalid_argument] from a [for_all2]. *)
let price_scale_invariance =
  QCheck.Test.make ~count:200 ~name:"price_scale_invariance" engine_inputs_arb
    (fun ei ->
      let trades_of (r : _ Cairos_engine.Backtest.result) =
        Array.of_list (Cairos.Nonempty.to_list r.trades)
      in
      let equity_of (r : _ Cairos_engine.Backtest.result) =
        Nx.to_array (Cairos.Series.values r.equity_curve)
      in
      let check_at ~commission ~slippage =
        match run_with ~commission ~slippage ei with
        | Error msg ->
            QCheck.Test.fail_reportf
              "Backtest.run errored on unscaled inputs (commission=%g, \
               slippage=%g): %s"
              commission slippage
              (Cairos_engine.Backtest.err_to_string msg)
        | Ok base ->
            let base_equity = equity_of base in
            let base_trades = trades_of base in
            List.for_all
              (fun k ->
                match run_with ~commission ~slippage (scale_prices ~k ei) with
                | Error msg ->
                    QCheck.Test.fail_reportf
                      "Backtest.run errored at k=%g (commission=%g, \
                       slippage=%g): %s"
                      k commission slippage
                      (Cairos_engine.Backtest.err_to_string msg)
                | Ok scaled ->
                    let scaled_equity = equity_of scaled in
                    let scaled_trades = trades_of scaled in
                    if Array.length base_equity <> Array.length scaled_equity
                    then
                      QCheck.Test.fail_reportf
                        "k=%g commission=%g: equity length %d <> %d" k
                        commission (Array.length base_equity)
                        (Array.length scaled_equity)
                    else if
                      Array.length base_trades <> Array.length scaled_trades
                    then
                      QCheck.Test.fail_reportf
                        "k=%g commission=%g: trade count %d <> %d" k commission
                        (Array.length base_trades)
                        (Array.length scaled_trades)
                    else begin
                      Array.iteri
                        (fun i b ->
                          let s = scaled_equity.(i) in
                          if not (Qcheck_gen.float_approx_equal ~tol:1e-10 b s)
                          then
                            QCheck.Test.fail_reportf
                              "k=%g commission=%g: equity bar %d moved with \
                               price scale: %.17g -> %.17g"
                              k commission i b s)
                        base_equity;
                      Array.iteri
                        (fun i (b : Cairos_engine.Trade.t) ->
                          let s = scaled_trades.(i) in
                          if
                            not
                              (Qcheck_gen.float_approx_equal ~tol:1e-10 b.pnl
                                 s.pnl)
                          then
                            QCheck.Test.fail_reportf
                              "k=%g commission=%g: trade %d pnl moved with \
                               price scale: %.17g -> %.17g"
                              k commission i b.pnl s.pnl;
                          if not (String.equal b.instrument s.instrument) then
                            QCheck.Test.fail_reportf
                              "k=%g commission=%g: trade %d instrument %s <> %s"
                              k commission i b.instrument s.instrument;
                          if b.holding_period_bars <> s.holding_period_bars then
                            QCheck.Test.fail_reportf
                              "k=%g commission=%g: trade %d holding period %d \
                               <> %d"
                              k commission i b.holding_period_bars
                              s.holding_period_bars)
                        base_trades;
                      true
                    end)
              [ 0.01; 100.0 ]
      in
      List.for_all
        (fun (commission, slippage) -> check_at ~commission ~slippage)
        [ (ei.commission, ei.slippage); (0.005, 0.005) ])

(* Raising commission never raises final NAV: for fixed inputs and a fixed
   slippage, the final equity bar at [commission = 0.005] is <= the bar at
   [commission = 0.0005]. This pins the *direction* of the cost model — a sign
   or magnitude regression that leaves cost price-independent (and so passes
   [price_scale_invariance]) still moves NAV the wrong way here.

   The comparison is a *strict* [<], not [<=] and not a tolerance comparison.
   Strictness matters: under [<=] a regression that deletes the cost deduction
   entirely yields [high = low] and passes, leaving the property vacuous.
   Strictness is sound because the generator pins [targets.(0).(0)] to [±1.0]
   and weights start at [0.0], so every case turns over at least [|dw| = 1.0]
   at its first rebalance; the two runs' NAVs therefore differ by at least
   [(0.005 -. 0.0005) *. 1.0 *. nav], nine orders of magnitude above float
   resolution. Exactness holds because the two runs share an identical weight
   path (targets come from the signal frame, and between-rebalance drift is
   NAV-scale-invariant), so cost only ever reduces NAV multiplicatively.

   [0.005] exceeds the generator's documented [0.0, 0.001] commission cap, but
   the property stays well-defined: turnover is bounded ([n_cols <= 3], weights
   in [-1, 1], rebalance bars in [1, n_bars-2] with [n_bars <= 8]), so the
   cumulative cost factor cannot drive pre-cost NAV to zero.

   Both arguments above are generator-coupled, so the caps they rest on are
   asserted rather than assumed. Widening the generator flips [cost = cs *.
   |dw| *. nav] negative once [nav] can go negative, inverting monotonicity —
   a failure that would otherwise surface as a baffling counterexample instead
   of a pointer at the assumption that broke. *)
let commission_monotonic_in_final_nav =
  QCheck.Test.make ~count:200 ~name:"commission_monotonic_in_final_nav"
    engine_inputs_arb (fun ei ->
      if ei.n_cols > 3 || ei.n_bars > 8 then
        failwith
          "test_invariants.commission_monotonic_in_final_nav: generator caps \
           widened past [n_cols <= 3, n_bars <= 8] — re-derive the NAV \
           positivity bound before trusting this property";
      if
        not
          (Float.equal
             (Float.abs ei.signal_targets.(0).(ei.rebalance_bars.(0)))
             1.0)
      then
        failwith
          "test_invariants.commission_monotonic_in_final_nav: generator no \
           longer pins the first rebalance's leading target to ±1.0 — turnover \
           may vanish, so the strict inequality below is no longer sound";
      let final_equity_at ~commission =
        match run_with ~commission ~slippage:ei.slippage ei with
        | Error msg ->
            QCheck.Test.fail_reportf "Backtest.run errored at commission=%g: %s"
              commission
              (Cairos_engine.Backtest.err_to_string msg)
        | Ok result ->
            let arr = Nx.to_array (Cairos.Series.values result.equity_curve) in
            arr.(ei.n_bars - 1)
      in
      let low = final_equity_at ~commission:0.0005 in
      let high = final_equity_at ~commission:0.005 in
      if high < low then true
      else
        QCheck.Test.fail_reportf
          "final NAV is not strictly decreasing in commission: %.17g at \
           commission=0.005 is not < %.17g at commission=0.0005"
          high low)

(* With zero commission and zero slippage the engine's equity curve equals the
   contract-derived frictionless recursion above, bar for bar. This anchors the
   engine's cost-free behaviour to the pinned execution/MTM contract instead of
   trusting the engine to check itself: [price_scale_invariance] and
   [commission_monotonic_in_final_nav] both compare the engine against itself,
   so a modeling error in the MTM step or the weight-timing convention is
   invisible to them.

   The oracle is independent by construction — see [frictionless_nav]'s
   provenance note. *)
let zero_cost_equals_frictionless_recursion =
  QCheck.Test.make ~count:200 ~name:"zero_cost_equals_frictionless_recursion"
    engine_inputs_arb (fun ei ->
      match run_with ~commission:0.0 ~slippage:0.0 ei with
      | Error e -> fail_run e
      | Ok result ->
          let actual = Nx.to_array (Cairos.Series.values result.equity_curve) in
          let expected = frictionless_nav ei in
          let tol = Float.max 1e-10 (float_of_int ei.n_bars *. 1e-12) in
          Array.length actual = Array.length expected
          && Array.for_all2 (Qcheck_gen.float_approx_equal ~tol) actual expected)

let () =
  Qcheck_gen.pin_seed_from_env ();
  let tests =
    List.map QCheck_alcotest.to_alcotest
      [
        equity_curve_starts_at_one;
        equity_curve_strictly_positive;
        transaction_costs_non_negative;
        trade_count_matches_round_trip_resolutions;
        max_drawdown_equals_min_drawdown_series;
        weights_constant_between_rebalances;
        trade_pnl_sum_plus_one_equals_final_equity;
        price_scale_invariance;
        commission_monotonic_in_final_nav;
        zero_cost_equals_frictionless_recursion;
      ]
  in
  Alcotest.run "cairos_engine.invariants" [ ("Layer 3", tests) ]
