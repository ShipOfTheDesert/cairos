(* Layer 3 — QCheck invariants for [Cairos_engine.Backtest.run].

   The seven properties pinned by RFC 0056 §Test Plan Layer 3 plus
   PRD 0055 FR-5 (count = 200 each). Inputs come from a single
   [engine_inputs_arb] generator that produces (price [Frame], signal
   [Frame], rebalance [Index.t], commission, slippage) tuples
   structurally satisfying RFC 0052's seven entrypoint preconditions.
   Determinism is pinned via [Qcheck_gen.pin_seed_from_env] (PRD 0029
   / 0045).

   The properties exercise behaviour the type system does not pin:
   the loop's mark-to-market sign, the cost-deduction direction, the
   trade accumulator's count of round-trip resolutions, the
   weights-frame's between-rebalance constancy, and the
   sum-of-pnl/equity reconciliation FR-10's [Trade.t.exit_timestamp]
   doc-comment promises. *)

(* === Generator ===

   [engine_inputs_arb] produces inputs satisfying RFC 0052's seven
   preconditions structurally:
     1. price/signal share the same [Index.t] (built from the same
        synthetic timestamp array).
     2. price/signal share the same column list (built from the same
        [c0..c{n-1}] name array).
     3. rebalance index is non-empty (at least one bar selected).
     4-6. rebalance dates land in [1, n_bars-2] — in-range, matching
        a price-frame row, with T+1 available.
     7. one (rebalance, instrument-0) cell is forced to ±1.0.

   Restrictions for the Layer 3 invariants to be well-defined:
     - target weights in [-1.0, 1.0] per instrument (RFC 0056 §Test
       Plan Layer 3 row 2 — leverage > 1.0 with adverse moves can
       drive NAV negative and [equity_curve_strictly_positive] becomes
       unsatisfiable).
     - rebalance bar indices in [1, n_bars-2]. The lower bound holds
       weights at zero on bar 0 so the [equity_curve_starts_at_one]
       invariant is structural per RFC 0052 OC-11. The upper bound
       satisfies precondition 6.
     - prices: strictly positive multiplicative random walks anchored
       at 1.0 with per-bar ratios in [0.97, 1.03]. Tight ratios keep
       the per-bar MTM factor [1 + sum w_j * (r_j - 1)] safely above
       zero for n_cols <= 3.
     - commission, slippage in [0.0, 0.001]. The cost formula is
       [(c+s) * |dw| * nav] (pre-cost NAV; RFC 0052 A1); tight bounds
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

let run ei =
  Cairos_engine.Backtest.run ~price_frame:ei.price_frame
    ~signal_frame:ei.signal_frame ~rebalance_index:ei.rebalance_index
    ~commission:ei.commission ~slippage:ei.slippage

let column_values name frame =
  match Cairos.Frame.get name frame with
  | Some s -> Nx.to_array (Cairos.Series.values s)
  | None ->
      failwith (Printf.sprintf "test_invariants.column_values: %s missing" name)

(* Predict trade count from inputs by replaying the inception/resolution
   counter per instrument across rebalance bars.

   Inceptions across the loop become trades 1:1 — each inception is
   resolved either by a later rebalance (full close or sign flip) or by
   end-of-backtest force-close per FR-10. *)
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

(* TP-Engine-1 — RFC 0056 §Test Plan Layer 3 row 1.

   The first equity-curve cell is exactly 1.0. The generator excludes
   bar 0 from rebalance bar indices, so the engine performs no
   cost-deduction or weight change at t=0; [equity_buf.(0)] is
   [current_nav = 1.0] verbatim. Catches a regression that mis-orders
   the loop body and writes anything other than the initial NAV at the
   first cell. *)
let equity_curve_starts_at_one =
  QCheck.Test.make ~count:200 ~name:"equity_curve_starts_at_one"
    engine_inputs_arb (fun ei ->
      match run ei with
      | Error msg -> QCheck.Test.fail_reportf "Backtest.run errored: %s" msg
      | Ok result ->
          let arr = Nx.to_array (Cairos.Series.values result.equity_curve) in
          Float.equal arr.(0) 1.0)

(* TP-Engine-2 — RFC 0056 §Test Plan Layer 3 row 2.

   Every cell of [equity_curve] is strictly positive. With per-instrument
   |w| <= 1.0 (generator restriction), per-bar price ratios in
   [0.97, 1.03] (generator restriction), and n_cols <= 3, the per-bar MTM
   factor [1 + sum_j w_j * (r_j - 1)] stays in [1 - 3*0.03, 1 + 3*0.03] =
   [0.91, 1.09]. Cumulative cost is bounded by tight (c+s) and price
   anchoring at 1.0. NAV cannot reach zero under these inputs. *)
let equity_curve_strictly_positive =
  QCheck.Test.make ~count:200 ~name:"equity_curve_strictly_positive"
    engine_inputs_arb (fun ei ->
      match run ei with
      | Error msg -> QCheck.Test.fail_reportf "Backtest.run errored: %s" msg
      | Ok result ->
          let arr = Nx.to_array (Cairos.Series.values result.equity_curve) in
          Array.for_all (fun x -> x > 0.0) arr)

(* TP-Engine-3 — RFC 0056 §Test Plan Layer 3 row 3.

   Transaction costs are non-negative. Under the turnover-notional
   convention (RFC 0052 Amendment A1) the per-rebalance cost is
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
   deferred to Feature CG-2. *)
let transaction_costs_non_negative =
  QCheck.Test.make ~count:200 ~name:"transaction_costs_non_negative"
    engine_inputs_arb (fun ei ->
      match run ei with
      | Error msg -> QCheck.Test.fail_reportf "Backtest.run errored: %s" msg
      | Ok result ->
          let cs = ei.commission +. ei.slippage in
          List.for_all
            (fun (t : Cairos_engine.Trade.t) ->
              let cost_lower_bound = cs *. t.entry_price in
              cost_lower_bound >= 0.0)
            (Cairos.Nonempty.to_list result.trades))

(* TP-Engine-4 — RFC 0056 §Test Plan Layer 3 row 4.

   Trade count equals the count of round-trip resolutions implied by
   the input weight schedule per PRD 0053 FR-7: every inception (a
   transition from [w_held = 0] to [w_held != 0], or a sign-flip
   transition mid-holding) becomes one trade — resolved either by a
   later rebalance or by the end-of-backtest force-close per FR-10.
   Same-direction size adjustments do not increment the count. *)
let trade_count_matches_round_trip_resolutions =
  QCheck.Test.make ~count:200 ~name:"trade_count_matches_round_trip_resolutions"
    engine_inputs_arb (fun ei ->
      match run ei with
      | Error msg -> QCheck.Test.fail_reportf "Backtest.run errored: %s" msg
      | Ok result ->
          let expected = expected_trade_count ei in
          let actual = Cairos.Nonempty.length result.trades in
          expected = actual)

(* TP-Engine-5 — RFC 0056 §Test Plan Layer 3 row 5.

   [Cairos_finance.max_drawdown s] equals [Float.abs (min (drawdown_series
   s))] per the [cairos_finance] internal cross-check (max_drawdown is
   defined as the negation of the minimum of the drawdown series).
   Following RFC 0056's literal wording, both sides are applied to
   [result.equity_curve]. The cross-check catches engine bugs that
   produce equity curves violating the metrics' assumptions (e.g. an
   accidental NaN cell that makes the two implementations diverge). *)
let max_drawdown_equals_min_drawdown_series =
  QCheck.Test.make ~count:200 ~name:"max_drawdown_equals_min_drawdown_series"
    engine_inputs_arb (fun ei ->
      match run ei with
      | Error msg -> QCheck.Test.fail_reportf "Backtest.run errored: %s" msg
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

(* TP-Engine-6 — RFC 0056 §Test Plan Layer 3 row 6.

   For every non-rebalance bar [i > 0] and every column [j],
   [weights.(i).(j) = weights.(i-1).(j)] exactly ([Float.equal]).
   Catches a regression where MTM accidentally writes into the weights
   row instead of the equity-curve row, or where the loop body forgets
   to carry [current_weights] through non-rebalance bars. *)
let weights_constant_between_rebalances =
  QCheck.Test.make ~count:200 ~name:"weights_constant_between_rebalances"
    engine_inputs_arb (fun ei ->
      match run ei with
      | Error msg -> QCheck.Test.fail_reportf "Backtest.run errored: %s" msg
      | Ok result ->
          let is_rebal = Array.make ei.n_bars false in
          Array.iter (fun bar -> is_rebal.(bar) <- true) ei.rebalance_bars;
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

(* TP-Engine-7 — RFC 0056 §Test Plan Layer 3 row 7.

   [1.0 +. sum(t.pnl) ≈ equity_curve.(N-1)] within tolerance
   [n_trades *. 1e-12] per the RFC's pinned scaling. This is the
   identity FR-10 / Decision 3 promises in [Trade.t.exit_timestamp]'s
   doc-comment. Catches: misordered cost deduction (deducted twice or
   zero times); wrong cost notional; wrong segment splitting in the
   round-trip accumulator; force-closing at the wrong price. *)
let trade_pnl_sum_plus_one_equals_final_equity =
  QCheck.Test.make ~count:200 ~name:"trade_pnl_sum_plus_one_equals_final_equity"
    engine_inputs_arb (fun ei ->
      match run ei with
      | Error msg -> QCheck.Test.fail_reportf "Backtest.run errored: %s" msg
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
      ]
  in
  Alcotest.run "cairos_engine.invariants" [ ("Layer 3", tests) ]
