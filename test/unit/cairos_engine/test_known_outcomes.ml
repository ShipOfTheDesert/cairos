(* Layer 1 — known-outcome strategy tests for [Cairos_engine.Backtest.run].

   Each test constructs price / signal / rebalance inputs for which the loop's
   output can be derived analytically from RFC 0052 OC-3 / OC-6 first
   principles plus PRD 0053 FR-7 / FR-9 / FR-10 trade accounting. Tolerance
   is absolute [1e-10] per RFC 0056 §Test Plan Layer 1.

   At RFC 0056 Phase 2 step 6 these tests are RED (Task 1's stub returns
   [Error "not implemented"]); RFC 0056 Phase 2 step 7 makes them GREEN. *)

(* === Test helpers ===

   These mirror [test/unit/cairos_finance/finance_test_helpers.ml] and
   [test/unit/cairos/test_helpers.ml]. We do not reach into [test_helpers]
   from this directory — RFC 0056 §Module Breakdown does not declare a
   helper library for [test/unit/cairos_engine/], and the helpers below are
   small enough that one-time inlining is cheaper than carrying a library
   dependency in the cycle-check gate of Task 4. *)

let make_daily_index dates =
  match Cairos.Index.daily dates with
  | Ok idx -> idx
  | Error e -> Alcotest.fail (Cairos.Index.err_to_string e)

let make_daily_series dates values =
  let idx = make_daily_index dates in
  let nx = Nx.create Nx.float64 [| Array.length values |] values in
  match Cairos.Series.make idx nx with
  | Ok s -> s
  | Error msg -> Alcotest.fail msg

let make_frame named_series =
  match named_series with
  | [] -> Alcotest.fail "make_frame: at least one column required"
  | (name, s) :: rest -> (
      let nonempty = Cairos.Nonempty.make (name, s) rest in
      match Cairos.Frame.of_series nonempty with
      | Ok f -> f
      | Error msg -> Alcotest.fail msg)

let frame_get_values name frame =
  match Cairos.Frame.get name frame with
  | Some s -> Nx.to_array (Cairos.Series.values s)
  | None -> Alcotest.fail (Printf.sprintf "missing column '%s'" name)

let ptime_of_date s =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> t
  | Error _ -> Alcotest.fail (Printf.sprintf "ptime_of_date: %s" s)

let ptime_testable = Alcotest.testable (Ptime.pp_rfc3339 ()) Ptime.equal

(* Float-array equality with absolute tolerance. *)
let check_float_array_close ~tol ~msg expected actual =
  Alcotest.(check int)
    (msg ^ " (length)") (Array.length expected) (Array.length actual);
  Array.iteri
    (fun i e ->
      let a = actual.(i) in
      Alcotest.(check (float tol)) (Printf.sprintf "%s [%d]" msg i) e a)
    expected

(* === Test 1: always_long_equity_curve_matches_compounded_return ===

   RFC 0056 §Test Plan Layer 1 row 1.

   Setup: one instrument "A", five daily bars, price grows exactly 1% per
   bar starting at [1.0] (so [price.(i) = 1.01 ** i]). Single rebalance at
   [t=1] with [target_weight = 1.0]. End-of-backtest force-close at the
   last bar.

   Why first rebalance at [t=1] (not [t=0]):
     RFC 0052 OC-6 pins the equity-curve value at a rebalance bar as
     [nav_after_costs] (post-cost, pre-MTM). Per OC-11 the first
     equity-curve cell is [1.0] only when the price-frame's first bar is
     not a rebalance — otherwise it is [1.0 -. cost], and the Layer 3
     [equity_curve_starts_at_one] invariant from RFC 0056 fails. We pin
     the first rebalance at [t=1] so [equity_curve.(0) = 1.0] holds
     structurally.

   Derivation per RFC 0052 OC-3 / OC-6:
     - Initial state (OC-11): [nav_0 = 1.0], all weights zero.
     - [t=0]: not a rebalance. Mark-to-market with zero weights leaves
       NAV unchanged. [equity.(0) = 1.0].
     - [t=1] (rebalance, T=1): execution at [T+1 = 2], so
       [execution_price = price.(2) = 1.0201] (this sets the trade's
       entry_price; it no longer enters the cost). MTM with zero weights
       leaves the pre-cost NAV at [nav_after_mtm = 1.0].
       [weight_delta = 1.0 - 0 = 1.0]. Cost is charged on turnover as a
       fraction of pre-cost NAV (feature 0058 / RFC 0052 Amendment A1),
       not on the price level:
       [cost = (commission +. slippage) *. |weight_delta| *. nav_after_mtm
             = 0.0015 *. 1.0 *. 1.0 = 0.0015].
       NAV-update ordering (OC-3, unchanged): deduct cost first, then
       apply new weights.
         [equity.(1) = nav_after_mtm -. cost = 1.0 -. 0.0015 = 0.9985]
         current_weights := 1.0
     - [t in {2,3,4}]: not rebalances. MTM with weight = 1.0:
         [equity.(i) = equity.(i-1) *. (price.(i) /. price.(i-1))]
         [        = equity.(i-1) *. 1.01].
       Hence [equity.(i) = (1.0 -. cost) *. 1.01 ** (i - 1)] for [i >= 1].

   Note (off-by-one vs RFC 0056 description): RFC 0056 §Test Plan Layer 1
   row 1 writes the formula as [equity.(i) = (1 - cost) * 1.01 ** i] for
   [i >= 1]. With [equity.(1) = 1 - cost] (no MTM applied at the rebalance
   bar itself) the correct exponent is [i - 1], not [i]. We follow the
   first-principles derivation above; the RFC description's exponent is a
   wording-side off-by-one.

   Trade record (force-close at [t=4]):
     - [entry_timestamp = price_index.(2)] (T+1 of T=1)
     - [exit_timestamp  = price_index.(4)] (last bar, force-close per FR-10)
     - [entry_price     = price.(2) = 1.0201]
     - [exit_price      = price.(4) = 1.04060401] (last bar's close per FR-10)
     - [holding_period_bars = 4 - 2 = 2]
     - [pnl] derived from PRD 0053 FR-10's equity-trade identity for the
       single-trade case: [pnl + 1.0 = equity.(N-1)], so
       [pnl = equity.(4) -. 1.0]. *)

let always_long_equity_curve_matches_compounded_return () =
  let dates =
    [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
  in
  let prices = Array.init 5 (fun i -> 1.01 ** float_of_int i) in
  let signals = [| 0.0; 1.0; 1.0; 1.0; 1.0 |] in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame = make_frame [ ("A", make_daily_series dates signals) ] in
  let rebalance_index = make_daily_index [| "2024-01-02" |] in
  let commission = 0.001 in
  let slippage = 0.0005 in
  (* Pre-cost NAV at the t=1 rebalance is [1.0]: zero weights through [t=0]
     make the MTM step a no-op. Cost is turnover as a fraction of that NAV
     ([(c+s) *. |dw| *. nav]), so [cost = 0.0015 *. 1.0 *. 1.0 = 0.0015],
     independent of [price.(2)]. *)
  let nav_at_rebalance = 1.0 in
  let cost = (commission +. slippage) *. 1.0 *. nav_at_rebalance in
  let expected_equity =
    Array.init 5 (fun i ->
        if i = 0 then 1.0 else (1.0 -. cost) *. (1.01 ** float_of_int (i - 1)))
  in
  match
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  with
  | Error msg -> Alcotest.fail msg
  | Ok result ->
      let equity_actual =
        Nx.to_array (Cairos.Series.values result.equity_curve)
      in
      check_float_array_close ~tol:1e-10 ~msg:"equity_curve" expected_equity
        equity_actual;
      Alcotest.(check int)
        "trade count" 1
        (Cairos.Nonempty.length result.trades);
      let trade = Cairos.Nonempty.hd result.trades in
      Alcotest.(check ptime_testable)
        "entry_timestamp"
        (ptime_of_date "2024-01-03T00:00:00Z")
        trade.entry_timestamp;
      Alcotest.(check ptime_testable)
        "exit_timestamp"
        (ptime_of_date "2024-01-05T00:00:00Z")
        trade.exit_timestamp;
      Alcotest.(check string) "instrument" "A" trade.instrument;
      Alcotest.(check (float 1e-10)) "entry_price" prices.(2) trade.entry_price;
      Alcotest.(check (float 1e-10)) "exit_price" prices.(4) trade.exit_price;
      Alcotest.(check int) "holding_period_bars" 2 trade.holding_period_bars;
      Alcotest.(check (float 1e-10))
        "pnl"
        (expected_equity.(4) -. 1.0)
        trade.pnl

(* === Test 2: alternating_long_short_pnl_matches_analytical ===

   RFC 0056 §Test Plan Layer 1 row 2.

   Setup: one instrument "A", five daily bars. Two rebalances forming a
   sign flip (long → short). End-of-backtest force-close at the last bar.

   Prices chosen so MTM ratios are clean: between rebalance bars and
   their executions, [price.(2)/price.(1) = 1.0] (no MTM drift), and
   the held-position MTM segment uses prices [1.0, 1.0, 1.0, 1.1, 1.0].

   Convention used for shorts:
     RFC 0052 OC-6 writes the MTM formula as
     [nav_t = nav_{t-1} *. sum_j (w_j *. p_t /. p_{t-1})]. Read literally
     this gives nonsensical (negative) NAV when [sum w_j < 0] (e.g.
     a single instrument with [w = -1]). The standard fractional-weight
     "weighted return" formula
     [nav_t = nav_{t-1} *. (1.0 +. sum_j (w_j *. (p_t /. p_{t-1} -. 1.0)))]
     is equivalent to OC-6's wording when [sum_j w_j = 1.0]
     (fully invested) and gives the realistic loss-on-price-up behaviour
     for shorts. This test assumes the engine implements the standard
     formula. If Task 3's loop body matches OC-6's literal wording
     (sum-of-weights = 1 only), this test will surface the divergence
     and the implementation choice is escalated per CLAUDE.md
     §What Must Never Happen.

   Sign-flip cost split (PRD 0053 FR-9):
     At a sign-flip rebalance with [w_old = +1.0, w_new = -1.0],
     [|weight_delta| = 2.0 = |w_old| + |w_new|]. The full per-rebalance
     cost [(c+s) *. 2.0 *. nav] (nav = pre-cost NAV at that bar) is paid
     once. PRD 0053 FR-9 attributes
     "all costs paid at i_0, ..., i_K and i_R" to the resolving trade.
     For the incepting trade at the same rebalance, the cost at i_0
     (its inception) is the same rebalance cost. PRD 0053 leaves the
     allocation between the two trades implicit. We split the cost
     proportionally to each side's contribution to [|weight_delta|]:
     half attributed to the closing trade, half to the opening trade
     (since [|w_old| = |w_new| = 1.0] here). If Task 3 uses a different
     allocation, the test surfaces the divergence and we escalate.

   Derivation (commission = 0.001, slippage = 0.0005, c = c+s = 0.0015):
     prices       = [1.0; 1.0; 1.0; 1.1; 1.0]
     rebalances   = [t=1; t=2]
     targets      = [+1.0 at t=1; -1.0 at t=2]

     - [t=0]: not rebalance. equity.(0) = 1.0.
     - [t=1]: rebalance T=1. nav_before_1 = 1.0 (zero weights, no MTM).
         cost1 = c * |dw| * nav_before_1 = 0.0015 * 1.0 * 1.0 = 0.0015.
         equity.(1) = 1.0 - 0.0015 = 0.9985. weights := +1.0.
         (execution_price = price.(2) = 1.0 sets the trade's entry_price;
         it no longer enters the cost — here nav and price coincide at 1.0.)
         Trade 1 inception: entry_timestamp = price_index.(2) = 2024-01-03,
         entry_price = price.(2) = 1.0.
     - [t=2]: rebalance T=2. weight_delta = -2.0.
         nav_before_2 = equity.(1) * (1 + 1.0 * (price.(2)/price.(1) - 1))
                      = 0.9985 * (1 + 0) = 0.9985.
         cost2 = c * |dw| * nav_before_2 = 0.0015 * 2.0 * 0.9985
               = 0.0029955.
         equity.(2) = 0.9985 - 0.0029955 = 0.9955045. weights := -1.0.
         (execution_price = price.(3) = 1.1 sets the trades' entry/exit
         prices; it no longer enters the cost.)
         Sign flip: trade 1 resolved at t=3 (T+1 of T=2),
         trade 2 incepted at t=3 (T+1 of T=2).
     - [t=3]: not rebalance. weight = -1.0. price.(3)/price.(2) = 1.1/1.0.
         equity.(3) = 0.9955045 * (1 + (-1.0) * (1.1 - 1.0))
                    = 0.9955045 * 0.9 = 0.89595405.
     - [t=4]: not rebalance. weight = -1.0. price.(4)/price.(3) = 1.0/1.1.
         equity.(4) = 0.89595405 * (1 + (-1.0) * (1.0/1.1 - 1.0))
                    = 0.89595405 * (2.0 - 1.0/1.1)
                    = 0.89595405 * 12.0/11.0.

   Trade records:
     Trade 1 (long):
       entry_timestamp = 2024-01-03 (T+1 of T=1).
       exit_timestamp  = 2024-01-04 (T+1 of T=2).
       entry_price = 1.0; exit_price = 1.1.
       holding_period_bars = 3 - 2 = 1.
     Trade 2 (short, force-closed):
       entry_timestamp = 2024-01-04 (T+1 of T=2).
       exit_timestamp  = 2024-01-05 (last bar; FR-10).
       entry_price = 1.1; exit_price = price.(4) = 1.0.
       holding_period_bars = 4 - 3 = 1.

     Per-trade pnl (FR-9 with half-allocation of sign-flip cost):
       Trade 1 has rebalance-bar segment [i_0=1, i_R=2):
         segment ratio = price.(2) / price.(1) = 1.0/1.0 = 1.0.
         segment pnl   = 1.0 * 1.0 * (1.0 - 1.0) = 0.0.
         pnl_1 = 0.0 - cost1 - 0.5 * cost2
               = -0.0015 - 0.00149775 = -0.00299775.
       Trade 2 has rebalance-bar segment [i_0=2, i_R=4) (force-close
       uses the last bar's index for i_R per FR-10):
         segment ratio = price.(4) / price.(2) = 1.0/1.0 = 1.0.
         segment pnl   = (-1.0) * (equity.(2)) * (1.0 - 1.0) = 0.0.

       Hmm — segment ratio of 1.0 on the rebalance bars makes the
       FR-9 segment contribution zero for both trades, but the
       equity curve clearly captures a non-zero P&L from holding
       the short through the [1.0 → 1.1 → 1.0] round trip. The
       price-bar / rebalance-bar mismatch in FR-9 (price_{i_R} is
       the resolution rebalance bar's price, not the resolution
       execution bar's price) means the full P&L of the round trip
       does not appear in the FR-9 segment formula here — it
       appears via the MTM accumulation between the rebalance
       bars and the price-frame's last bar.

       We assert only the equity curve and the trade fields
       (timestamps, prices, holding_period_bars) for this test.
       The per-trade [pnl] is a downstream concern of FR-9 that
       Task 3 must reconcile against the equity-trade identity
       [sum(pnl) +. 1.0 = equity.(N-1)]; that identity is checked
       in Test 1 (single-trade) and as a Layer 3 invariant
       [trade_pnl_sum_plus_one_equals_final_equity] (Task 4). *)

let alternating_long_short_pnl_matches_analytical () =
  let dates =
    [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
  in
  let prices = [| 1.0; 1.0; 1.0; 1.1; 1.0 |] in
  let signals = [| 0.0; 1.0; -1.0; -1.0; -1.0 |] in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame = make_frame [ ("A", make_daily_series dates signals) ] in
  let rebalance_index = make_daily_index [| "2024-01-02"; "2024-01-03" |] in
  let commission = 0.001 in
  let slippage = 0.0005 in
  let c = commission +. slippage in
  (* Pre-cost NAV at the t=1 rebalance = 1.0 (zero weights through t=0 make
     the MTM step a no-op). *)
  let nav_reb1 = 1.0 in
  let cost1 = c *. 1.0 *. nav_reb1 in
  let equity_1 = 1.0 -. cost1 in
  (* Pre-cost NAV at the t=2 rebalance = equity_1: the MTM ratio
     price.(2)/price.(1) = 1.0/1.0 = 1.0, so NAV does not drift between the
     two rebalances. The sign-flip turnover |dw| = 2.0 is costed against
     this NAV, not against price.(3). *)
  let nav_reb2 = equity_1 in
  let cost2 = c *. 2.0 *. nav_reb2 in
  let equity_2 = equity_1 -. cost2 in
  let equity_3 =
    equity_2 *. (1.0 +. (-1.0 *. ((prices.(3) /. prices.(2)) -. 1.0)))
  in
  let equity_4 =
    equity_3 *. (1.0 +. (-1.0 *. ((prices.(4) /. prices.(3)) -. 1.0)))
  in
  let expected_equity = [| 1.0; equity_1; equity_2; equity_3; equity_4 |] in
  match
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  with
  | Error msg -> Alcotest.fail msg
  | Ok result ->
      let equity_actual =
        Nx.to_array (Cairos.Series.values result.equity_curve)
      in
      check_float_array_close ~tol:1e-10 ~msg:"equity_curve" expected_equity
        equity_actual;
      (* Sum-of-pnl identity per FR-10. *)
      let pnl_sum =
        List.fold_left
          (fun acc (t : Cairos_engine.Trade.t) -> acc +. t.pnl)
          0.0
          (Cairos.Nonempty.to_list result.trades)
      in
      Alcotest.(check (float 1e-10))
        "sum(pnl) + 1 = equity.(last)" (equity_4 -. 1.0) pnl_sum;
      (* Trade count: sign flip (1 resolve + 1 incept) + force-close
         (1 resolve) = 2 trades. *)
      Alcotest.(check int)
        "trade count" 2
        (Cairos.Nonempty.length result.trades);
      let trade_long, trade_short =
        match Cairos.Nonempty.to_list result.trades with
        | [ a; b ] -> (a, b)
        | _ -> Alcotest.fail "expected exactly 2 trades"
      in
      Alcotest.(check ptime_testable)
        "long entry_timestamp"
        (ptime_of_date "2024-01-03T00:00:00Z")
        trade_long.entry_timestamp;
      Alcotest.(check ptime_testable)
        "long exit_timestamp"
        (ptime_of_date "2024-01-04T00:00:00Z")
        trade_long.exit_timestamp;
      Alcotest.(check (float 1e-10))
        "long entry_price" prices.(2) trade_long.entry_price;
      Alcotest.(check (float 1e-10))
        "long exit_price" prices.(3) trade_long.exit_price;
      Alcotest.(check int)
        "long holding_period_bars" 1 trade_long.holding_period_bars;
      Alcotest.(check ptime_testable)
        "short entry_timestamp"
        (ptime_of_date "2024-01-04T00:00:00Z")
        trade_short.entry_timestamp;
      Alcotest.(check ptime_testable)
        "short exit_timestamp"
        (ptime_of_date "2024-01-05T00:00:00Z")
        trade_short.exit_timestamp;
      Alcotest.(check (float 1e-10))
        "short entry_price" prices.(3) trade_short.entry_price;
      Alcotest.(check (float 1e-10))
        "short exit_price (force-close at last close)" prices.(4)
        trade_short.exit_price;
      Alcotest.(check int)
        "short holding_period_bars" 1 trade_short.holding_period_bars

(* === Test 3: single_rebalance_known_cost_known_pnl ===

   RFC 0056 §Test Plan Layer 1 row 3.

   Focuses on the trade-record fields under the simplest possible
   topology: one instrument, one rebalance, one force-close at the last
   bar. Every field is computed analytically from RFC 0052 OC-3 / OC-6
   and PRD 0053 FR-7..FR-10.

   Setup:
     dates       = [2024-01-01; 2024-01-02; 2024-01-03; 2024-01-04]
     prices      = [100.0; 100.0; 102.0; 104.04]
       — flat from t=0 to t=1 (zero weights, no MTM drift), then +2%
       per bar from t=1 onward.
     signals     = [0.0; 1.0; 1.0; 1.0]
     rebalance   = [t=1] (2024-01-02)
     commission  = 0.001
     slippage    = 0.0005

   Derivation:
     - [t=0]: not rebalance. equity.(0) = 1.0.
     - [t=1]: rebalance T=1. MTM with zero weights leaves the pre-cost NAV
         at [nav_after_mtm = 1.0]. Cost is turnover as a fraction of pre-cost
         NAV (feature 0058 / RFC 0052 Amendment A1), independent of the
         price level: [cost = (c+s) *. |dw| *. nav_after_mtm
         = 0.0015 *. 1.0 *. 1.0 = 0.0015].
         equity.(1) = 1.0 - 0.0015 = 0.9985. weights := 1.0.
         (Strong pin: the old price-notional formula charged
         [(c+s) *. 1.0 *. 102.0 = 0.153], ~100x larger. Prices anchored at
         100 make the two conventions differ by the price level, so this
         test alone rules out any price-dimensioned cost — execution_price
         = price.(2) = 102.0 now only sets the trade's entry_price.)
     - [t=2]: not rebalance. equity.(2) = 0.9985 * 102.0/100.0
              = 0.9985 * 1.02 = 1.018470.
     - [t=3]: not rebalance. equity.(3) = 1.018470 * 104.04/102.0
              = 1.018470 * 1.02 = 1.0388394.

   Trade record (force-close at [t=3] = last bar):
     - entry_timestamp = price_index.(2) = 2024-01-03 (T+1 of T=1).
     - exit_timestamp  = price_index.(3) = 2024-01-04 (last bar, FR-10).
     - entry_price     = price.(2) = 102.0.
     - exit_price      = price.(3) = 104.04 (last bar's close; FR-10).
     - holding_period_bars = 3 - 2 = 1.
     - pnl = equity.(3) - 1.0 (single-trade equity-trade identity, FR-10). *)

let single_rebalance_known_cost_known_pnl () =
  let dates = [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |] in
  let prices = [| 100.0; 100.0; 102.0; 104.04 |] in
  let signals = [| 0.0; 1.0; 1.0; 1.0 |] in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame = make_frame [ ("A", make_daily_series dates signals) ] in
  let rebalance_index = make_daily_index [| "2024-01-02" |] in
  let commission = 0.001 in
  let slippage = 0.0005 in
  (* Pre-cost NAV at the t=1 rebalance is [1.0]: zero weights through [t=0]
     make the MTM step a no-op, so the cost is [(c+s) *. |dw| *. 1.0
     = 0.0015] regardless of the price anchor at 100. Under the old
     price-notional formula this was [0.0015 *. 1.0 *. 102.0 = 0.153]. *)
  let nav_at_rebalance = 1.0 in
  let cost = (commission +. slippage) *. 1.0 *. nav_at_rebalance in
  let equity_0 = 1.0 in
  let equity_1 = 1.0 -. cost in
  let equity_2 = equity_1 *. (prices.(2) /. prices.(1)) in
  let equity_3 = equity_2 *. (prices.(3) /. prices.(2)) in
  let expected_equity = [| equity_0; equity_1; equity_2; equity_3 |] in
  match
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  with
  | Error msg -> Alcotest.fail msg
  | Ok result ->
      let equity_actual =
        Nx.to_array (Cairos.Series.values result.equity_curve)
      in
      check_float_array_close ~tol:1e-10 ~msg:"equity_curve" expected_equity
        equity_actual;
      (* weights frame: 0 at t=0, then 1.0 from t=1 onward (held). *)
      let weights_a = frame_get_values "A" result.weights in
      check_float_array_close ~tol:1e-10 ~msg:"weights[A]"
        [| 0.0; 1.0; 1.0; 1.0 |] weights_a;
      Alcotest.(check int)
        "trade count" 1
        (Cairos.Nonempty.length result.trades);
      let trade = Cairos.Nonempty.hd result.trades in
      Alcotest.(check ptime_testable)
        "entry_timestamp"
        (ptime_of_date "2024-01-03T00:00:00Z")
        trade.entry_timestamp;
      Alcotest.(check ptime_testable)
        "exit_timestamp"
        (ptime_of_date "2024-01-04T00:00:00Z")
        trade.exit_timestamp;
      Alcotest.(check string) "instrument" "A" trade.instrument;
      Alcotest.(check (float 1e-10)) "entry_price" prices.(2) trade.entry_price;
      Alcotest.(check (float 1e-10)) "exit_price" prices.(3) trade.exit_price;
      Alcotest.(check int) "holding_period_bars" 1 trade.holding_period_bars;
      Alcotest.(check (float 1e-10)) "pnl" (equity_3 -. 1.0) trade.pnl

let () =
  Alcotest.run "cairos_engine known outcomes"
    [
      ( "Layer 1",
        [
          Alcotest.test_case
            "always_long_equity_curve_matches_compounded_return" `Quick
            always_long_equity_curve_matches_compounded_return;
          Alcotest.test_case "alternating_long_short_pnl_matches_analytical"
            `Quick alternating_long_short_pnl_matches_analytical;
          Alcotest.test_case "single_rebalance_known_cost_known_pnl" `Quick
            single_rebalance_known_cost_known_pnl;
        ] );
    ]
