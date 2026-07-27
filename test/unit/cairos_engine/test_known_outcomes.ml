(* Layer 1 — known-outcome strategy tests for [Cairos_engine.Backtest.run].

   Each test constructs price / signal / rebalance inputs for which the loop's
   output can be derived analytically from first principles plus the
   trade-accounting rules. Tolerance is absolute [1e-10].

   Tests 1-3 were RED until the loop was implemented (the stub returned
   [Error "not implemented"]); implementing it made them GREEN. Tests added
   after the loop shipped cannot get their red that way — the engine is
   already correct. They establish it instead by mutating the derivation to
   the rival convention it is meant to exclude (the literal mark-to-market
   form, a whole-cost-to-one-leg flip split) and observing the specific
   assertion fail, then reverting. Which mutation reddens which assertion is
   recorded per test below. *)

(* === Test helpers ===

   These mirror [test/unit/cairos_finance/finance_test_helpers.ml] and
   [test/unit/cairos/test_helpers.ml]. We do not reach into [test_helpers]
   from this directory — no helper library is declared for
   [test/unit/cairos_engine/], and the helpers below are
   small enough that one-time inlining is cheaper than carrying a library
   dependency in the cycle-check gate. *)

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

   Setup: one instrument "A", five daily bars, price grows exactly 1% per
   bar starting at [1.0] (so [price.(i) = 1.01 ** i]). Single rebalance at
   [t=1] with [target_weight = 1.0]. End-of-backtest force-close at the
   last bar.

   Why first rebalance at [t=1] (not [t=0]):
     The equity-curve value at a rebalance bar is
     [nav_after_costs] (post-cost, pre-MTM). The first
     equity-curve cell is [1.0] only when the price-frame's first bar is
     not a rebalance — otherwise it is [1.0 -. cost], and the Layer 3
     [equity_curve_starts_at_one] invariant fails. We pin
     the first rebalance at [t=1] so [equity_curve.(0) = 1.0] holds
     structurally.

   Derivation:
     - Initial state: [nav_0 = 1.0], all weights zero.
     - [t=0]: not a rebalance. Mark-to-market with zero weights leaves
       NAV unchanged. [equity.(0) = 1.0].
     - [t=1] (rebalance, T=1): execution at [T+1 = 2], so
       [execution_price = price.(2) = 1.0201] (this sets the trade's
       entry_price; it no longer enters the cost). MTM with zero weights
       leaves the pre-cost NAV at [nav_after_mtm = 1.0].
       [weight_delta = 1.0 - 0 = 1.0]. Cost is charged on turnover as a
       fraction of pre-cost NAV,
       not on the price level:
       [cost = (commission +. slippage) *. |weight_delta| *. nav_after_mtm
             = 0.0015 *. 1.0 *. 1.0 = 0.0015].
       NAV-update ordering (unchanged): deduct cost first, then
       apply new weights.
         [equity.(1) = nav_after_mtm -. cost = 1.0 -. 0.0015 = 0.9985]
         current_weights := 1.0
     - [t in {2,3,4}]: not rebalances. MTM with weight = 1.0:
         [equity.(i) = equity.(i-1) *. (price.(i) /. price.(i-1))]
         [        = equity.(i-1) *. 1.01].
       Hence [equity.(i) = (1.0 -. cost) *. 1.01 ** (i - 1)] for [i >= 1].

   Note (off-by-one in the naive formula): writing the formula as
   [equity.(i) = (1 - cost) * 1.01 ** i] for
   [i >= 1] is off by one. With [equity.(1) = 1 - cost] (no MTM applied at
   the rebalance bar itself) the correct exponent is [i - 1], not [i]. We
   follow the first-principles derivation above.

   Trade record (force-close at [t=4]):
     - [entry_timestamp = price_index.(2)] (T+1 of T=1)
     - [exit_timestamp  = price_index.(4)] (last bar, force-close)
     - [entry_price     = price.(2) = 1.0201]
     - [exit_price      = price.(4) = 1.04060401] (last bar's close)
     - [holding_period_bars = 4 - 2 = 2]
     - [pnl] derived from the equity-trade identity for the
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
  | Error e -> Alcotest.fail (Cairos_engine.Backtest.err_to_string e)
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

   Setup: one instrument "A", five daily bars. Two rebalances forming a
   sign flip (long → short). End-of-backtest force-close at the last bar.

   Prices chosen so MTM ratios are clean: between rebalance bars and
   their executions, [price.(2)/price.(1) = 1.0] (no MTM drift), and
   the held-position MTM segment uses prices [1.0, 1.0, 1.0, 1.1, 1.0].

   Convention used for shorts:
     One MTM formula is
     [nav_t = nav_{t-1} *. sum_j (w_j *. p_t /. p_{t-1})]. Read literally
     this gives nonsensical (negative) NAV when [sum w_j < 0] (e.g.
     a single instrument with [w = -1]). The standard fractional-weight
     "weighted return" formula
     [nav_t = nav_{t-1} *. (1.0 +. sum_j (w_j *. (p_t /. p_{t-1} -. 1.0)))]
     is equivalent to that wording when [sum_j w_j = 1.0]
     (fully invested) and gives the realistic loss-on-price-up behaviour
     for shorts. This test assumes the engine implements the standard
     formula. If the loop body matches the literal wording
     (sum-of-weights = 1 only), this test will surface the divergence
     and the implementation choice is escalated.

   Sign-flip cost split:
     At a sign-flip rebalance with [w_old = +1.0, w_new = -1.0],
     [|weight_delta| = 2.0 = |w_old| + |w_new|]. The full per-rebalance
     cost [(c+s) *. 2.0 *. nav] (nav = pre-cost NAV at that bar) is paid
     once. The cost attribution assigns all costs paid at
     i_0, ..., i_K and i_R to the resolving trade.
     For the incepting trade at the same rebalance, the cost at i_0
     (its inception) is the same rebalance cost. The rule leaves the
     allocation between the two trades implicit. We split the cost
     proportionally to each side's contribution to [|weight_delta|]:
     half attributed to the closing trade, half to the opening trade
     (since [|w_old| = |w_new| = 1.0] here). If the engine uses a different
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
       exit_timestamp  = 2024-01-05 (last bar).
       entry_price = 1.1; exit_price = price.(4) = 1.0.
       holding_period_bars = 4 - 3 = 1.

     Per-trade pnl (half-allocation of sign-flip cost):
       Trade 1 has rebalance-bar segment [i_0=1, i_R=2):
         segment ratio = price.(2) / price.(1) = 1.0/1.0 = 1.0.
         segment pnl   = 1.0 * 1.0 * (1.0 - 1.0) = 0.0.
         pnl_1 = 0.0 - cost1 - 0.5 * cost2
               = -0.0015 - 0.00149775 = -0.00299775.
       Trade 2 has rebalance-bar segment [i_0=2, i_R=4) (force-close
       uses the last bar's index for i_R):
         segment ratio = price.(4) / price.(2) = 1.0/1.0 = 1.0.
         segment pnl   = (-1.0) * (equity.(2)) * (1.0 - 1.0) = 0.0.

       Hmm — segment ratio of 1.0 on the rebalance bars makes the
       segment contribution zero for both trades, but the
       equity curve clearly captures a non-zero P&L from holding
       the short through the [1.0 → 1.1 → 1.0] round trip. The
       price-bar / rebalance-bar mismatch (price_{i_R} is
       the resolution rebalance bar's price, not the resolution
       execution bar's price) means the full P&L of the round trip
       does not appear in the segment formula here — it
       appears via the MTM accumulation between the rebalance
       bars and the price-frame's last bar.

       We assert only the equity curve and the trade fields
       (timestamps, prices, holding_period_bars) for this test.
       The per-trade [pnl] is a downstream concern reconciled
       against the equity-trade identity
       [sum(pnl) +. 1.0 = equity.(N-1)]; that identity is checked
       in Test 1 (single-trade) and as a Layer 3 invariant
       [trade_pnl_sum_plus_one_equals_final_equity]. *)

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
  | Error e -> Alcotest.fail (Cairos_engine.Backtest.err_to_string e)
  | Ok result ->
      let equity_actual =
        Nx.to_array (Cairos.Series.values result.equity_curve)
      in
      check_float_array_close ~tol:1e-10 ~msg:"equity_curve" expected_equity
        equity_actual;
      (* Sum-of-pnl identity. *)
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

   Focuses on the trade-record fields under the simplest possible
   topology: one instrument, one rebalance, one force-close at the last
   bar. Every field is computed analytically from first principles
   and the trade-accounting rules.

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
         NAV, independent of the
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
     - exit_timestamp  = price_index.(3) = 2024-01-04 (last bar).
     - entry_price     = price.(2) = 102.0.
     - exit_price      = price.(3) = 104.04 (last bar's close).
     - holding_period_bars = 3 - 2 = 1.
     - pnl = equity.(3) - 1.0 (single-trade equity-trade identity). *)

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
  | Error e -> Alcotest.fail (Cairos_engine.Backtest.err_to_string e)
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

(* === Test 4: short_only_equity_curve_matches_analytical ===

   The three tests above never hold a short across a one-way price path:
   Test 2 holds one, but over a path that returns to where it started. So
   nothing yet pins the equity curve of a short through monotonically rising
   prices — the case where the literal reading of the mark-to-market rule,
   [nav_t = nav_{t-1} *. sum_j (w_j *. p_t /. p_{t-1})], drives NAV negative
   rather than merely down. The standard weighted-return form,
   [nav_t = nav_{t-1} *. (1.0 +. sum_j (w_j *. (p_t /. p_{t-1} -. 1.0)))],
   is the one in force; this test is where the two part company visibly.

   Setup:
     dates       = [2024-01-01 .. 2024-01-05]
     prices      = [80.0; 100.0; 125.0; 156.25; 195.3125]
       — exactly +25% per bar; every ratio is exact in binary floating point.
     signals     = [0.0; -1.0; -1.0; -1.0; -1.0]
     rebalance   = [t=1] (2024-01-02)
     commission  = 0.001
     slippage    = 0.0005   (c = commission +. slippage = 0.0015)

   Derivation:
     - [t=0]: not a rebalance, weights are zero. The weighted-return form
       gives [nav_0 = 1.0 *. (1.0 +. 0.0 *. (100.0/80.0 -. 1.0)) = 1.0].
       (The literal sum-of-w-times-ratio form gives [1.0 *. 0.0 = 0.0] here —
       the two readings already differ before any position exists.)
       equity.(0) = 1.0.
     - [t=1]: rebalance T=1. Mark-to-market with zero weights leaves the
       pre-cost NAV at [nav_after_mtm = 1.0].
       [weight_delta = -1.0 -. 0.0 = -1.0]. Cost is turnover as a fraction of
       pre-cost NAV, not a price level:
         [cost = (c) *. |weight_delta| *. nav_after_mtm
               = 0.0015 *. 1.0 *. 1.0 = 0.0015]
       (A price-dimensioned notional would charge
       [0.0015 *. 1.0 *. 125.0 = 0.1875] — 125x larger.)
       Deduct cost first, then apply the new weights:
         equity.(1) = 1.0 -. 0.0015 = 0.9985
         current_weights := -1.0
       Execution at T+1 = bar 2 sets the trade's entry_price = price.(2).
     - [t in {2,3,4}]: not rebalances. weight = -1.0, ratio = 1.25:
         equity.(i) = equity.(i-1) *. (1.0 +. (-1.0) *. (1.25 -. 1.0))
                    = equity.(i-1) *. 0.75
         equity.(2) = 0.748875
         equity.(3) = 0.56165625
         equity.(4) = 0.4212421875
       The curve falls on every bar and stays strictly positive. Under the
       literal form equity.(2) would be [0.9985 *. (-1.0 *. 1.25)
       = -1.248125] — negative, and every bar after it meaningless.

   Trade record (force-close at [t=4] = last bar):
     - entry_timestamp = price_index.(2) = 2024-01-03 (T+1 of T=1)
     - exit_timestamp  = price_index.(4) = 2024-01-05 (last bar)
     - entry_price     = price.(2) = 125.0
     - exit_price      = price.(4) = 195.3125 (last bar's close; a
       force-close is not a rebalance, so no exit cost is charged)
     - holding_period_bars = 4 - 2 = 2
     - pnl: one trade spans the whole run, so the equity/trade-log identity
       [1.0 +. sum(pnl) = equity.(N-1)] fixes it at [equity.(4) -. 1.0
       = -0.5787578125].

   Note on the constant-weight segment form of pnl. Written as a single
   segment [w *. nav_at_segment_entry *. (price_end /. price_start -. 1.0)]
   net of costs, this trade would be
   [-1.0 *. 0.9985 *. (195.3125/100.0 -. 1.0) -. 0.0015 = -0.9531953125],
   which is not [equity.(4) -. 1.0]. The segment product and the
   multiplicative mark-to-market path agree only when the held position
   compounds linearly — any weight across a single mark-to-market step, or
   [w = 1.0] across any number of them. Three steps at [w = -1.0] is neither.
   The equity/trade-log identity is the pinned reconciliation and is what is
   asserted here; Test 2's comment records the same tension from the other
   side. Settling the segment form itself belongs to the mid-series
   size-adjustment scenario, where it is the behaviour under test rather than
   an incidental consequence.

   Red-first (by rival-hypothesis mutation, per this file's head comment).
   Each applied on its own, run, and reverted:

   - Rewriting [mtm] as the literal [nav *. (w *. p_t /. p_{t-1})] reddens
     [equity_curve [2]] — expected -1.24813 against 0.748875. The expected
     value goes *negative*, which is the literal form's documented failure on
     a short and the reason it was superseded.
   - Expecting the single-segment pnl
     [-1.0 *. equity_1 *. (price.(4)/price.(1) -. 1.0) -. cost] instead of the
     identity reddens [pnl] — -0.953195 against -0.578758. *)

let short_only_equity_curve_matches_analytical () =
  let dates =
    [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
  in
  let prices = [| 80.0; 100.0; 125.0; 156.25; 195.3125 |] in
  let signals = [| 0.0; -1.0; -1.0; -1.0; -1.0 |] in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame = make_frame [ ("A", make_daily_series dates signals) ] in
  let rebalance_index = make_daily_index [| "2024-01-02" |] in
  let commission = 0.001 in
  let slippage = 0.0005 in
  let c = commission +. slippage in
  let target_weight = -1.0 in
  (* Zero weights through [t=0] make the mark-to-market step a no-op, so the
     pre-cost NAV at the [t=1] rebalance is [1.0] whatever price.(1)/price.(0)
     is, and the cost is a pure fraction of it. *)
  let nav_at_rebalance = 1.0 in
  let weight_delta = target_weight -. 0.0 in
  let cost = c *. Float.abs weight_delta *. nav_at_rebalance in
  let mtm nav i =
    nav *. (1.0 +. (target_weight *. ((prices.(i) /. prices.(i - 1)) -. 1.0)))
  in
  let equity_0 = 1.0 in
  let equity_1 = nav_at_rebalance -. cost in
  let equity_2 = mtm equity_1 2 in
  let equity_3 = mtm equity_2 3 in
  let equity_4 = mtm equity_3 4 in
  let expected_equity =
    [| equity_0; equity_1; equity_2; equity_3; equity_4 |]
  in
  match
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  with
  | Error e -> Alcotest.fail (Cairos_engine.Backtest.err_to_string e)
  | Ok result ->
      let equity_actual =
        Nx.to_array (Cairos.Series.values result.equity_curve)
      in
      check_float_array_close ~tol:1e-10 ~msg:"equity_curve" expected_equity
        equity_actual;
      (* Falls on every bar: the cost at [t=1], then a 25% loss per bar on the
         short. Stated separately from the array check so a sign error in the
         weighted-return form reads as "the short made money" rather than as a
         cell mismatch. *)
      Array.iteri
        (fun i v ->
          if i > 0 then
            Alcotest.(check bool)
              (Printf.sprintf "equity falls at bar %d" i)
              true
              (v < equity_actual.(i - 1)))
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
      Alcotest.(check (float 1e-10)) "pnl" (equity_4 -. 1.0) trade.pnl

(* === Test 5: sign_flip_costs_charged_on_both_legs ===

   Test 2 covers a sign flip, but a symmetric one (+1.0 -> -1.0) whose
   proportional cost split is 50/50 and therefore indistinguishable from any
   other rule that halves the cost; and it asserts only the *sum* of the two
   trades' pnl, which every allocation of a fixed total preserves. This test
   makes the split observable: an asymmetric flip (+1.0 -> -0.5) whose two
   shares are 2/3 and 1/3 of the rebalance's cost, each asserted separately.

   Rules being pinned:
     - At a rebalance the per-instrument cost is
       [(c+s) *. |weight_delta| *. nav], where [nav] is the pre-cost NAV at
       that bar — post-mark-to-market, pre-deduction. Both legs of a flip are
       charged against that same [nav].
     - A sign flip resolves the in-flight position and incepts a new one at
       the same T+1 open. Its single cost is split proportionally to each
       side's contribution to |weight_delta|:
         closing_share = cost *. |w_old| /. (|w_old| +. |w_new|)
         opening_share = cost *. |w_new| /. (|w_old| +. |w_new|)
       For a flip [|weight_delta| = |w_old| +. |w_new|], so the two shares
       exhaust the cost exactly.
     - A trade's pnl is its constant-weight segment P&L net of the costs
       attributed to it: the whole cost at a rebalance that is its own
       inception or resolution, and its share of a flip's cost.
     - An end-of-backtest force-close pays no exit cost — costs are charged
       at rebalance dates only, and the end of the price frame is not one.

   Setup:
     dates       = [2024-01-01 .. 2024-01-05]
     prices      = [100.0; 100.0; 125.0; 125.0; 156.25]
     signals     = [0.0; 1.0; -0.5; -0.5; -0.5]
     rebalance   = [t=1; t=2] (2024-01-02, 2024-01-03)
     commission  = 0.001
     slippage    = 0.0005   (c = 0.0015)

   The price path is deliberate on two counts. It moves +25% between bar 1
   and bar 2, so the pre-cost NAV at the flip (1.248125) is far from the NAV
   the first rebalance left behind (0.9985); charging the flip against the
   wrong one of those two is then a ~5.6e-4 error rather than a rounding
   difference. And it is flat between bar 2 and bar 3, so each leg is held
   across exactly one non-trivial mark-to-market step — the condition under
   which the constant-weight segment form and the equity path agree exactly
   (Test 4's closing note). Both legs' pnl are therefore unambiguous.

   Derivation:
     - [t=0]: not a rebalance, zero weights. equity.(0) = 1.0.
     - [t=1]: rebalance T=1. Pre-cost nav = 1.0 (zero weights, MTM no-op).
         weight_delta = 1.0 -. 0.0 = 1.0
         cost1 = 0.0015 *. 1.0 *. 1.0 = 0.0015
         equity.(1) = 1.0 -. 0.0015 = 0.9985; current_weights := +1.0
       Trade L incepts at T+1 = bar 2, entry_price = price.(2) = 125.0.
     - [t=2]: rebalance T=2. Mark to market first, at the held +1.0:
         nav_before = 0.9985 *. (1.0 +. 1.0 *. (125.0/100.0 -. 1.0))
                    = 0.9985 *. 1.25 = 1.248125
         weight_delta = -0.5 -. 1.0 = -1.5
         cost2 = 0.0015 *. 1.5 *. 1.248125 = 0.00280828125
         closing_share = cost2 *. 1.0 /. 1.5 = 0.0018721875
         opening_share = cost2 *. 0.5 /. 1.5 = 0.00093609375
         equity.(2) = 1.248125 -. 0.00280828125 = 1.24531671875
         current_weights := -0.5
       Trade L resolves and trade S incepts, both at T+1 = bar 3, at
       price.(3) = 125.0.
     - [t=3]: not a rebalance. ratio = 125.0/125.0 = 1.0, so
         equity.(3) = equity.(2) = 1.24531671875.
     - [t=4]: not a rebalance. ratio = 156.25/125.0 = 1.25, weight -0.5:
         equity.(4) = 1.24531671875 *. (1.0 +. (-0.5) *. 0.25)
                    = 1.24531671875 *. 0.875 = 1.08965212890625

   Trade L (long, resolved by the flip):
     entry_timestamp = 2024-01-03 (bar 2); exit_timestamp = 2024-01-04 (bar 3)
     entry_price = 125.0; exit_price = 125.0; holding_period_bars = 1
     segment pnl = +1.0 *. 0.9985 *. (125.0/100.0 -. 1.0) = 0.249625
       (the segment endpoints are the rebalance bars 1 and 2, not the
       execution bars — entry_price is the execution bar's price and is a
       reported field, not a term in this sum)
     pnl = 0.249625 -. cost1 -. closing_share
         = 0.249625 -. 0.0015 -. 0.0018721875 = 0.2462528125

   Trade S (short, force-closed at the last bar):
     entry_timestamp = 2024-01-04 (bar 3); exit_timestamp = 2024-01-05 (bar 4)
     entry_price = 125.0; exit_price = 156.25; holding_period_bars = 1
     segment pnl = -0.5 *. 1.24531671875 *. (156.25/125.0 -. 1.0)
                 = -0.15566458984375
     pnl = -0.15566458984375 -. opening_share = -0.15660068359375

   Cross-check: [1.0 +. 0.2462528125 -. 0.15660068359375 = 1.08965212890625]
   = equity.(4). The identity holds for *any* allocation that exhausts cost2,
   which is exactly why each share is asserted on its own below.

   Red-first (by rival-hypothesis mutation, per this file's head comment):
   setting [closing_share = cost2] and [opening_share = 0.0] — the whole flip
   cost on the closing leg — reddens "closing leg's share of the flip cost",
   expected 0.00280828 against 0.00187219. All six [equity_curve] assertions,
   the trade count and every entry/exit field stayed green ahead of it; the
   share assertion is the first thing to fail. That is the discrimination this
   test exists for: the identity holds under any allocation exhausting cost2,
   so only a per-leg assertion can see the split. Applied on its own, run, and
   reverted. *)

let sign_flip_costs_charged_on_both_legs () =
  let dates =
    [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
  in
  let prices = [| 100.0; 100.0; 125.0; 125.0; 156.25 |] in
  let signals = [| 0.0; 1.0; -0.5; -0.5; -0.5 |] in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame = make_frame [ ("A", make_daily_series dates signals) ] in
  let rebalance_index = make_daily_index [| "2024-01-02"; "2024-01-03" |] in
  let commission = 0.001 in
  let slippage = 0.0005 in
  let c = commission +. slippage in
  let w_old = 1.0 in
  let w_new = -0.5 in
  (* Rebalance 1 at [t=1]: zero weights make the mark-to-market step a no-op,
     so the pre-cost NAV is 1.0. *)
  let nav_reb1 = 1.0 in
  let cost1 = c *. Float.abs (w_old -. 0.0) *. nav_reb1 in
  let equity_1 = nav_reb1 -. cost1 in
  (* Rebalance 2 at [t=2]: mark to market at the held [w_old] first; the
     flip's single cost is charged against that post-MTM, pre-deduction NAV,
     and both legs share it. *)
  let nav_reb2 =
    equity_1 *. (1.0 +. (w_old *. ((prices.(2) /. prices.(1)) -. 1.0)))
  in
  let cost2 = c *. Float.abs (w_new -. w_old) *. nav_reb2 in
  let gross_turnover = Float.abs w_old +. Float.abs w_new in
  let closing_share = cost2 *. Float.abs w_old /. gross_turnover in
  let opening_share = cost2 *. Float.abs w_new /. gross_turnover in
  let equity_2 = nav_reb2 -. cost2 in
  let equity_3 =
    equity_2 *. (1.0 +. (w_new *. ((prices.(3) /. prices.(2)) -. 1.0)))
  in
  let equity_4 =
    equity_3 *. (1.0 +. (w_new *. ((prices.(4) /. prices.(3)) -. 1.0)))
  in
  let expected_equity = [| 1.0; equity_1; equity_2; equity_3; equity_4 |] in
  (* Constant-weight segment P&L per leg, over the rebalance-bar endpoints.
     Each leg spans exactly one non-trivial mark-to-market step, so the
     segment form and the equity path coincide. *)
  let long_segment_pnl =
    w_old *. equity_1 *. ((prices.(2) /. prices.(1)) -. 1.0)
  in
  let short_segment_pnl =
    w_new *. equity_2 *. ((prices.(4) /. prices.(2)) -. 1.0)
  in
  match
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  with
  | Error e -> Alcotest.fail (Cairos_engine.Backtest.err_to_string e)
  | Ok result ->
      let equity_actual =
        Nx.to_array (Cairos.Series.values result.equity_curve)
      in
      check_float_array_close ~tol:1e-10 ~msg:"equity_curve" expected_equity
        equity_actual;
      Alcotest.(check int)
        "trade count (flip resolves one and incepts one)" 2
        (Cairos.Nonempty.length result.trades);
      (* Ordering is contractual, not incidental: records are appended on
         resolution, so the long leg — resolved by the flip at [t=2] — precedes
         the short, which resolves only at the end-of-backtest force-close.
         Destructured positionally rather than searched for, so a change to the
         append order fails here rather than silently swapping which leg each
         per-field assertion below is checking. *)
      let trade_long, trade_short =
        match Cairos.Nonempty.to_list result.trades with
        | [ a; b ] -> (a, b)
        | _ -> Alcotest.fail "expected exactly 2 trades"
      in
      Alcotest.(check string) "long instrument" "A" trade_long.instrument;
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
      Alcotest.(check string) "short instrument" "A" trade_short.instrument;
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
        "short holding_period_bars" 1 trade_short.holding_period_bars;
      (* Each leg's share of the flip's cost, recovered from the engine's own
         pnl values by subtracting the derived segment P&L and the costs that
         are unambiguously the leg's own. These two assertions are the two
         [pnl] assertions rearranged into the terms this test is about: an
         allocation putting the whole flip cost on the closing leg leaves both
         the sum of pnl and the equity curve untouched, and is caught here and
         nowhere else. *)
      let implied_closing_share = long_segment_pnl -. cost1 -. trade_long.pnl in
      let implied_opening_share = short_segment_pnl -. trade_short.pnl in
      Alcotest.(check (float 1e-10))
        "closing leg's share of the flip cost" closing_share
        implied_closing_share;
      Alcotest.(check (float 1e-10))
        "opening leg's share of the flip cost" opening_share
        implied_opening_share;
      Alcotest.(check (float 1e-10))
        "the two shares exhaust the flip's cost" cost2
        (implied_closing_share +. implied_opening_share)

(* === Test 6: two_instruments_unequal_weights_known_pnl ===

   Every test above holds one instrument. Nothing yet pins how two positions
   at *different* weights combine into one NAV, nor that each instrument's
   turnover is costed against its own [|weight_delta|] rather than the
   portfolio's. Both are single points of failure for a cross-sectional
   strategy, which is the MVP's acceptance-criterion shape.

   Setup:
     dates       = [2024-01-01 .. 2024-01-04]
     prices A    = [100.0; 100.0; 125.0; 156.25]   (+25%, +25%)
     prices B    = [200.0; 200.0; 250.0; 187.5]    (+25%, -25%)
     signals A   = [0.0; 0.75; 0.75; 0.75]
     signals B   = [0.0; 0.25; 0.25; 0.25]
     rebalance   = [t=1] (2024-01-02)
     commission  = 0.001
     slippage    = 0.0005   (c = 0.0015)

   Why these weights and this price path. The weights are 3:1, so an
   equal-weight assumption is a different number rather than a rounding
   difference, and they sum to [1.0] (fully invested). The two instruments
   move *together* on bar 2 and *apart* on bar 3. That ordering is deliberate,
   and the reason is the segment/accrual tension Test 4's closing note records:

     The pinned trade-accounting rule states a trade's P&L as a sum of
     constant-weight segments, each
     [w *. NAV_at_segment_entry *. (price_{i_R} /. price_{i_0} -. 1.0)] over
     *rebalance-bar* endpoints, while NAV itself accrues bar by bar through
     mark-to-market. The price *ratio* is the same either way — the per-bar
     ratios telescope to [price_{i_R} /. price_{i_0}] exactly. What differs is
     the NAV each step is scaled by: the segment form uses the entry NAV for
     every step, the accrual uses the running one. The two agree on a step [t]
     only when the portfolio return [R_s] equals this instrument's own return
     [r_{j,s}] on every earlier step [s] of the segment.
     Here [R_2 = 0.75 *. 0.25 +. 0.25 *. 0.25 = 0.25 = r_{A,2} = r_{B,2}],
     because full investment plus a common move makes the portfolio return the
     common return. So the bar-3 divergence is scaled by the same NAV under
     both readings, and every number below is the value the segment form and
     the engine's accrual *both* produce. The test asserts a value neither
     reading has to win to make true. (Reverse the two bars — diverge first,
     move together second — and they part company; that variant would be
     asserting one reading over the other and is deliberately not what is
     built here.)

   Derivation:
     - [t=0]: not a rebalance, zero weights. equity.(0) = 1.0.
     - [t=1]: rebalance T=1. Mark-to-market with zero weights leaves the
       pre-cost NAV at 1.0. Each instrument is costed on its *own* turnover:
         cost_A = 0.0015 *. |0.75 -. 0.0| *. 1.0 = 0.001125
         cost_B = 0.0015 *. |0.25 -. 0.0| *. 1.0 = 0.000375
         equity.(1) = 1.0 -. (0.001125 +. 0.000375) = 0.9985
       (The total is [0.0015], the same total a single full-weight position
       would pay — so the equity curve alone cannot see the split. The two
       [pnl] assertions below are where it becomes observable.)
       Both trades incept at T+1 = bar 2: entry_price_A = 125.0,
       entry_price_B = 250.0.
     - [t=2]: not a rebalance. r_A = 125.0/100.0 -. 1.0 = 0.25,
       r_B = 250.0/200.0 -. 1.0 = 0.25.
         R_2 = 0.75 *. 0.25 +. 0.25 *. 0.25 = 0.25
         equity.(2) = 0.9985 *. 1.25 = 1.248125
     - [t=3]: not a rebalance. r_A = 156.25/125.0 -. 1.0 = 0.25,
       r_B = 187.5/250.0 -. 1.0 = -0.25.
         R_3 = 0.75 *. 0.25 +. 0.25 *. (-0.25) = 0.1875 -. 0.0625 = 0.125
         equity.(3) = 1.248125 *. 1.125 = 1.404140625
       Under an equal-weight assumption (0.5/0.5) R_3 would be [0.0] and the
       curve would stop at 1.248125; with the two weights swapped it would be
       [-0.125]. Both are ~1.5e-1 away, not a tolerance question.

   Trade records (both force-closed at [t=3] = last bar, no exit cost):
     Trade A: entry 2024-01-03 @ 125.0; exit 2024-01-04 @ 156.25;
       holding_period_bars = 1.
       segment pnl = 0.75 *. 0.9985 *. (156.25/100.0 -. 1.0)
                   = 0.75 *. 0.9985 *. 0.5625 = 0.4212421875
       pnl = 0.4212421875 -. 0.001125 = 0.4201171875
     Trade B: entry 2024-01-03 @ 250.0; exit 2024-01-04 @ 187.5;
       holding_period_bars = 1.
       segment pnl = 0.25 *. 0.9985 *. (187.5/200.0 -. 1.0)
                   = 0.25 *. 0.9985 *. (-0.0625) = -0.0156015625
       pnl = -0.0156015625 -. 0.000375 = -0.0159765625

     The segment endpoints are the *rebalance* bars — [price.(1)] and, for a
     force-close, [price.(3)] — not the execution bars. [entry_price] is the
     execution bar's price (125.0 / 250.0) and is a reported field, not a term
     in either sum; the two differ here by construction so a test that
     conflated them would fail.

   Cross-check: [1.0 +. 0.4201171875 -. 0.0159765625 = 1.404140625] =
   equity.(3).

   Red-first (by rival-hypothesis mutation, per this file's head comment):
   splitting the rebalance's total cost equally between the two trades
   ([cost_a = cost_b = 0.00075] in the derivation) reddens ["A pnl"] —
   0.420492 expected against 0.420117 received — and leaves every
   equity-curve and weights assertion green, which is the discrimination this
   test exists for (the run stops at the first failing assertion, so ["B pnl"]
   is not reached; it moves by the same 3.75e-4). Aggregating the marks at
   equal weights ([0.5] for both instruments in [mtm]) reddens
   [equity_curve [3]] — 1.24813 against 1.40414. Each was observed failing on
   its own and reverted. *)

let two_instruments_unequal_weights_known_pnl () =
  let dates = [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |] in
  let prices_a = [| 100.0; 100.0; 125.0; 156.25 |] in
  let prices_b = [| 200.0; 200.0; 250.0; 187.5 |] in
  let w_a = 0.75 in
  let w_b = 0.25 in
  let signals_a = [| 0.0; w_a; w_a; w_a |] in
  let signals_b = [| 0.0; w_b; w_b; w_b |] in
  let price_frame =
    make_frame
      [
        ("A", make_daily_series dates prices_a);
        ("B", make_daily_series dates prices_b);
      ]
  in
  let signal_frame =
    make_frame
      [
        ("A", make_daily_series dates signals_a);
        ("B", make_daily_series dates signals_b);
      ]
  in
  let rebalance_index = make_daily_index [| "2024-01-02" |] in
  let commission = 0.001 in
  let slippage = 0.0005 in
  let c = commission +. slippage in
  (* Zero weights through [t=0] make the mark-to-market step a no-op, so the
     pre-cost NAV at the [t=1] rebalance is [1.0] and each instrument's cost is
     a pure fraction of it, charged on that instrument's own turnover. *)
  let nav_at_rebalance = 1.0 in
  let cost_a = c *. Float.abs (w_a -. 0.0) *. nav_at_rebalance in
  let cost_b = c *. Float.abs (w_b -. 0.0) *. nav_at_rebalance in
  let ret prices i = (prices.(i) /. prices.(i - 1)) -. 1.0 in
  let mtm nav i =
    nav *. (1.0 +. (w_a *. ret prices_a i) +. (w_b *. ret prices_b i))
  in
  let equity_0 = 1.0 in
  let equity_1 = nav_at_rebalance -. (cost_a +. cost_b) in
  let equity_2 = mtm equity_1 2 in
  let equity_3 = mtm equity_2 3 in
  let expected_equity = [| equity_0; equity_1; equity_2; equity_3 |] in
  (* Constant-weight segment P&L over the rebalance-bar endpoints: inception
     bar 1 to the last bar for a force-close. *)
  let segment_pnl w prices =
    w *. equity_1 *. ((prices.(3) /. prices.(1)) -. 1.0)
  in
  let pnl_a = segment_pnl w_a prices_a -. cost_a in
  let pnl_b = segment_pnl w_b prices_b -. cost_b in
  match
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  with
  | Error e -> Alcotest.fail (Cairos_engine.Backtest.err_to_string e)
  | Ok result ->
      let equity_actual =
        Nx.to_array (Cairos.Series.values result.equity_curve)
      in
      check_float_array_close ~tol:1e-10 ~msg:"equity_curve" expected_equity
        equity_actual;
      (* Held weights are carried per instrument, not as one portfolio number. *)
      check_float_array_close ~tol:1e-10 ~msg:"weights[A]"
        [| 0.0; w_a; w_a; w_a |]
        (frame_get_values "A" result.weights);
      check_float_array_close ~tol:1e-10 ~msg:"weights[B]"
        [| 0.0; w_b; w_b; w_b |]
        (frame_get_values "B" result.weights);
      Alcotest.(check int)
        "trade count (one force-close per instrument)" 2
        (Cairos.Nonempty.length result.trades);
      (* Column order is the price frame's insertion order, so the force-close
         emits A before B. *)
      let trade_a, trade_b =
        match Cairos.Nonempty.to_list result.trades with
        | [ a; b ] -> (a, b)
        | _ -> Alcotest.fail "expected exactly 2 trades"
      in
      Alcotest.(check string) "A instrument" "A" trade_a.instrument;
      Alcotest.(check ptime_testable)
        "A entry_timestamp"
        (ptime_of_date "2024-01-03T00:00:00Z")
        trade_a.entry_timestamp;
      Alcotest.(check ptime_testable)
        "A exit_timestamp"
        (ptime_of_date "2024-01-04T00:00:00Z")
        trade_a.exit_timestamp;
      Alcotest.(check (float 1e-10))
        "A entry_price" prices_a.(2) trade_a.entry_price;
      Alcotest.(check (float 1e-10))
        "A exit_price" prices_a.(3) trade_a.exit_price;
      Alcotest.(check int) "A holding_period_bars" 1 trade_a.holding_period_bars;
      Alcotest.(check string) "B instrument" "B" trade_b.instrument;
      Alcotest.(check ptime_testable)
        "B entry_timestamp"
        (ptime_of_date "2024-01-03T00:00:00Z")
        trade_b.entry_timestamp;
      Alcotest.(check ptime_testable)
        "B exit_timestamp"
        (ptime_of_date "2024-01-04T00:00:00Z")
        trade_b.exit_timestamp;
      Alcotest.(check (float 1e-10))
        "B entry_price" prices_b.(2) trade_b.entry_price;
      Alcotest.(check (float 1e-10))
        "B exit_price" prices_b.(3) trade_b.exit_price;
      Alcotest.(check int) "B holding_period_bars" 1 trade_b.holding_period_bars;
      (* Each instrument's P&L in proportion to its own weight, net of its own
         turnover cost. An equal split of the rebalance's total cost, or an
         equal-weight aggregation, moves these and nothing else. *)
      Alcotest.(check (float 1e-10)) "A pnl" pnl_a trade_a.pnl;
      Alcotest.(check (float 1e-10)) "B pnl" pnl_b trade_b.pnl;
      let pnl_sum =
        List.fold_left
          (fun acc (t : Cairos_engine.Trade.t) -> acc +. t.pnl)
          0.0
          (Cairos.Nonempty.to_list result.trades)
      in
      Alcotest.(check (float 1e-10))
        "1 + sum(pnl) = equity.(last)" equity_3 (1.0 +. pnl_sum)

(* === Test 7: mid_series_size_adjustment_segment_sum_pnl ===

   The in-flight trade update — a same-direction size change that adjusts the
   open position rather than closing it — is the one branch of the pinned
   trade state machine no Layer 1 test reaches. Tests 1, 3, 4 and 6 only ever
   incept and force-close; Tests 2 and 5 flip. This test drives the fourth
   branch and pins the three rules that govern it: the trade count does not
   increment, the entry fields stay at the *original* inception, and the
   recorded [pnl] is the sum of both constant-weight segments net of the
   mid-holding cost.

   Setup:
     dates       = [2024-01-01 .. 2024-01-06]
     prices      = [100.0; 100.0; 125.0; 125.0; 156.25; 195.3125]
     signals     = [0.0; 0.5; 0.5; 1.0; 1.0; 1.0]
     rebalance   = [t=1; t=3] (2024-01-02, 2024-01-04)
     commission  = 0.001
     slippage    = 0.0005   (c = 0.0015)

   Weight timeline. A rebalance at bar [T] sets the weight in force from bar
   [T] onward, and mark-to-market at bar [t] applies the weight held at
   [t-1]. So [w = 0.5] drives the marks at bars 2 and 3, and [w = 1.0] drives
   bars 4 and 5. The constant-weight segments are therefore [i_0=1, i_1=3) at 0.5 and
   [i_1=3, i_R=5) at 1.0, with [i_R] the last bar because the resolution is a
   force-close.

   Why the price is flat between bars 2 and 3. This is the scenario Test 4's
   closing note routes the segment-versus-accrual question to, so it is built
   so the question does not have to be answered by fiat. Within one segment
   the two forms agree exactly when either the weight is [1.0] (the portfolio
   return then *is* the instrument's return, so the running NAV and the
   segment-entry NAV compound identically) or the segment carries a single
   non-trivial mark-to-market step. Segment 2 satisfies the first condition by
   being fully invested; segment 1 is given the second by [price.(3) =
   price.(2)]. Every number below is what the segment sum and the engine's
   bar-by-bar accrual both produce, so the assertion does not pick a side.
   A 0.5-weighted segment spanning two live steps would force that choice; it
   is not what this test builds, and the divergence stays recorded in Test 4's
   note rather than being silently resolved here.

   Derivation:
     - [t=0]: not a rebalance, zero weights. equity.(0) = 1.0.
     - [t=1]: rebalance T=1. Pre-cost NAV = 1.0 (zero weights, MTM no-op).
         weight_delta = 0.5 -. 0.0 = 0.5
         cost1 = 0.0015 *. 0.5 *. 1.0 = 0.00075
         equity.(1) = 1.0 -. 0.00075 = 0.99925; current_weights := 0.5
       Inception at T+1 = bar 2: entry_timestamp = 2024-01-03,
       entry_price = price.(2) = 125.0, entry_bar = 2.
     - [t=2]: not a rebalance. w = 0.5, r = 125.0/100.0 -. 1.0 = 0.25.
         equity.(2) = 0.99925 *. (1.0 +. 0.5 *. 0.25)
                    = 0.99925 *. 1.125 = 1.12415625
     - [t=3]: mark to market first, at the still-held 0.5:
         r = 125.0/125.0 -. 1.0 = 0.0, so the pre-cost NAV is 1.12415625.
       Then rebalance T=3, target 1.0 against a held 0.5 — same sign, neither
       zero, so this is a size adjustment:
         weight_delta = 1.0 -. 0.5 = 0.5
         cost2 = 0.0015 *. 0.5 *. 1.12415625 = 0.0008431171875
         equity.(3) = 1.12415625 -. 0.0008431171875 = 1.1233131328125
         current_weights := 1.0
       No trade is emitted and no trade is resolved; the in-flight record
       absorbs cost2 and keeps its bar-2 entry fields. The execution price at
       T+1 = bar 4 is 156.25 — a bug that re-based [entry_price] on the
       adjustment would report that instead of 125.0.
     - [t=4]: not a rebalance. w = 1.0, r = 156.25/125.0 -. 1.0 = 0.25.
         equity.(4) = 1.1233131328125 *. 1.25 = 1.404141416015625
     - [t=5]: not a rebalance. w = 1.0, r = 195.3125/156.25 -. 1.0 = 0.25.
         equity.(5) = 1.404141416015625 *. 1.25 = 1.75517677001953125

   Trade record (one trade, force-closed at [t=5] = last bar, no exit cost):
     entry_timestamp = 2024-01-03 (bar 2, the *original* inception)
     exit_timestamp  = 2024-01-06 (bar 5)
     entry_price = 125.0; exit_price = 195.3125
     holding_period_bars = 5 -. 2 = 3 (not subdivided by the adjustment)
     segment 1 = 0.5 *. equity.(1) *. (price.(3)/price.(1) -. 1.0)
               = 0.5 *. 0.99925 *. 0.25 = 0.12490625
     segment 2 = 1.0 *. equity.(3) *. (price.(5)/price.(3) -. 1.0)
               = 1.1233131328125 *. 0.5625 = 0.63186363720703125
     pnl = 0.12490625 +. 0.63186363720703125 -. 0.00075 -. 0.0008431171875
         = 0.75517677001953125

   Cross-check: [1.0 +. 0.75517677001953125 = 1.75517677001953125] =
   equity.(5). Note the identity alone does not pin this test — it holds for
   any [pnl] equal to [equity.(5) -. 1.0], including one computed from a
   single 0.5-weighted or single 1.0-weighted segment over the whole span. The
   two-segment sum is what the segment assertions below state.

   Red-first (by rival-hypothesis mutation, per this file's head comment):
   dropping [cost2] from the expected [pnl] reddens [pnl] alone, 0.75602
   against 0.755177 — the equity curve is unaffected because the engine
   deducts that cost from NAV either way, so only the *attribution* to the
   trade is in question; expecting a single segment
   [0.5 *. equity.(1) *. (price.(5)/price.(1) -. 1.0)] over the whole span
   reddens [pnl], 0.474612 against 0.755177; expecting [entry_price =
   price.(4)] — the adjustment's execution price — reddens [entry_price],
   156.25 against 125; expecting a trade count of 2 reddens the count. Each
   was observed failing on its own and reverted. *)

let mid_series_size_adjustment_segment_sum_pnl () =
  let dates =
    [|
      "2024-01-01";
      "2024-01-02";
      "2024-01-03";
      "2024-01-04";
      "2024-01-05";
      "2024-01-06";
    |]
  in
  let prices = [| 100.0; 100.0; 125.0; 125.0; 156.25; 195.3125 |] in
  let w_entry = 0.5 in
  let w_adjusted = 1.0 in
  let signals =
    [| 0.0; w_entry; w_entry; w_adjusted; w_adjusted; w_adjusted |]
  in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame = make_frame [ ("A", make_daily_series dates signals) ] in
  let rebalance_index = make_daily_index [| "2024-01-02"; "2024-01-04" |] in
  let commission = 0.001 in
  let slippage = 0.0005 in
  let c = commission +. slippage in
  let mtm w nav i =
    nav *. (1.0 +. (w *. ((prices.(i) /. prices.(i - 1)) -. 1.0)))
  in
  (* Rebalance 1 at [t=1]: zero weights make the mark-to-market step a no-op,
     so the pre-cost NAV is 1.0. *)
  let nav_reb1 = 1.0 in
  let cost1 = c *. Float.abs (w_entry -. 0.0) *. nav_reb1 in
  let equity_1 = nav_reb1 -. cost1 in
  let equity_2 = mtm w_entry equity_1 2 in
  (* Rebalance 2 at [t=3]: mark to market at the still-held [w_entry] first;
     the size adjustment's cost is charged against that post-MTM,
     pre-deduction NAV. *)
  let nav_reb2 = mtm w_entry equity_2 3 in
  let cost2 = c *. Float.abs (w_adjusted -. w_entry) *. nav_reb2 in
  let equity_3 = nav_reb2 -. cost2 in
  let equity_4 = mtm w_adjusted equity_3 4 in
  let equity_5 = mtm w_adjusted equity_4 5 in
  let expected_equity =
    [| 1.0; equity_1; equity_2; equity_3; equity_4; equity_5 |]
  in
  (* The two constant-weight segments, over rebalance-bar endpoints: bar 1 to
     bar 3 at [w_entry], bar 3 to the last bar at [w_adjusted]. *)
  let segment_1_pnl =
    w_entry *. equity_1 *. ((prices.(3) /. prices.(1)) -. 1.0)
  in
  let segment_2_pnl =
    w_adjusted *. equity_3 *. ((prices.(5) /. prices.(3)) -. 1.0)
  in
  let expected_pnl = segment_1_pnl +. segment_2_pnl -. cost1 -. cost2 in
  match
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  with
  | Error e -> Alcotest.fail (Cairos_engine.Backtest.err_to_string e)
  | Ok result ->
      let equity_actual =
        Nx.to_array (Cairos.Series.values result.equity_curve)
      in
      check_float_array_close ~tol:1e-10 ~msg:"equity_curve" expected_equity
        equity_actual;
      check_float_array_close ~tol:1e-10 ~msg:"weights[A]"
        [| 0.0; w_entry; w_entry; w_adjusted; w_adjusted; w_adjusted |]
        (frame_get_values "A" result.weights);
      (* The adjustment updates the in-flight record; it neither resolves it
         nor incepts a second one. *)
      Alcotest.(check int)
        "trade count (size adjustment emits no trade)" 1
        (Cairos.Nonempty.length result.trades);
      let trade = Cairos.Nonempty.hd result.trades in
      Alcotest.(check string) "instrument" "A" trade.instrument;
      (* Entry fields stay at the original inception, not the adjustment's
         execution bar (bar 4, price 156.25). *)
      Alcotest.(check ptime_testable)
        "entry_timestamp (original inception)"
        (ptime_of_date "2024-01-03T00:00:00Z")
        trade.entry_timestamp;
      Alcotest.(check (float 1e-10))
        "entry_price (original inception)" prices.(2) trade.entry_price;
      Alcotest.(check ptime_testable)
        "exit_timestamp"
        (ptime_of_date "2024-01-06T00:00:00Z")
        trade.exit_timestamp;
      Alcotest.(check (float 1e-10))
        "exit_price (force-close at last close)" prices.(5) trade.exit_price;
      Alcotest.(check int)
        "holding_period_bars (not subdivided by the adjustment)" 3
        trade.holding_period_bars;
      Alcotest.(check (float 1e-10))
        "pnl (both segments, net of the mid-holding cost)" expected_pnl
        trade.pnl;
      Alcotest.(check (float 1e-10))
        "1 + sum(pnl) = equity.(last)" equity_5 (1.0 +. trade.pnl)

(* === Test 8: exit_to_flat_resolves_trade_at_execution_bar ===

   Every other test in this file, every [test_invariants.ml] property, and
   all three [validate-oracle] scenarios close their positions the same way:
   end-of-backtest force-close. None of them ever takes a target weight back
   to [0.0] while a position is open, so the loop's *resolution* branch —
   [weight_delta <> 0.0] with [target = 0.0] — runs in no test in the
   repository. Established by mutation, not by reading: dropping the closing
   cost from that branch ([pnl = pnl_acc -. cost_j] to [pnl = pnl_acc]) left
   [dune runtest --force] at exit 0.

   The branch differs from force-close in four observable ways at once — the
   exit timestamp, the exit price, the holding period, and whether the
   closing cost reaches the trade — so a single exit-to-flat case pins all
   four.

   Setup:
     dates       = [2024-01-01 .. 2024-01-06]
     prices      = [100; 100; 102; 104.04; 106.1208; 108.243216]
       — flat from t=0 to t=1 (zero weights, no MTM drift), then +2% per bar.
     signals     = [0.0; 1.0; 1.0; 0.0; 0.0; 0.0]
     rebalance   = [t=1; t=3]
     commission  = 0.001
     slippage    = 0.0005   (c = 0.0015)

   Derivation:
     - [t=0]: not a rebalance, zero weights. equity.(0) = 1.0.
     - [t=1]: rebalance. MTM at zero weights is a no-op, so the pre-cost NAV
         is 1.0. [dw = 1.0 -. 0.0], cost1 = 0.0015. Inception executes at
         [T+1 = 2]: entry_price = price.(2) = 102.0.
         equity.(1) = 1.0 -. cost1. weights := 1.0.
     - [t=2]: not a rebalance. MTM at w = 1.0.
     - [t=3]: rebalance to [target = 0.0]. MTM at the still-held w = 1.0
         first; [dw = 0.0 -. 1.0 = -1.0], so cost2 is charged on full
         turnover against that post-MTM NAV. The position *resolves* here:
         the trade closes at [T+1 = 4], not at the last bar.
         equity.(3) = nav_after_mtm -. cost2. weights := 0.0.
     - [t=4], [t=5]: weights are 0.0, so MTM is a no-op and the equity curve
         is flat at equity.(3) even though prices keep rising 2% per bar.
         That flatness is the pin that the exit really happened.

   Trade record (resolution at [t=3], executed at bar 4):
     - entry_timestamp = price_index.(2) = 2024-01-03.
     - exit_timestamp  = price_index.(4) = 2024-01-05 — NOT the last bar.
     - entry_price     = price.(2) = 102.0.
     - exit_price      = price.(4) = 106.1208 — NOT price.(5).
     - holding_period_bars = 4 - 2 = 2 — NOT 3.
     - pnl = equity.(3) -. 1.0, by the single-trade equity-trade identity.
       The identity holds only if cost2 is charged to the closing trade as
       well as deducted from NAV; omitting it leaves pnl high by cost2.

   Red-first (by rival-hypothesis mutation, per this file's head comment):
   under the force-close hypothesis — the engine reaching the last bar with
   the position still open — [exit_timestamp] reddens (2024-01-06 against
   2024-01-05), [exit_price] reddens (108.243216 against 106.1208), and
   [holding_period_bars] reddens (3 against 2). Under the
   cost-not-attributed hypothesis, [pnl] alone reddens while the equity
   curve stays green, because NAV is debited either way and only the
   attribution to the trade is in question. Each was observed failing on its
   own and reverted. *)

let exit_to_flat_resolves_trade_at_execution_bar () =
  let dates =
    [|
      "2024-01-01";
      "2024-01-02";
      "2024-01-03";
      "2024-01-04";
      "2024-01-05";
      "2024-01-06";
    |]
  in
  let prices = [| 100.0; 100.0; 102.0; 104.04; 106.1208; 108.243216 |] in
  let w_entry = 1.0 in
  let signals = [| 0.0; w_entry; w_entry; 0.0; 0.0; 0.0 |] in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame = make_frame [ ("A", make_daily_series dates signals) ] in
  let rebalance_index = make_daily_index [| "2024-01-02"; "2024-01-04" |] in
  let commission = 0.001 in
  let slippage = 0.0005 in
  let c = commission +. slippage in
  let mtm w nav i =
    nav *. (1.0 +. (w *. ((prices.(i) /. prices.(i - 1)) -. 1.0)))
  in
  (* Rebalance 1 at [t=1]: zero weights make the mark-to-market step a no-op,
     so the pre-cost NAV is 1.0. *)
  let cost1 = c *. Float.abs (w_entry -. 0.0) *. 1.0 in
  let equity_1 = 1.0 -. cost1 in
  let equity_2 = mtm w_entry equity_1 2 in
  (* Rebalance 2 at [t=3]: mark to market at the still-held [w_entry] first;
     the exit's cost is charged on full turnover against that post-MTM,
     pre-deduction NAV. *)
  let nav_reb2 = mtm w_entry equity_2 3 in
  let cost2 = c *. Float.abs (0.0 -. w_entry) *. nav_reb2 in
  let equity_3 = nav_reb2 -. cost2 in
  (* Flat after the exit: prices keep rising, NAV does not. *)
  let expected_equity =
    [| 1.0; equity_1; equity_2; equity_3; equity_3; equity_3 |]
  in
  match
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  with
  | Error e -> Alcotest.fail (Cairos_engine.Backtest.err_to_string e)
  | Ok result ->
      let equity_actual =
        Nx.to_array (Cairos.Series.values result.equity_curve)
      in
      check_float_array_close ~tol:1e-10 ~msg:"equity_curve" expected_equity
        equity_actual;
      check_float_array_close ~tol:1e-10 ~msg:"weights[A]"
        [| 0.0; w_entry; w_entry; 0.0; 0.0; 0.0 |]
        (frame_get_values "A" result.weights);
      Alcotest.(check int)
        "trade count (resolution emits the only trade)" 1
        (Cairos.Nonempty.length result.trades);
      let trade = Cairos.Nonempty.hd result.trades in
      Alcotest.(check string) "instrument" "A" trade.instrument;
      Alcotest.(check ptime_testable)
        "entry_timestamp"
        (ptime_of_date "2024-01-03T00:00:00Z")
        trade.entry_timestamp;
      Alcotest.(check (float 1e-10)) "entry_price" prices.(2) trade.entry_price;
      Alcotest.(check ptime_testable)
        "exit_timestamp (resolution's T+1, not the last bar)"
        (ptime_of_date "2024-01-05T00:00:00Z")
        trade.exit_timestamp;
      Alcotest.(check (float 1e-10))
        "exit_price (resolution's execution price, not the last close)"
        prices.(4) trade.exit_price;
      Alcotest.(check int)
        "holding_period_bars (ends at the exit bar, not the last bar)" 2
        trade.holding_period_bars;
      Alcotest.(check (float 1e-10))
        "pnl (net of both the entry and the exit cost)" (equity_3 -. 1.0)
        trade.pnl

(* === Test 9: leading_all_zero_rebalance_is_inert ===

   Every other test in this file, every [test_invariants.ml] property, and
   all three [validate-oracle] scenarios carry a non-zero target weight at
   their *first* rebalance date, so no test in the repository has a
   rebalance bar that precedes the first non-zero target. Established by
   mutation, not by reading: replacing the pre-first-trade rebalance charge
   with [current_nav := Float.nan] left [dune runtest --force] at exit 0.

   That stretch of bars is where the loop holds nothing and can hold
   nothing, and it is walked separately from the general body so the first
   trade can be opened by straight-line code. A leading all-zero rebalance
   is the input that makes the stretch non-trivial: it is a rebalance the
   loop must visit and must leave inert.

   Setup:
     dates       = [2024-01-01 .. 2024-01-05]
     prices      = [100; 100; 100; 102; 104.04]
       — flat through t=2 (nothing is held there), then +2% per bar.
     signals     = [0.0; 0.0; 1.0; 1.0; 1.0]
     rebalance   = [t=1; t=2]
     commission  = 0.001
     slippage    = 0.0005   (c = 0.0015)

   Derivation:
     - [t=0]: not a rebalance, nothing held. equity.(0) = 1.0.
     - [t=1]: rebalance, but every target is 0.0, so [dw = 0.0], the charge
         is [c *. 0.0 *. 1.0 = 0.0] and no position opens.
         equity.(1) = 1.0 — exactly, not to tolerance. weights := 0.0.
     - [t=2]: rebalance to [target = 1.0]. Nothing is held, so the
         mark-to-market step is a no-op and the pre-cost NAV is still 1.0.
         cost = c *. 1.0 *. 1.0. Inception executes at [T+1 = 3].
         equity.(2) = 1.0 -. cost. weights := 1.0.
     - [t=3], [t=4]: not rebalances. MTM at w = 1.0 compounds +2% per bar.

   Trade record (force-closed at the last bar):
     - entry_timestamp = price_index.(3) = 2024-01-04 — the T+1 of the
       *second* rebalance, not the first.
     - entry_price = price.(3) = 102.0.
     - holding_period_bars = 4 - 3 = 1.
     - pnl = equity.(4) -. 1.0, by the single-trade equity-trade identity.

   Red-first (by rival-hypothesis mutation, per this file's head comment):
   under the hypothesis that the pre-first-trade stretch is not walked
   faithfully ([current_nav := Float.nan] at its rebalance), [equity_curve]
   reddens at index 1. Under the hypothesis that the first trade opens at
   the first *rebalance* rather than at the first non-zero target (the
   witness selected without regard to the targets), the loop opens a
   zero-weight position at [t=1] and the [t=2] rebalance closes it as a
   sign flip: the trade count reddens (2 against 1), as do
   [entry_timestamp] and [entry_price]. Each was observed failing on its
   own and reverted. *)

let leading_all_zero_rebalance_is_inert () =
  let dates =
    [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
  in
  let prices = [| 100.0; 100.0; 100.0; 102.0; 104.04 |] in
  let w_entry = 1.0 in
  let signals = [| 0.0; 0.0; w_entry; w_entry; w_entry |] in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame = make_frame [ ("A", make_daily_series dates signals) ] in
  let rebalance_index = make_daily_index [| "2024-01-02"; "2024-01-03" |] in
  let commission = 0.001 in
  let slippage = 0.0005 in
  let c = commission +. slippage in
  let mtm w nav i =
    nav *. (1.0 +. (w *. ((prices.(i) /. prices.(i - 1)) -. 1.0)))
  in
  (* The [t=1] rebalance moves nothing: zero turnover, zero charge. *)
  let cost = c *. Float.abs (w_entry -. 0.0) *. 1.0 in
  let equity_2 = 1.0 -. cost in
  let equity_3 = mtm w_entry equity_2 3 in
  let equity_4 = mtm w_entry equity_3 4 in
  let expected_equity = [| 1.0; 1.0; equity_2; equity_3; equity_4 |] in
  match
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  with
  | Error e -> Alcotest.fail (Cairos_engine.Backtest.err_to_string e)
  | Ok result ->
      let equity_actual =
        Nx.to_array (Cairos.Series.values result.equity_curve)
      in
      check_float_array_close ~tol:1e-10 ~msg:"equity_curve" expected_equity
        equity_actual;
      check_float_array_close ~tol:1e-10 ~msg:"weights[A]"
        [| 0.0; 0.0; w_entry; w_entry; w_entry |]
        (frame_get_values "A" result.weights);
      Alcotest.(check int)
        "trade count (the all-zero rebalance opens nothing)" 1
        (Cairos.Nonempty.length result.trades);
      let trade = Cairos.Nonempty.hd result.trades in
      Alcotest.(check string) "instrument" "A" trade.instrument;
      Alcotest.(check ptime_testable)
        "entry_timestamp (T+1 of the second rebalance, not the first)"
        (ptime_of_date "2024-01-04T00:00:00Z")
        trade.entry_timestamp;
      Alcotest.(check (float 1e-10))
        "entry_price (the second rebalance's execution price)" prices.(3)
        trade.entry_price;
      Alcotest.(check ptime_testable)
        "exit_timestamp (force-close at the last bar)"
        (ptime_of_date "2024-01-05T00:00:00Z")
        trade.exit_timestamp;
      Alcotest.(check (float 1e-10))
        "exit_price (last close)" prices.(4) trade.exit_price;
      Alcotest.(check int) "holding_period_bars" 1 trade.holding_period_bars;
      Alcotest.(check (float 1e-10))
        "pnl (net of the entry cost)" (equity_4 -. 1.0) trade.pnl

(* === Test 10: force_close_order_is_column_order_not_entry_order ===

   The force-close sweep emits one trade per still-open instrument in
   column order. Every multi-instrument fixture in the repository opens
   both instruments at the same rebalance, so column order and entry order
   coincide there and neither pins the other; and every fixture's first
   non-zero target is in column 0, so the sweep's "columns before the first
   traded one" stretch is never non-empty. Established by mutation: dropping
   that stretch left [dune runtest --force] at exit 0, and so did reversing
   the order in which the trades closed during the loop are emitted.

   This fixture separates the two orders. "B" is the only instrument with a
   non-zero target at the first rebalance, so it is the first to open and
   the one the loop's first trade belongs to; "A" opens a rebalance later
   and is emitted *before* it, because "A" is the earlier column.

   Setup:
     dates       = [2024-01-01 .. 2024-01-05]
     prices      = A and B both flat at 100 through every bar — the trades
                   are pure cost, so the ordering assertions cannot be
                   satisfied accidentally by a price effect.
     signals     = A: [0; 0; 1; 1; 1]      B: [0; 1; 1; 1; 1]
     rebalance   = [t=1; t=2]
     commission  = 0.001
     slippage    = 0.0005   (c = 0.0015)

   Derivation:
     - [t=0]: nothing held. equity.(0) = 1.0.
     - [t=1]: rebalance. [dw_A = 0.0], [dw_B = 1.0], so only B is charged:
         cost_B = c *. 1.0 *. 1.0. equity.(1) = 1.0 -. cost_B. B executes
         at [T+1 = 2].
     - [t=2]: rebalance. Prices are flat, so the mark-to-market step is a
         no-op and the pre-cost NAV is equity.(1). [dw_A = 1.0],
         [dw_B = 0.0]: cost_A = c *. 1.0 *. equity.(1), B is not charged
         again. equity.(2) = equity.(1) -. cost_A. A executes at [T+1 = 3].
     - [t=3], [t=4]: flat prices, so the equity curve holds at equity.(2).

   Trades (both force-closed at the last bar, in column order):
     - [0] = A: entry 2024-01-04, holding 1 bar, pnl = -.cost_A.
     - [1] = B: entry 2024-01-03, holding 2 bars, pnl = -.cost_B.
     The entry timestamps are decreasing across the list; that inversion is
     the pin.

   Red-first (by rival-hypothesis mutation, per this file's head comment):
   under the hypothesis that the sweep drops the columns before the first
   traded one, the trade count reddens (1 against 2). Under the hypothesis
   that it emits them in the other order — entry order, or the accumulator's
   own newest-first order left unreversed — the instrument, entry-timestamp,
   holding-period and pnl assertions all redden as a block. Each was
   observed failing on its own and reverted. *)

let force_close_order_is_column_order_not_entry_order () =
  let dates =
    [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
  in
  let flat_prices = [| 100.0; 100.0; 100.0; 100.0; 100.0 |] in
  let w = 1.0 in
  let price_frame =
    make_frame
      [
        ("A", make_daily_series dates flat_prices);
        ("B", make_daily_series dates flat_prices);
      ]
  in
  let signal_frame =
    make_frame
      [
        ("A", make_daily_series dates [| 0.0; 0.0; w; w; w |]);
        ("B", make_daily_series dates [| 0.0; w; w; w; w |]);
      ]
  in
  let rebalance_index = make_daily_index [| "2024-01-02"; "2024-01-03" |] in
  let commission = 0.001 in
  let slippage = 0.0005 in
  let c = commission +. slippage in
  let cost_b = c *. w *. 1.0 in
  let equity_1 = 1.0 -. cost_b in
  let cost_a = c *. w *. equity_1 in
  let equity_2 = equity_1 -. cost_a in
  match
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  with
  | Error e -> Alcotest.fail (Cairos_engine.Backtest.err_to_string e)
  | Ok result -> (
      check_float_array_close ~tol:1e-10 ~msg:"equity_curve"
        [| 1.0; equity_1; equity_2; equity_2; equity_2 |]
        (Nx.to_array (Cairos.Series.values result.equity_curve));
      check_float_array_close ~tol:1e-10 ~msg:"weights[A]"
        [| 0.0; 0.0; w; w; w |]
        (frame_get_values "A" result.weights);
      check_float_array_close ~tol:1e-10 ~msg:"weights[B]" [| 0.0; w; w; w; w |]
        (frame_get_values "B" result.weights);
      Alcotest.(check int)
        "trade count (one force-close per instrument)" 2
        (Cairos.Nonempty.length result.trades);
      match Cairos.Nonempty.to_list result.trades with
      | [ first; second ] ->
          Alcotest.(check string)
            "trades[0] instrument (earlier column, later entry)" "A"
            first.instrument;
          Alcotest.(check ptime_testable)
            "trades[0] entry_timestamp"
            (ptime_of_date "2024-01-04T00:00:00Z")
            first.entry_timestamp;
          Alcotest.(check int)
            "trades[0] holding_period_bars" 1 first.holding_period_bars;
          Alcotest.(check (float 1e-10))
            "trades[0] pnl (entry cost only)" (-.cost_a) first.pnl;
          Alcotest.(check string)
            "trades[1] instrument (later column, earlier entry)" "B"
            second.instrument;
          Alcotest.(check ptime_testable)
            "trades[1] entry_timestamp"
            (ptime_of_date "2024-01-03T00:00:00Z")
            second.entry_timestamp;
          Alcotest.(check int)
            "trades[1] holding_period_bars" 2 second.holding_period_bars;
          Alcotest.(check (float 1e-10))
            "trades[1] pnl (entry cost only)" (-.cost_b) second.pnl
      | ts ->
          Alcotest.failf "expected exactly two trades, got %d" (List.length ts))

(* === Test 11: consecutive_round_trips_emit_trades_in_close_order ===

   No fixture in the repository closes two trades *during* the loop: every
   multi-trade run in this file closes one by resolution or sign flip and
   the rest by end-of-backtest force close. So the order in which the loop's
   own closes accumulate relative to each other is pinned by nothing —
   established by mutation: leaving the accumulator's first trade at its
   head instead of the most recent one left [dune runtest --force] at exit
   0, and no test observed a difference.

   Two round trips on one instrument, both resolved before the last bar, is
   the smallest input that separates them. Prices are flat throughout, so
   each trade's pnl is exactly its two cost charges and no price effect can
   make a mis-ordered pair look right.

   Setup:
     dates       = [2024-01-01 .. 2024-01-06]
     prices      = flat at 100.
     signals     = [0; 1; 0; 1; 0; 0]
     rebalance   = [t=1; t=2; t=3; t=4]
     commission  = 0.001
     slippage    = 0.0005   (c = 0.0015)

   Derivation — flat prices make every mark-to-market step a no-op, so NAV
   moves only by the charge [c *. |dw| *. nav], and every [|dw|] here is
   1.0:
     - [t=0]: nothing held. equity.(0) = 1.0.
     - [t=1]: open. equity.(1) = 1.0 *. (1 -. c). Executes at bar 2.
     - [t=2]: close. equity.(2) = equity.(1) *. (1 -. c). Trade 1 resolves
         at bar 3, carrying both charges: pnl = equity.(2) -. 1.0.
     - [t=3]: open again. equity.(3) = equity.(2) *. (1 -. c). Executes at
         bar 4.
     - [t=4]: close. equity.(4) = equity.(3) *. (1 -. c). Trade 2 resolves
         at bar 5, carrying both of its charges:
         pnl = equity.(4) -. equity.(2).
     - [t=5]: nothing held. equity.(5) = equity.(4).

   Both trades close inside the loop and nothing is open at the end, so the
   force-close sweep contributes nothing and the list is the loop's own
   closes alone, oldest first.

   Red-first (by rival-hypothesis mutation, per this file's head comment):
   under the hypothesis that the accumulator is not ordered newest-first —
   or is emitted without being reversed — [trades[0] entry_timestamp] and
   [trades[1] entry_timestamp] redden as a pair while the equity curve and
   the trade count stay green, because only the order is in question. This
   was observed failing and reverted. *)

let consecutive_round_trips_emit_trades_in_close_order () =
  let dates =
    [|
      "2024-01-01";
      "2024-01-02";
      "2024-01-03";
      "2024-01-04";
      "2024-01-05";
      "2024-01-06";
    |]
  in
  let flat_prices = [| 100.0; 100.0; 100.0; 100.0; 100.0; 100.0 |] in
  let w = 1.0 in
  let price_frame = make_frame [ ("A", make_daily_series dates flat_prices) ] in
  let signal_frame =
    make_frame [ ("A", make_daily_series dates [| 0.0; w; 0.0; w; 0.0; 0.0 |]) ]
  in
  let rebalance_index =
    make_daily_index
      [| "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
  in
  let commission = 0.001 in
  let slippage = 0.0005 in
  let c = commission +. slippage in
  let equity_1 = 1.0 *. (1.0 -. c) in
  let equity_2 = equity_1 *. (1.0 -. c) in
  let equity_3 = equity_2 *. (1.0 -. c) in
  let equity_4 = equity_3 *. (1.0 -. c) in
  match
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  with
  | Error e -> Alcotest.fail (Cairos_engine.Backtest.err_to_string e)
  | Ok result -> (
      check_float_array_close ~tol:1e-10 ~msg:"equity_curve"
        [| 1.0; equity_1; equity_2; equity_3; equity_4; equity_4 |]
        (Nx.to_array (Cairos.Series.values result.equity_curve));
      Alcotest.(check int)
        "trade count (one per round trip)" 2
        (Cairos.Nonempty.length result.trades);
      match Cairos.Nonempty.to_list result.trades with
      | [ first; second ] ->
          Alcotest.(check ptime_testable)
            "trades[0] entry_timestamp (the earlier round trip)"
            (ptime_of_date "2024-01-03T00:00:00Z")
            first.entry_timestamp;
          Alcotest.(check ptime_testable)
            "trades[0] exit_timestamp"
            (ptime_of_date "2024-01-04T00:00:00Z")
            first.exit_timestamp;
          Alcotest.(check (float 1e-10))
            "trades[0] pnl (both of its charges)" (equity_2 -. 1.0) first.pnl;
          Alcotest.(check ptime_testable)
            "trades[1] entry_timestamp (the later round trip)"
            (ptime_of_date "2024-01-05T00:00:00Z")
            second.entry_timestamp;
          Alcotest.(check ptime_testable)
            "trades[1] exit_timestamp"
            (ptime_of_date "2024-01-06T00:00:00Z")
            second.exit_timestamp;
          Alcotest.(check (float 1e-10))
            "trades[1] pnl (both of its charges)" (equity_4 -. equity_2)
            second.pnl
      | ts ->
          Alcotest.failf "expected exactly two trades, got %d" (List.length ts))

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
          Alcotest.test_case "short_only_equity_curve_matches_analytical" `Quick
            short_only_equity_curve_matches_analytical;
          Alcotest.test_case "sign_flip_costs_charged_on_both_legs" `Quick
            sign_flip_costs_charged_on_both_legs;
          Alcotest.test_case "two_instruments_unequal_weights_known_pnl" `Quick
            two_instruments_unequal_weights_known_pnl;
          Alcotest.test_case "mid_series_size_adjustment_segment_sum_pnl" `Quick
            mid_series_size_adjustment_segment_sum_pnl;
          Alcotest.test_case "exit_to_flat_resolves_trade_at_execution_bar"
            `Quick exit_to_flat_resolves_trade_at_execution_bar;
          Alcotest.test_case "leading_all_zero_rebalance_is_inert" `Quick
            leading_all_zero_rebalance_is_inert;
          Alcotest.test_case "force_close_order_is_column_order_not_entry_order"
            `Quick force_close_order_is_column_order_not_entry_order;
          Alcotest.test_case
            "consecutive_round_trips_emit_trades_in_close_order" `Quick
            consecutive_round_trips_emit_trades_in_close_order;
        ] );
    ]
