(* In-process unit tests for [Cairos_finance] metrics.

   These cover edge cases and frequency-dispatch behaviour that the
   fixture-based cross-validation suite (cross_validate.ml) does not exercise:
   empty / all-NaN inputs, mid-series NaN, and the Hour/Minute/Week branches
   of [annualization_factor] which the daily-only fixtures cannot reach. *)

open Finance_test_helpers

(* === cumulative_return === *)

let cum_empty () =
  let s = make_daily_series [||] in
  Alcotest.(check (float 0.0))
    "empty series → 0.0 (empty product)" 0.0
    (Cairos_finance.cumulative_return s)

let cum_all_nan () =
  let s = make_daily_series [| Float.nan; Float.nan; Float.nan |] in
  Alcotest.(check (float 0.0))
    "all-NaN → 0.0" 0.0
    (Cairos_finance.cumulative_return s)

let cum_skips_leading_nan () =
  (* Mimics the [pct_change] output shape: leading NaN, real returns after.
     Expected: (1 + 0.02) * (1 + -0.01) - 1 = 0.0098 *)
  let s = make_daily_series [| Float.nan; 0.02; -0.01 |] in
  let expected = (1.02 *. 0.99) -. 1.0 in
  Alcotest.(check (float 1e-12))
    "leading NaN is skipped" expected
    (Cairos_finance.cumulative_return s)

let cum_skips_mid_nan () =
  (* Independent NaN-skip check: NaN in the middle of valid returns.
     Expected: (1 + 0.1) * (1 + 0.05) - 1 = 0.155 *)
  let s = make_daily_series [| 0.1; Float.nan; 0.05 |] in
  Alcotest.(check (float 1e-12))
    "mid-series NaN is skipped" 0.155
    (Cairos_finance.cumulative_return s)

(* === annualised_return — degenerate inputs === *)

let ann_empty_is_nan () =
  let s = make_daily_series [||] in
  Alcotest.(check bool)
    "empty → nan" true
    (Float.is_nan (Cairos_finance.annualised_return s))

let ann_all_nan_is_nan () =
  let s = make_daily_series [| Float.nan; Float.nan |] in
  Alcotest.(check bool)
    "all-NaN → nan" true
    (Float.is_nan (Cairos_finance.annualised_return s))

let ann_skips_mid_nan () =
  (* With [0.1; nan; 0.05]: cum_ret = 0.155, n = 2, factor = 252.0.
     Reproduce the function's exact computation order so the result is
     bit-identical (tolerance 0.0). Using a literal [1.155] would diverge
     by a few ULPs, which dwarfs any reasonable absolute tolerance once
     raised to the 126th power. *)
  let s = make_daily_series [| 0.1; Float.nan; 0.05 |] in
  let cum_ret = ((1.0 +. 0.1) *. (1.0 +. 0.05)) -. 1.0 in
  let expected = ((1.0 +. cum_ret) ** (252.0 /. 2.0)) -. 1.0 in
  Alcotest.(check (float 0.0))
    "mid-series NaN excluded from both product and n" expected
    (Cairos_finance.annualised_return s)

(* === annualised_return — frequency dispatch ===

   These four tests collectively exercise every branch of
   [annualization_factor]. The same returns array is fed at four
   frequencies; the function uses [Cairos.Index.freq] to look up the
   per-frequency factor, so each test asserts a different expected value.

   Tolerance is [0.0]: the function and the expected value compute the
   same [Float.pow] formula, so the results are bit-identical. If the
   factor were wrong, the difference would be huge — not a ULP. *)

let single_return = [| Float.nan; 0.001 |]
let expected_ann ~factor = ((1.0 +. 0.001) ** (factor /. 1.0)) -. 1.0

let ann_factor_daily () =
  let s = make_series (daily_index_of_length 2) single_return in
  Alcotest.(check (float 0.0))
    "Day → factor 252.0"
    (expected_ann ~factor:252.0)
    (Cairos_finance.annualised_return s)

let ann_factor_hourly () =
  let s = make_series (hourly_index_of_length 2) single_return in
  Alcotest.(check (float 0.0))
    "Hour → factor 1638.0"
    (expected_ann ~factor:1638.0)
    (Cairos_finance.annualised_return s)

let ann_factor_minute () =
  let s = make_series (minute_index_of_length 2) single_return in
  Alcotest.(check (float 0.0))
    "Minute → factor 98280.0"
    (expected_ann ~factor:98280.0)
    (Cairos_finance.annualised_return s)

let ann_factor_weekly () =
  let s = make_series (weekly_index_of_length 2) single_return in
  Alcotest.(check (float 0.0))
    "Week → factor 52.0"
    (expected_ann ~factor:52.0)
    (Cairos_finance.annualised_return s)

(* === annualised_vol — degenerate inputs === *)

let vol_empty_is_nan () =
  let s = make_daily_series [||] in
  Alcotest.(check bool)
    "empty → nan" true
    (Float.is_nan (Cairos_finance.annualised_vol s))

let vol_all_nan_is_nan () =
  let s = make_daily_series [| Float.nan; Float.nan; Float.nan |] in
  Alcotest.(check bool)
    "all-NaN → nan" true
    (Float.is_nan (Cairos_finance.annualised_vol s))

let vol_single_non_nan_is_nan () =
  (* n = 1 < 2: sample std is undefined. *)
  let s = make_daily_series [| Float.nan; 0.01 |] in
  Alcotest.(check bool)
    "single non-NaN → nan" true
    (Float.is_nan (Cairos_finance.annualised_vol s))

let vol_skips_mid_nan () =
  (* With [0.01; nan; -0.01]: n=2, mean=0, sample std = sqrt(((0.01)^2 +
     (-0.01)^2) / (2-1)) = sqrt(0.0002). Reproduce the function's exact
     computation order so the result is bit-identical. *)
  let s = make_daily_series [| 0.01; Float.nan; -0.01 |] in
  let std = Float.sqrt 0.0002 in
  let expected = std *. Float.sqrt 252.0 in
  Alcotest.(check (float 1e-15))
    "mid-series NaN excluded from mean and std" expected
    (Cairos_finance.annualised_vol s)

(* === sharpe — degenerate inputs === *)

let sharpe_empty_is_nan () =
  let s = make_daily_series [||] in
  Alcotest.(check bool)
    "empty → nan" true
    (Float.is_nan (Cairos_finance.sharpe ~risk_free:0.0 s))

let sharpe_all_nan_is_nan () =
  let s = make_daily_series [| Float.nan; Float.nan |] in
  Alcotest.(check bool)
    "all-NaN → nan" true
    (Float.is_nan (Cairos_finance.sharpe ~risk_free:0.0 s))

let sharpe_single_non_nan_is_nan () =
  let s = make_daily_series [| Float.nan; 0.01 |] in
  Alcotest.(check bool)
    "single non-NaN → nan (n<2)" true
    (Float.is_nan (Cairos_finance.sharpe ~risk_free:0.0 s))

let sharpe_constant_returns_rf0_is_nan () =
  (* All-zero returns with rf=0 → excess all zero → std=0 → nan
     (0/0 path). Decoupled from the cross-validation flat fixture. *)
  let s = make_daily_series [| 0.0; 0.0; 0.0; 0.0 |] in
  Alcotest.(check bool)
    "constant returns, rf=0 → nan" true
    (Float.is_nan (Cairos_finance.sharpe ~risk_free:0.0 s))

let sharpe_constant_returns_rf_nonzero_is_nan () =
  (* All-zero returns with rf>0 → excess is a non-zero constant → std=0
     but mean ≠ 0. Without the std=0 guard this would be ±inf, not nan.
     This is the branch that would silently regress if the guard were
     dropped. *)
  let s = make_daily_series [| 0.0; 0.0; 0.0; 0.0 |] in
  Alcotest.(check bool)
    "constant returns, rf=0.04 → nan (not ±inf)" true
    (Float.is_nan (Cairos_finance.sharpe ~risk_free:0.04 s))

let sharpe_skips_mid_nan () =
  (* With [0.01; nan; -0.01], rf=0: excess = [0.01; nan; -0.01], n=2,
     mean=0, std=sqrt(0.0002). Sharpe = 0 / std * sqrt(252) = 0. *)
  let s = make_daily_series [| 0.01; Float.nan; -0.01 |] in
  Alcotest.(check (float 1e-15))
    "mid-series NaN excluded" 0.0
    (Cairos_finance.sharpe ~risk_free:0.0 s)

let () =
  Alcotest.run "cairos_finance"
    [
      ( "cumulative_return",
        [
          Alcotest.test_case "empty series → 0.0" `Quick cum_empty;
          Alcotest.test_case "all-NaN → 0.0" `Quick cum_all_nan;
          Alcotest.test_case "skips leading NaN" `Quick cum_skips_leading_nan;
          Alcotest.test_case "skips mid-series NaN" `Quick cum_skips_mid_nan;
        ] );
      ( "annualised_return",
        [
          Alcotest.test_case "empty → nan" `Quick ann_empty_is_nan;
          Alcotest.test_case "all-NaN → nan" `Quick ann_all_nan_is_nan;
          Alcotest.test_case "skips mid-series NaN" `Quick ann_skips_mid_nan;
        ] );
      ( "annualization_factor",
        [
          Alcotest.test_case "Day → 252" `Quick ann_factor_daily;
          Alcotest.test_case "Hour → 1638" `Quick ann_factor_hourly;
          Alcotest.test_case "Minute → 98280" `Quick ann_factor_minute;
          Alcotest.test_case "Week → 52" `Quick ann_factor_weekly;
        ] );
      ( "annualised_vol",
        [
          Alcotest.test_case "empty → nan" `Quick vol_empty_is_nan;
          Alcotest.test_case "all-NaN → nan" `Quick vol_all_nan_is_nan;
          Alcotest.test_case "single non-NaN → nan" `Quick
            vol_single_non_nan_is_nan;
          Alcotest.test_case "skips mid-series NaN" `Quick vol_skips_mid_nan;
        ] );
      ( "sharpe",
        [
          Alcotest.test_case "empty → nan" `Quick sharpe_empty_is_nan;
          Alcotest.test_case "all-NaN → nan" `Quick sharpe_all_nan_is_nan;
          Alcotest.test_case "single non-NaN → nan" `Quick
            sharpe_single_non_nan_is_nan;
          Alcotest.test_case "constant returns, rf=0 → nan" `Quick
            sharpe_constant_returns_rf0_is_nan;
          Alcotest.test_case "constant returns, rf=0.04 → nan (not ±inf)" `Quick
            sharpe_constant_returns_rf_nonzero_is_nan;
          Alcotest.test_case "skips mid-series NaN" `Quick sharpe_skips_mid_nan;
        ] );
    ]
