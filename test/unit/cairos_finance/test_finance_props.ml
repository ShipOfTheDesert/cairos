(* Property suite for [Cairos_finance]. RFC 0046 TP-Finance-CR1..MDD2 (twelve
   properties spanning all six scalar/series metrics).

   Each property runs at [~count:200] per RFC 0046 Test Plan; CI/local
   reproducibility is provided by [Qcheck_gen.pin_seed_from_env] (FR-4).

   Inputs come from [Qcheck_gen.daily_returns_series_arb] unless a closed-form
   invariant requires constructing the input directly (constant-r, all-zero,
   strict-positive). Per Appendix A row 1, [max_drawdown] returns a
   non-negative magnitude (sign flip from the epic phrasing). Per Appendix A
   row 10, the [annualised_vol = 0.0] on a constant series is qualified to
   [n >= 2] because ddof=1 std is undefined for n=1. Sharpe2 substitutes the
   unsound additive-shift compensation from PRD/epic with scale-invariance at
   [~risk_free:0.0] (RFC §G); the geometric per-period [risk_free] conversion
   makes the additive form algebraically wrong, while scale-invariance is
   exact. *)

(* [series_is_non_constant s] is [true] when at least two elements of [s] differ.
   Used as a [QCheck.assume] guard for Sharpe properties whose live path
   requires the empirical sample variance to be > 0; on a constant series
   [Cairos_finance.sharpe] returns [nan] (cairos_finance.mli:71-73). *)
let series_is_non_constant s =
  let arr = Nx.to_array (Cairos.Series.values s) in
  if Array.length arr < 2 then false
  else
    let first = arr.(0) in
    Array.exists (fun x -> x <> first) arr

(* === cumulative_return === *)

(* TP-Finance-CR1 — finite returns produce a finite cumulative_return. The
   generator yields values in [-0.5, 0.5] and length [2, 64]; the worst-case
   product (1.5)^64 is ~6.5e11, well within [Float.max]. Catches a regression
   that introduces a spurious overflow or NaN-corrupting accumulation. *)
let cumulative_return_finite_on_finite_input =
  QCheck.Test.make ~count:200 ~name:"cumulative_return_finite_on_finite_input"
    Qcheck_gen.daily_returns_series_arb (fun s ->
      Float.is_finite (Cairos_finance.cumulative_return s))

(* TP-Finance-CR2 — for an all-zero returns series, [cumulative_return] is
   [0.0] within [~tol:1e-15] (empty / all-zero product is [1.0], minus 1).
   Generator builds the series directly with all zeros; length matches
   [daily_returns_series_arb] so the case distribution is comparable.

   The current implementation accumulates via repeated [( *. )], which is
   bit-exact at zero, so [= 0.0] would also pass. Using [float_approx_equal]
   keeps the property robust if the implementation switches accumulation
   strategy (e.g. log-sum or Kahan summation) in the future. *)
let zero_series_arb =
  let open QCheck in
  let gen =
    let open Gen in
    let* n = int_range 2 64 in
    return
      (Qcheck_gen.make_series_from_floats ~freq:Cairos.Freq.Day
         (Array.make n 0.0))
  in
  make
    ~print:(fun s ->
      Printf.sprintf "<zero series len=%d>" (Cairos.Series.length s))
    gen

let cumulative_return_zero_on_all_zero =
  QCheck.Test.make ~count:200 ~name:"cumulative_return_zero_on_all_zero"
    zero_series_arb (fun s ->
      Qcheck_gen.float_approx_equal ~tol:1e-15
        (Cairos_finance.cumulative_return s)
        0.0)

(* TP-Finance-CR3 — for a constant returns series of length [n] and value [r],
   [cumulative_return s = (1 +. r) ** float_of_int n -. 1.0] within [tol 1e-10].
   Both sides compute via [Float.pow] / repeated multiplication; the relative
   tolerance absorbs the ULP drift between accumulation orders. Catches a
   regression where the product is mis-ordered or a NaN-skip path is wrong. *)
let constant_returns_arb =
  let open QCheck in
  let gen =
    let open Gen in
    let* n = int_range 1 32 in
    let* r = float_range (-0.1) 0.1 in
    return (n, r)
  in
  make ~print:(fun (n, r) -> Printf.sprintf "(n=%d, r=%g)" n r) gen

let cumulative_return_constant_r_closed_form =
  QCheck.Test.make ~count:200 ~name:"cumulative_return_constant_r_closed_form"
    constant_returns_arb (fun (n, r) ->
      let s =
        Qcheck_gen.make_series_from_floats ~freq:Cairos.Freq.Day
          (Array.make n r)
      in
      let actual = Cairos_finance.cumulative_return s in
      let expected = ((1.0 +. r) ** float_of_int n) -. 1.0 in
      Qcheck_gen.float_approx_equal ~tol:1e-10 actual expected)

(* === annualised_return === *)

(* TP-Finance-AR1 — for a non-empty all-finite returns input,
   [annualised_return] is finite. Per cairos_finance.mli:30-31, empty / all-NaN
   yields [nan]; [daily_returns_series_arb] excludes those (length >= 2,
   values via [float_range] are finite). The closed-form is
   [(1 + cum_ret) ** (252.0 /. n) - 1]; with returns in [-0.5, 0.5] and length
   [2, 64], log10 of the magnitude stays under ~50 — well inside Float64
   range. *)
let annualised_return_finite_or_nan_per_contract =
  QCheck.Test.make ~count:200
    ~name:"annualised_return_finite_or_nan_per_contract"
    Qcheck_gen.daily_returns_series_arb (fun s ->
      Float.is_finite (Cairos_finance.annualised_return s))

(* === annualised_vol === *)

(* TP-Finance-AV1 — for a finite returns input of length >= 2,
   [annualised_vol] is finite. ddof=1 std is undefined for n < 2 — the
   generator's length floor (2) covers the contract. *)
let annualised_vol_finite_on_finite_input_n_ge_2 =
  QCheck.Test.make ~count:200
    ~name:"annualised_vol_finite_on_finite_input_n_ge_2"
    Qcheck_gen.daily_returns_series_arb (fun s ->
      Float.is_finite (Cairos_finance.annualised_vol s))

(* TP-Finance-AV2 — Appendix A row 10: for a constant series of length n >= 2,
   ddof=1 std is exactly 0.0, so [annualised_vol] is exactly 0.0. The n=1 nan
   case is the documented contract and is pinned by the deterministic
   Alcotest case [vol_single_non_nan_is_nan] in test_metrics.ml; this property
   pins the n >= 2 path. *)
let constant_n_ge_2_arb =
  let open QCheck in
  let gen =
    let open Gen in
    let* n = int_range 2 32 in
    let* c = float_range (-0.1) 0.1 in
    return
      (Qcheck_gen.make_series_from_floats ~freq:Cairos.Freq.Day (Array.make n c))
  in
  make
    ~print:(fun s ->
      Printf.sprintf "<constant series len=%d>" (Cairos.Series.length s))
    gen

let annualised_vol_zero_on_constant_n_ge_2 =
  QCheck.Test.make ~count:200 ~name:"annualised_vol_zero_on_constant_n_ge_2"
    constant_n_ge_2_arb (fun s -> Cairos_finance.annualised_vol s = 0.0)

(* TP-Finance-AV3 — sample std is shift-invariant, so
   [annualised_vol (s +. c) = annualised_vol s] within [tol 1e-9]. The shift
   is applied at the Nx-tensor level via [Nx.add_s] inside [Series.map] (per
   ~/.claude/solutions/ocaml/cairos-series-map-is-tensor-level.md, [Series.map]
   operates on the values payload as a whole tensor). Catches a regression
   that sneaks the input mean into the std computation. *)
let returns_with_shift_arb =
  let open QCheck in
  let gen =
    let open Gen in
    let* s = get_gen Qcheck_gen.daily_returns_series_arb in
    let* c = float_range (-0.1) 0.1 in
    return (s, c)
  in
  make
    ~print:(fun (s, c) ->
      Printf.sprintf "<returns len=%d shift c=%g>" (Cairos.Series.length s) c)
    gen

let annualised_vol_invariant_under_additive_shift =
  QCheck.Test.make ~count:200
    ~name:"annualised_vol_invariant_under_additive_shift" returns_with_shift_arb
    (fun (s, c) ->
      let shifted = Cairos.Series.map (fun t -> Nx.add_s t c) s in
      let actual = Cairos_finance.annualised_vol shifted in
      let expected = Cairos_finance.annualised_vol s in
      Qcheck_gen.float_approx_equal ~tol:1e-9 actual expected)

(* === sharpe === *)

(* TP-Finance-Sharpe1 — for a non-constant finite returns input of length >= 2,
   [sharpe ~risk_free:0.0] is finite. The constant case (std = 0) returns
   [nan] per cairos_finance.mli:71-73 and is pinned by the deterministic
   Alcotest case [sharpe_constant_returns_rf0_is_nan] in test_metrics.ml.
   [QCheck.assume] discards constant inputs so the live path is exercised. *)
let sharpe_finite_on_non_constant_input =
  QCheck.Test.make ~count:200 ~name:"sharpe_finite_on_non_constant_input"
    Qcheck_gen.daily_returns_series_arb (fun s ->
      QCheck.assume (series_is_non_constant s);
      Float.is_finite (Cairos_finance.sharpe ~risk_free:0.0 s))

(* TP-Finance-Sharpe2 — [sharpe ~risk_free:0.0 (k *. s)] equals
   [sharpe ~risk_free:0.0 s] within [tol 1e-9] for [k > 0]. With
   [~risk_free:0.0], excess = returns; mean and ddof=1 std both scale by k,
   so the ratio is invariant. The [sqrt(ann_factor)] multiplier is constant
   for daily input. Substituted by RFC §G for the unsound additive-shift
   compensation in the PRD: [~risk_free] is geometrically converted per
   cairos_finance.mli:60, so an additive shift on the input is not exactly
   compensated by an additive shift on [~risk_free]. Scale-invariance at
   [~risk_free:0.0] is algebraically exact. The scale is applied at the
   Nx-tensor level via [Nx.mul_s] inside [Series.map]. *)
let returns_with_scale_arb =
  let open QCheck in
  let gen =
    let open Gen in
    let* s = get_gen Qcheck_gen.daily_returns_series_arb in
    let* k = float_range 1e-3 1e3 in
    return (s, k)
  in
  make
    ~print:(fun (s, k) ->
      Printf.sprintf "<returns len=%d scale k=%g>" (Cairos.Series.length s) k)
    gen

let sharpe_scale_invariant_at_zero_risk_free =
  QCheck.Test.make ~count:200 ~name:"sharpe_scale_invariant_at_zero_risk_free"
    returns_with_scale_arb (fun (s, k) ->
      QCheck.assume (series_is_non_constant s);
      let scaled = Cairos.Series.map (fun t -> Nx.mul_s t k) s in
      let actual = Cairos_finance.sharpe ~risk_free:0.0 scaled in
      let expected = Cairos_finance.sharpe ~risk_free:0.0 s in
      Qcheck_gen.float_approx_equal ~tol:1e-9 actual expected)

(* === drawdown_series === *)

(* TP-Finance-DD1 — every element of [drawdown_series s] is <= 0 and the
   output series has the same length as the input. Pointwise: the output is
   [(wealth - peak) / peak] where [peak = cummax(wealth)], so wealth <= peak
   pointwise by construction. Length: [drawdown_series] is a series transform
   that preserves the input index, so the per-element output matches the
   input length. [daily_returns_series_arb] produces no-NaN finite values per
   [Gen.float_range], satisfying the "behaviour undefined if input contains
   NaN" precondition (cairos_finance.mli:99). *)
let drawdown_series_pointwise_non_positive =
  QCheck.Test.make ~count:200 ~name:"drawdown_series_pointwise_non_positive"
    Qcheck_gen.daily_returns_series_arb (fun s ->
      let dd = Cairos_finance.drawdown_series s in
      let arr = Nx.to_array (Cairos.Series.values dd) in
      let len_preserved = Cairos.Series.length dd = Cairos.Series.length s in
      let pointwise_non_positive = arr |> Array.for_all (fun x -> x <= 0.0) in
      len_preserved && pointwise_non_positive)

(* === max_drawdown === *)

(* TP-Finance-MDD1 — Appendix A row 1: [max_drawdown] is the magnitude of the
   worst peak-to-trough decline, returned as a non-negative fraction in
   [0.0, 1.0]. The epic's [<= 0] phrasing was a sign flip; library contract
   wins. Catches a regression that returns the signed drawdown instead of its
   magnitude. *)
let max_drawdown_non_negative =
  QCheck.Test.make ~count:200 ~name:"max_drawdown_non_negative"
    Qcheck_gen.daily_returns_series_arb (fun s ->
      Cairos_finance.max_drawdown s >= 0.0)

(* TP-Finance-MDD2 — for strictly positive returns, the implied wealth index
   [cumprod(1 +. r)] is monotone-increasing, so the drawdown series is
   identically [0.0] and [max_drawdown s = 0.0] exactly. Generator restricts
   each return to [1e-6, 0.5] (strict positivity). *)
let strict_positive_returns_arb =
  let open QCheck in
  let gen =
    let open Gen in
    let* n = int_range 1 32 in
    let* xs = array_size (return n) (float_range 1e-6 0.5) in
    return (Qcheck_gen.make_series_from_floats ~freq:Cairos.Freq.Day xs)
  in
  make
    ~print:(fun s ->
      Printf.sprintf "<strict-positive returns len=%d>" (Cairos.Series.length s))
    gen

let max_drawdown_zero_on_strict_positive_returns =
  QCheck.Test.make ~count:200
    ~name:"max_drawdown_zero_on_strict_positive_returns"
    strict_positive_returns_arb (fun s -> Cairos_finance.max_drawdown s = 0.0)

let () =
  Qcheck_gen.pin_seed_from_env ();
  let tests =
    List.map QCheck_alcotest.to_alcotest
      [
        cumulative_return_finite_on_finite_input;
        cumulative_return_zero_on_all_zero;
        cumulative_return_constant_r_closed_form;
        annualised_return_finite_or_nan_per_contract;
        annualised_vol_finite_on_finite_input_n_ge_2;
        annualised_vol_zero_on_constant_n_ge_2;
        annualised_vol_invariant_under_additive_shift;
        sharpe_finite_on_non_constant_input;
        sharpe_scale_invariant_at_zero_risk_free;
        drawdown_series_pointwise_non_positive;
        max_drawdown_non_negative;
        max_drawdown_zero_on_strict_positive_returns;
      ]
  in
  Alcotest.run "cairos_finance.props" [ ("property", tests) ]
