let epoch_2024_01_01_utc = 1_704_067_200.0

(* The two [failwith]s below are unreachable by construction (synthetic UTC
   epoch + length-matched [Nx.t]) and are intentional per RFC 0030 §R4: a
   generator must return [Series.t], not [(_, string) result]. Do not "fix"
   by propagating [result] — that breaks the QCheck arbitrary contract. *)
let make_daily_series_from_floats (xs : float array) :
    ([ `Daily ], (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t =
  let n = Array.length xs in
  let ts =
    Array.init n (fun i -> epoch_2024_01_01_utc +. (float_of_int i *. 86_400.0))
  in
  let idx =
    match Cairos.Index.of_unix_floats Cairos.Freq.Day ts with
    | Ok i -> i
    | Error e -> failwith ("generator index: " ^ Cairos.Index.err_to_string e)
  in
  let values = Nx.create Nx.float64 [| n |] xs in
  match Cairos.Series.make idx values with
  | Ok s -> s
  | Error e -> failwith ("generator series: " ^ e)

(* Prefix-truncation shrink: shortens the series to length 1, n/2, or n-1. Safe
   to share across all generators below because truncation cannot violate a
   per-element precondition (non-negative, strict-positive) that already held
   on the untruncated series. *)
let shrink_daily_series s =
  let arr = Nx.to_array (Cairos.Series.values s) in
  let n = Array.length arr in
  let open QCheck.Iter in
  if n <= 1 then empty
  else
    let candidates =
      List.sort_uniq compare [ 1; n / 2; n - 1 ]
      |> List.filter (fun k -> k >= 1 && k < n)
    in
    of_list candidates >|= fun k ->
    make_daily_series_from_floats (Array.sub arr 0 k)

let daily_float_series_arb :
    ([ `Daily ], (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t
    QCheck.arbitrary =
  let open QCheck in
  let gen =
    Gen.map make_daily_series_from_floats
      (Gen.array_size (Gen.int_range 1 64) Gen.float)
  in
  make ~shrink:shrink_daily_series
    ~print:(fun s ->
      Printf.sprintf "<daily series len=%d>" (Cairos.Series.length s))
    gen

(* Test 1: scan preserves length *)
let scan_preserves_length =
  QCheck.Test.make ~count:200 ~name:"scan_preserves_length"
    daily_float_series_arb (fun s ->
      Cairos.Series.length (Cairos.Series.scan ( +. ) 0.0 s)
      = Cairos.Series.length s)

(* Test 2: cumsum monotone on non-negatives *)
let daily_non_negative_series_arb =
  let open QCheck in
  let gen =
    Gen.map make_daily_series_from_floats
      (Gen.array_size (Gen.int_range 1 64) (Gen.float_bound_inclusive 1e6))
  in
  make ~shrink:shrink_daily_series
    ~print:(fun s ->
      Printf.sprintf "<daily non-neg series len=%d>" (Cairos.Series.length s))
    gen

let cumsum_monotone_on_non_negatives =
  QCheck.Test.make ~count:200 ~name:"cumsum_monotone_on_non_negatives"
    daily_non_negative_series_arb (fun s ->
      let vs = Nx.to_array (Cairos.Series.values (Cairos.Series.cumsum s)) in
      let ok = ref true in
      for i = 1 to Array.length vs - 1 do
        if vs.(i) < vs.(i - 1) then ok := false
      done;
      !ok)

(* Test 3: cumprod positive on strict positives *)
let daily_strict_positive_series_arb =
  let open QCheck in
  let gen =
    Gen.map make_daily_series_from_floats
      (Gen.array_size (Gen.int_range 1 64) (Gen.float_range 1e-6 1e6))
  in
  make ~shrink:shrink_daily_series
    ~print:(fun s ->
      Printf.sprintf "<daily pos series len=%d>" (Cairos.Series.length s))
    gen

let cumprod_positive_on_strict_positives =
  QCheck.Test.make ~count:200 ~name:"cumprod_positive_on_strict_positives"
    daily_strict_positive_series_arb (fun s ->
      let vs = Nx.to_array (Cairos.Series.values (Cairos.Series.cumprod s)) in
      Array.for_all (fun v -> v > 0.0) vs)

(* OCaml's generic (=) returns false for nan = nan (IEEE 754). We need
   bitwise equality: two NaN values from the same code path are equal. *)
let float_bitwise_equal a b =
  Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b)

let float_arrays_bitwise_equal a b =
  Array.length a = Array.length b && Array.for_all2 float_bitwise_equal a b

(* Test 4: scan (+.) 0.0 equals cumsum — exact bit equality *)
let scan_plus_zero_equals_cumsum =
  QCheck.Test.make ~count:200 ~name:"scan_plus_zero_equals_cumsum"
    daily_float_series_arb (fun s ->
      let via_scan =
        Nx.to_array (Cairos.Series.values (Cairos.Series.scan ( +. ) 0.0 s))
      in
      let via_cumsum =
        Nx.to_array (Cairos.Series.values (Cairos.Series.cumsum s))
      in
      float_arrays_bitwise_equal via_scan via_cumsum)

(* Test 5: scan ( *. ) 1.0 equals cumprod — exact bit equality *)
let scan_times_one_equals_cumprod =
  QCheck.Test.make ~count:200 ~name:"scan_times_one_equals_cumprod"
    daily_float_series_arb (fun s ->
      let via_scan =
        Nx.to_array (Cairos.Series.values (Cairos.Series.scan ( *. ) 1.0 s))
      in
      let via_cumprod =
        Nx.to_array (Cairos.Series.values (Cairos.Series.cumprod s))
      in
      float_arrays_bitwise_equal via_scan via_cumprod)

(* Three-branch NaN-aware tolerance comparator per
   ~/.claude/solutions/general/nan-aware-tolerance-comparator.md:
   branch on is_nan for both operands before subtraction.

   Infinity handling: after the nan branches, short-circuit on [a = b] so
   same-sign infinities compare equal (otherwise [inf -. inf = nan] and the
   relative-tolerance branch wrongly rejects them). If only one side is still
   infinite — or signs differ — return false explicitly instead of relying
   on the arithmetic, because [+inf <= +inf] is [true] in OCaml and would
   otherwise let [|+inf - (-inf)| <= tol *. +inf] slip through. *)
let float_approx_equal ~tol a b =
  match (Float.is_nan a, Float.is_nan b) with
  | true, true -> true
  | true, false
  | false, true ->
      false
  | false, false ->
      if a = b then true
      else if (not (Float.is_finite a)) || not (Float.is_finite b) then false
      else
        let diff = Float.abs (a -. b) in
        let scale = Float.max 1.0 (Float.max (Float.abs a) (Float.abs b)) in
        diff <= tol *. scale

(* Test 6: scan final element equals Array.fold_left *)
let scan_final_equals_fold =
  QCheck.Test.make ~count:200 ~name:"scan_final_equals_fold"
    daily_float_series_arb (fun s ->
      let vs = Nx.to_array (Cairos.Series.values s) in
      let scanned =
        Nx.to_array (Cairos.Series.values (Cairos.Series.scan ( +. ) 0.0 s))
      in
      let n = Array.length vs in
      let scan_last = scanned.(n - 1) in
      let fold_result = Array.fold_left ( +. ) 0.0 vs in
      float_approx_equal ~tol:1e-10 scan_last fold_result)

(* Test 7: scan pointwise prefix equivalence *)
let scan_pointwise_prefix_equivalence =
  QCheck.Test.make ~count:200 ~name:"scan_pointwise_prefix_equivalence"
    daily_float_series_arb (fun s ->
      let vs = Nx.to_array (Cairos.Series.values s) in
      let scanned =
        Nx.to_array (Cairos.Series.values (Cairos.Series.scan ( +. ) 0.0 s))
      in
      let n = Array.length vs in
      let ok = ref true in
      for i = 0 to n - 1 do
        let prefix = Array.sub vs 0 (i + 1) in
        let naive = Array.fold_left ( +. ) 0.0 prefix in
        if not (float_approx_equal ~tol:1e-10 scanned.(i) naive) then
          ok := false
      done;
      !ok)

let qcheck_tests =
  List.map QCheck_alcotest.to_alcotest
    [
      scan_preserves_length;
      cumsum_monotone_on_non_negatives;
      cumprod_positive_on_strict_positives;
      scan_plus_zero_equals_cumsum;
      scan_times_one_equals_cumprod;
      scan_final_equals_fold;
      scan_pointwise_prefix_equivalence;
    ]

(* --- Deterministic Alcotest tests (C1, S1, S2) --- *)

let check_float_array msg ~expected actual =
  let n_exp = Array.length expected in
  let n_act = Array.length actual in
  Alcotest.(check int) (msg ^ " length") n_exp n_act;
  Array.iteri
    (fun i e ->
      let a = actual.(i) in
      match (Float.is_nan e, Float.is_nan a) with
      | true, true -> ()
      | true, false -> Alcotest.failf "%s.[%d]: expected NaN, got %g" msg i a
      | false, true -> Alcotest.failf "%s.[%d]: expected %g, got NaN" msg i e
      | false, false ->
          let tol = 1e-14 *. Float.max 1.0 (Float.abs e) in
          if Float.abs (a -. e) > tol then
            Alcotest.failf "%s.[%d]: expected %g, got %g" msg i e a)
    expected

(* C1: deterministic regression tests *)
let test_cumsum_known () =
  let s = make_daily_series_from_floats [| 1.0; 2.0; 3.0; 4.0 |] in
  let result = Nx.to_array (Cairos.Series.values (Cairos.Series.cumsum s)) in
  check_float_array "cumsum [1;2;3;4]" ~expected:[| 1.0; 3.0; 6.0; 10.0 |]
    result

let test_cumprod_known () =
  let s = make_daily_series_from_floats [| 2.0; 3.0; 4.0 |] in
  let result = Nx.to_array (Cairos.Series.values (Cairos.Series.cumprod s)) in
  check_float_array "cumprod [2;3;4]" ~expected:[| 2.0; 6.0; 24.0 |] result

let test_scan_sub_known () =
  let s = make_daily_series_from_floats [| 10.0; 3.0; 1.0 |] in
  let result =
    Nx.to_array (Cairos.Series.values (Cairos.Series.scan ( -. ) 0.0 s))
  in
  (* 0-10 = -10; -10-3 = -13; -13-1 = -14 *)
  check_float_array "scan (-.) 0.0 [10;3;1]" ~expected:[| -10.0; -13.0; -14.0 |]
    result

(* S1: empty series *)
let test_scan_empty () =
  let s = make_daily_series_from_floats [||] in
  let result =
    Nx.to_array (Cairos.Series.values (Cairos.Series.scan ( +. ) 0.0 s))
  in
  Alcotest.(check int) "empty scan length" 0 (Array.length result)

let test_cumsum_empty () =
  let s = make_daily_series_from_floats [||] in
  let result = Nx.to_array (Cairos.Series.values (Cairos.Series.cumsum s)) in
  Alcotest.(check int) "empty cumsum length" 0 (Array.length result)

let test_cumprod_empty () =
  let s = make_daily_series_from_floats [||] in
  let result = Nx.to_array (Cairos.Series.values (Cairos.Series.cumprod s)) in
  Alcotest.(check int) "empty cumprod length" 0 (Array.length result)

(* S2: NaN propagation — mid-stream NaN poisons all subsequent elements *)
let test_cumsum_nan_propagation () =
  let s = make_daily_series_from_floats [| 1.0; 2.0; Float.nan; 4.0; 5.0 |] in
  let result = Nx.to_array (Cairos.Series.values (Cairos.Series.cumsum s)) in
  check_float_array "cumsum NaN propagation"
    ~expected:[| 1.0; 3.0; Float.nan; Float.nan; Float.nan |]
    result

let test_cumprod_nan_propagation () =
  let s = make_daily_series_from_floats [| 2.0; 3.0; Float.nan; 4.0; 5.0 |] in
  let result = Nx.to_array (Cairos.Series.values (Cairos.Series.cumprod s)) in
  check_float_array "cumprod NaN propagation"
    ~expected:[| 2.0; 6.0; Float.nan; Float.nan; Float.nan |]
    result

(* Direct coverage of NaN propagation at the [scan] primitive (not just via
   cumsum/cumprod). Uses a non-commutative fold so the test also pins fold
   order — reversing the operands would produce different finite values. *)
let test_scan_nan_propagation () =
  let s = make_daily_series_from_floats [| 1.0; 2.0; Float.nan; 4.0; 5.0 |] in
  let result =
    Nx.to_array (Cairos.Series.values (Cairos.Series.scan ( -. ) 0.0 s))
  in
  (* 0-1=-1; -1-2=-3; -3-nan=nan; nan-4=nan; nan-5=nan *)
  check_float_array "scan (-.) NaN propagation"
    ~expected:[| -1.0; -3.0; Float.nan; Float.nan; Float.nan |]
    result

(* Unit tests for float_approx_equal — pin both NaN branches per
   nan-aware-tolerance-comparator.md convention *)
let test_approx_eq_both_nan () =
  Alcotest.(check bool)
    "NaN = NaN" true
    (float_approx_equal ~tol:1e-10 Float.nan Float.nan)

let test_approx_eq_nan_vs_finite () =
  Alcotest.(check bool)
    "NaN <> 1.0" false
    (float_approx_equal ~tol:1e-10 Float.nan 1.0)

let test_approx_eq_finite_vs_nan () =
  Alcotest.(check bool)
    "1.0 <> NaN" false
    (float_approx_equal ~tol:1e-10 1.0 Float.nan)

let test_approx_eq_within_tol () =
  Alcotest.(check bool)
    "1.0 ≈ 1.0+1e-15" true
    (float_approx_equal ~tol:1e-10 1.0 (1.0 +. 1e-15))

let test_approx_eq_outside_tol () =
  Alcotest.(check bool)
    "1.0 ≉ 2.0" false
    (float_approx_equal ~tol:1e-10 1.0 2.0)

let test_approx_eq_same_sign_infinity () =
  Alcotest.(check bool)
    "+inf = +inf" true
    (float_approx_equal ~tol:1e-10 Float.infinity Float.infinity);
  Alcotest.(check bool)
    "-inf = -inf" true
    (float_approx_equal ~tol:1e-10 Float.neg_infinity Float.neg_infinity)

let test_approx_eq_opposite_sign_infinity () =
  Alcotest.(check bool)
    "+inf ≉ -inf" false
    (float_approx_equal ~tol:1e-10 Float.infinity Float.neg_infinity)

let test_approx_eq_infinity_vs_finite () =
  Alcotest.(check bool)
    "+inf ≉ 1.0" false
    (float_approx_equal ~tol:1e-10 Float.infinity 1.0)

let deterministic_tests =
  [
    Alcotest.test_case "cumsum known values" `Quick test_cumsum_known;
    Alcotest.test_case "cumprod known values" `Quick test_cumprod_known;
    Alcotest.test_case "scan sub known values" `Quick test_scan_sub_known;
    Alcotest.test_case "scan empty series" `Quick test_scan_empty;
    Alcotest.test_case "cumsum empty series" `Quick test_cumsum_empty;
    Alcotest.test_case "cumprod empty series" `Quick test_cumprod_empty;
    Alcotest.test_case "cumsum NaN propagation" `Quick
      test_cumsum_nan_propagation;
    Alcotest.test_case "cumprod NaN propagation" `Quick
      test_cumprod_nan_propagation;
    Alcotest.test_case "scan NaN propagation" `Quick test_scan_nan_propagation;
  ]

let comparator_tests =
  [
    Alcotest.test_case "both NaN equal" `Quick test_approx_eq_both_nan;
    Alcotest.test_case "NaN vs finite mismatch" `Quick
      test_approx_eq_nan_vs_finite;
    Alcotest.test_case "finite vs NaN mismatch" `Quick
      test_approx_eq_finite_vs_nan;
    Alcotest.test_case "within tolerance" `Quick test_approx_eq_within_tol;
    Alcotest.test_case "outside tolerance" `Quick test_approx_eq_outside_tol;
    Alcotest.test_case "same-sign infinity equal" `Quick
      test_approx_eq_same_sign_infinity;
    Alcotest.test_case "opposite-sign infinity mismatch" `Quick
      test_approx_eq_opposite_sign_infinity;
    Alcotest.test_case "infinity vs finite mismatch" `Quick
      test_approx_eq_infinity_vs_finite;
  ]

let () =
  Alcotest.run "Series.scan"
    [
      ("property", qcheck_tests);
      ("deterministic", deterministic_tests);
      ("comparator", comparator_tests);
    ]
