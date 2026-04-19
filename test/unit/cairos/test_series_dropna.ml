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
    | Error e -> failwith ("generator index: " ^ e)
  in
  let values = Nx.create Nx.float64 [| n |] xs in
  match Cairos.Series.make idx values with
  | Ok s -> s
  | Error e -> failwith ("generator series: " ^ e)

(* Prefix-truncation shrinker: shortens the series to length 1, n/2, or n-1.
   For n <= 1 there is nothing meaningful to shrink to. *)
let shrink_daily_series s =
  let arr = Nx.to_array (Cairos.Series.values s) in
  let n = Array.length arr in
  let open QCheck.Iter in
  if n <= 1 then empty
  else
    let candidates =
      List.sort_uniq compare [ 0; 1; n / 2; n - 1 ]
      |> List.filter (fun k -> k >= 0 && k < n)
    in
    of_list candidates >|= fun k ->
    make_daily_series_from_floats (Array.sub arr 0 k)

(* [Gen.float] draws bit patterns uniformly, so NaN probability is ~1/4000
   per element — too rare to reliably exercise the mask-and-gather path.
   Mix explicit NaN in so the length and order invariants actually stress
   dropna's core behaviour on most generated inputs. *)
let float_or_nan_gen : float QCheck.Gen.t =
  QCheck.Gen.oneof_weighted
    [ (1, QCheck.Gen.return Float.nan); (3, QCheck.Gen.float) ]

let daily_float_series_arb :
    ([ `Daily ], (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t
    QCheck.arbitrary =
  let open QCheck in
  let gen =
    Gen.map make_daily_series_from_floats
      (Gen.array_size (Gen.int_range 0 64) float_or_nan_gen)
  in
  make ~shrink:shrink_daily_series
    ~print:(fun s ->
      Printf.sprintf "<daily series len=%d>" (Cairos.Series.length s))
    gen

let count_nan (xs : float array) =
  Array.fold_left (fun acc v -> if Float.is_nan v then acc + 1 else acc) 0 xs

(* Bitwise float equality — needed because dropna performs no arithmetic, so
   surviving values must be bit-identical to their input counterparts. Guards
   against a subtle miscompile where a no-op round-trip might perturb NaN
   payloads or sign bits. *)
let float_bitwise_equal a b =
  Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b)

(* Test 1: output length = input length - NaN count *)
let dropna_length_equals_input_minus_nan_count =
  QCheck.Test.make ~count:200 ~name:"dropna_length_equals_input_minus_nan_count"
    daily_float_series_arb (fun s ->
      let vs = Nx.to_array (Cairos.Series.values s) in
      let result = Cairos.Series.dropna s in
      Cairos.Series.length result = Array.length vs - count_nan vs)

(* Test 2: non-NaN values (and their paired timestamps) preserved in order,
   bit-identical to the input. Oracle: Array.filter against the input pairs. *)
let dropna_preserves_order_of_non_nan =
  QCheck.Test.make ~count:200 ~name:"dropna_preserves_order_of_non_nan"
    daily_float_series_arb (fun s ->
      let in_vs = Nx.to_array (Cairos.Series.values s) in
      let in_ts = Cairos.Index.timestamps (Cairos.Series.index s) in
      let expected_pairs =
        Array.to_list (Array.mapi (fun i v -> (in_ts.(i), v)) in_vs)
        |> List.filter (fun (_, v) -> not (Float.is_nan v))
      in
      let result = Cairos.Series.dropna s in
      let out_vs = Nx.to_array (Cairos.Series.values result) in
      let out_ts = Cairos.Index.timestamps (Cairos.Series.index result) in
      let n = Array.length out_vs in
      if List.length expected_pairs <> n then false
      else if Array.length out_ts <> n then false
      else
        let ok = ref true in
        List.iteri
          (fun i (ts_expected, v_expected) ->
            if not (Ptime.equal ts_expected out_ts.(i)) then ok := false;
            if not (float_bitwise_equal v_expected out_vs.(i)) then ok := false)
          expected_pairs;
        !ok)

let qcheck_tests =
  List.map QCheck_alcotest.to_alcotest
    [
      dropna_length_equals_input_minus_nan_count;
      dropna_preserves_order_of_non_nan;
    ]

(* A float32 counterpart to [make_daily_series_from_floats]. Exists solely so
   the [dropna_preserves_float32_element_type] test can pin the ['b] phantom
   across the signature — the float64 generator above cannot witness that. *)
let make_daily_float32_series_from_floats (xs : float array) :
    ([ `Daily ], (float, Bigarray.float32_elt) Nx.t) Cairos.Series.t =
  let n = Array.length xs in
  let ts =
    Array.init n (fun i -> epoch_2024_01_01_utc +. (float_of_int i *. 86_400.0))
  in
  let idx =
    match Cairos.Index.of_unix_floats Cairos.Freq.Day ts with
    | Ok i -> i
    | Error e -> failwith ("generator index: " ^ e)
  in
  let values = Nx.create Nx.float32 [| n |] xs in
  match Cairos.Series.make idx values with
  | Ok s -> s
  | Error e -> failwith ("generator series: " ^ e)

(* --- Deterministic Alcotest tests --- *)

let ptime_testable = Alcotest.testable (Ptime.pp_rfc3339 ()) Ptime.equal

let test_dropna_empty_input () =
  let s = make_daily_series_from_floats [||] in
  let result = Cairos.Series.dropna s in
  Alcotest.(check int) "length is 0" 0 (Cairos.Series.length result);
  let ts = Cairos.Index.timestamps (Cairos.Series.index result) in
  Alcotest.(check int) "timestamps length is 0" 0 (Array.length ts)

let test_dropna_all_nan_input () =
  let s =
    make_daily_series_from_floats
      [| Float.nan; Float.nan; Float.nan; Float.nan; Float.nan |]
  in
  let result = Cairos.Series.dropna s in
  Alcotest.(check int) "length is 0" 0 (Cairos.Series.length result);
  let ts = Cairos.Index.timestamps (Cairos.Series.index result) in
  Alcotest.(check int) "timestamps length is 0" 0 (Array.length ts)

let test_dropna_pairs_timestamps_with_surviving_values () =
  let s =
    make_daily_series_from_floats [| 1.0; Float.nan; 3.0; Float.nan; 5.0 |]
  in
  let in_ts = Cairos.Index.timestamps (Cairos.Series.index s) in
  let result = Cairos.Series.dropna s in
  let out_vs = Nx.to_array (Cairos.Series.values result) in
  let out_ts = Cairos.Index.timestamps (Cairos.Series.index result) in
  Alcotest.(check int) "output length" 3 (Array.length out_vs);
  Alcotest.(check (float 0.0)) "value 0" 1.0 out_vs.(0);
  Alcotest.(check (float 0.0)) "value 1" 3.0 out_vs.(1);
  Alcotest.(check (float 0.0)) "value 2" 5.0 out_vs.(2);
  Alcotest.(check int) "timestamps length" 3 (Array.length out_ts);
  Alcotest.(check ptime_testable) "ts 0" in_ts.(0) out_ts.(0);
  Alcotest.(check ptime_testable) "ts 1" in_ts.(2) out_ts.(1);
  Alcotest.(check ptime_testable) "ts 2" in_ts.(4) out_ts.(2)

let test_dropna_all_finite_input () =
  let s = make_daily_series_from_floats [| 1.0; 2.0; 3.0 |] in
  let in_ts = Cairos.Index.timestamps (Cairos.Series.index s) in
  let result = Cairos.Series.dropna s in
  let out_vs = Nx.to_array (Cairos.Series.values result) in
  let out_ts = Cairos.Index.timestamps (Cairos.Series.index result) in
  Alcotest.(check int) "output length unchanged" 3 (Array.length out_vs);
  Alcotest.(check (float 0.0)) "value 0" 1.0 out_vs.(0);
  Alcotest.(check (float 0.0)) "value 1" 2.0 out_vs.(1);
  Alcotest.(check (float 0.0)) "value 2" 3.0 out_vs.(2);
  Alcotest.(check int) "timestamps length" 3 (Array.length out_ts);
  Alcotest.(check ptime_testable) "ts 0" in_ts.(0) out_ts.(0);
  Alcotest.(check ptime_testable) "ts 1" in_ts.(1) out_ts.(1);
  Alcotest.(check ptime_testable) "ts 2" in_ts.(2) out_ts.(2)

let test_dropna_preserves_infinities () =
  let s =
    make_daily_series_from_floats
      [| Float.infinity; Float.nan; Float.neg_infinity |]
  in
  let in_ts = Cairos.Index.timestamps (Cairos.Series.index s) in
  let result = Cairos.Series.dropna s in
  let out_vs = Nx.to_array (Cairos.Series.values result) in
  let out_ts = Cairos.Index.timestamps (Cairos.Series.index result) in
  Alcotest.(check int) "output length" 2 (Array.length out_vs);
  Alcotest.(check bool)
    "value 0 is +infinity" true
    (Float.equal Float.infinity out_vs.(0));
  Alcotest.(check bool)
    "value 1 is -infinity" true
    (Float.equal Float.neg_infinity out_vs.(1));
  Alcotest.(check int) "timestamps length" 2 (Array.length out_ts);
  Alcotest.(check ptime_testable) "ts 0" in_ts.(0) out_ts.(0);
  Alcotest.(check ptime_testable) "ts 1" in_ts.(2) out_ts.(1)

(* The ascription on [result] is the whole point of this test: if [dropna]'s
   signature ever stopped being polymorphic in the element-type phantom ['b]
   (for instance, forcing float64 output like [cumsum]/[cumprod] do), this
   line would fail to type-check and the regression would be caught before
   running. RFC 0038 Options §A motivates ['b] preservation explicitly. *)
let test_dropna_preserves_float32_element_type () =
  let s = make_daily_float32_series_from_floats [| 1.0; Float.nan; 3.0 |] in
  let result : ([ `Daily ], (float, Bigarray.float32_elt) Nx.t) Cairos.Series.t
      =
    Cairos.Series.dropna s
  in
  let out_vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check int) "output length" 2 (Array.length out_vs);
  Alcotest.(check (float 0.0)) "value 0" 1.0 out_vs.(0);
  Alcotest.(check (float 0.0)) "value 1" 3.0 out_vs.(1)

let deterministic_tests =
  [
    Alcotest.test_case "dropna empty input" `Quick test_dropna_empty_input;
    Alcotest.test_case "dropna all-NaN input" `Quick test_dropna_all_nan_input;
    Alcotest.test_case "dropna pairs timestamps with surviving values" `Quick
      test_dropna_pairs_timestamps_with_surviving_values;
    Alcotest.test_case "dropna all-finite input" `Quick
      test_dropna_all_finite_input;
    Alcotest.test_case "dropna preserves infinities" `Quick
      test_dropna_preserves_infinities;
    Alcotest.test_case "dropna preserves float32 element type" `Quick
      test_dropna_preserves_float32_element_type;
  ]

let () =
  Alcotest.run "Series.dropna"
    [ ("property", qcheck_tests); ("deterministic", deterministic_tests) ]
