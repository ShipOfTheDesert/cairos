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

let daily_float_series_arb :
    ([ `Daily ], (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t
    QCheck.arbitrary =
  let open QCheck in
  let gen =
    Gen.map make_daily_series_from_floats
      (Gen.array_size (Gen.int_range 1 64) Gen.float)
  in
  let shrink s =
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
  in
  make ~shrink
    ~print:(fun s ->
      Printf.sprintf "<daily series len=%d>" (Cairos.Series.length s))
    gen

let pct_change_preserves_length =
  QCheck.Test.make ~count:200 ~name:"pct_change preserves series length"
    daily_float_series_arb (fun s ->
      Cairos.Series.length (Cairos.Series.pct_change s) = Cairos.Series.length s)

let pct_change_index_zero_is_nan =
  QCheck.Test.make ~count:200 ~name:"pct_change index 0 is nan"
    daily_float_series_arb (fun s ->
      let vs =
        Nx.to_array (Cairos.Series.values (Cairos.Series.pct_change s))
      in
      Float.is_nan vs.(0))

let pct_change_value_semantics =
  let approx_eq a b =
    if a = b then true
    else if Float.is_finite a && Float.is_finite b then
      let diff = Float.abs (a -. b) in
      let scale = Float.max 1.0 (Float.max (Float.abs a) (Float.abs b)) in
      diff <= 1e-12 *. scale
    else false
  in
  QCheck.Test.make ~count:200
    ~name:"pct_change i = (v_i - v_{i-1}) / v_{i-1} where finite and prev <> 0"
    daily_float_series_arb (fun s ->
      let vs = Nx.to_array (Cairos.Series.values s) in
      let rs =
        Nx.to_array (Cairos.Series.values (Cairos.Series.pct_change s))
      in
      let ok = ref true in
      for i = 1 to Array.length vs - 1 do
        let prev = vs.(i - 1) and curr = vs.(i) in
        if Float.is_finite prev && Float.is_finite curr && prev <> 0.0 then
          let expected = (curr -. prev) /. prev in
          if Float.is_finite expected && not (approx_eq rs.(i) expected) then
            ok := false
      done;
      !ok)

let make_succeeds_with_matching_lengths () =
  match Cairos.Index.daily [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] with
  | Error e -> Alcotest.fail (Cairos.Index.err_to_string e)
  | Ok idx -> (
      let values = Nx.create Nx.float64 [| 3 |] [| 10.0; 20.0; 30.0 |] in
      match Cairos.Series.make idx values with
      | Error e -> Alcotest.fail e
      | Ok s -> Alcotest.(check int) "length is 3" 3 (Cairos.Series.length s))

let make_fails_with_mismatched_lengths () =
  match Cairos.Index.daily [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] with
  | Error e -> Alcotest.fail (Cairos.Index.err_to_string e)
  | Ok idx -> (
      let values = Nx.create Nx.float64 [| 5 |] [| 1.0; 2.0; 3.0; 4.0; 5.0 |] in
      match Cairos.Series.make idx values with
      | Ok _ -> Alcotest.fail "expected Error for mismatched lengths"
      | Error _ -> ())

let slice_produces_correct_subseries () =
  match
    Cairos.Index.daily
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |]
  with
  | Error e -> Alcotest.fail (Cairos.Index.err_to_string e)
  | Ok idx -> (
      let values = Nx.create Nx.float64 [| 4 |] [| 10.0; 20.0; 30.0; 40.0 |] in
      match Cairos.Series.make idx values with
      | Error e -> Alcotest.fail e
      | Ok s ->
          let sliced = Cairos.Series.slice ~start:1 ~stop:3 s in
          Alcotest.(check int) "sliced length" 2 (Cairos.Series.length sliced);
          let vs = Nx.to_array (Cairos.Series.values sliced) in
          Alcotest.(check (float 0.001)) "value at 0" 20.0 vs.(0);
          Alcotest.(check (float 0.001)) "value at 1" 30.0 vs.(1))

let map_transforms_values_and_preserves_index () =
  match Cairos.Index.daily [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] with
  | Error e -> Alcotest.fail (Cairos.Index.err_to_string e)
  | Ok idx -> (
      let values = Nx.create Nx.float64 [| 3 |] [| 1.0; 2.0; 3.0 |] in
      match Cairos.Series.make idx values with
      | Error e -> Alcotest.fail e
      | Ok s ->
          let doubled = Cairos.Series.map (fun v -> Nx.mul_s v 2.0) s in
          let vs = Nx.to_array (Cairos.Series.values doubled) in
          Alcotest.(check (float 0.001)) "doubled 0" 2.0 vs.(0);
          Alcotest.(check (float 0.001)) "doubled 1" 4.0 vs.(1);
          Alcotest.(check (float 0.001)) "doubled 2" 6.0 vs.(2);
          let ptime_testable =
            Alcotest.testable
              (fun ppf t -> Fmt.pf ppf "%a" (Ptime.pp_rfc3339 ()) t)
              Ptime.equal
          in
          let orig_ts = Cairos.Index.timestamps (Cairos.Series.index s) in
          let mapped_ts =
            Cairos.Index.timestamps (Cairos.Series.index doubled)
          in
          Alcotest.(check ptime_testable) "ts 0" orig_ts.(0) mapped_ts.(0);
          Alcotest.(check ptime_testable) "ts 1" orig_ts.(1) mapped_ts.(1);
          Alcotest.(check ptime_testable) "ts 2" orig_ts.(2) mapped_ts.(2))

let empty_series_succeeds () =
  match Cairos.Index.daily [||] with
  | Error e -> Alcotest.fail (Cairos.Index.err_to_string e)
  | Ok idx -> (
      let values = Nx.create Nx.float64 [| 0 |] [||] in
      match Cairos.Series.make idx values with
      | Error e -> Alcotest.fail e
      | Ok s -> Alcotest.(check int) "length is 0" 0 (Cairos.Series.length s))

let slice_empty_range_produces_empty_series () =
  match Cairos.Index.daily [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] with
  | Error e -> Alcotest.fail (Cairos.Index.err_to_string e)
  | Ok idx -> (
      let values = Nx.create Nx.float64 [| 3 |] [| 1.0; 2.0; 3.0 |] in
      match Cairos.Series.make idx values with
      | Error e -> Alcotest.fail e
      | Ok s ->
          let equal_bounds = Cairos.Series.slice ~start:1 ~stop:1 s in
          Alcotest.(check int)
            "start == stop" 0
            (Cairos.Series.length equal_bounds);
          Alcotest.(check int)
            "tensor shape is 0" 0
            (Nx.shape (Cairos.Series.values equal_bounds)).(0);
          let reversed = Cairos.Series.slice ~start:2 ~stop:1 s in
          Alcotest.(check int) "start > stop" 0 (Cairos.Series.length reversed))

let shift_positive_lags_values () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 10.0; 20.0; 30.0; 40.0; 50.0 |]
  in
  let shifted = Cairos.Series.shift 2 s in
  let vs = Nx.to_array (Cairos.Series.values shifted) in
  Alcotest.(check bool) "pos 0 is nan" true (Float.is_nan vs.(0));
  Alcotest.(check bool) "pos 1 is nan" true (Float.is_nan vs.(1));
  Alcotest.(check (float 0.001)) "pos 2" 10.0 vs.(2);
  Alcotest.(check (float 0.001)) "pos 3" 20.0 vs.(3);
  Alcotest.(check (float 0.001)) "pos 4" 30.0 vs.(4)

let shift_negative_leads_values () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 10.0; 20.0; 30.0; 40.0; 50.0 |]
  in
  let shifted = Cairos.Series.shift (-2) s in
  let vs = Nx.to_array (Cairos.Series.values shifted) in
  Alcotest.(check (float 0.001)) "pos 0" 30.0 vs.(0);
  Alcotest.(check (float 0.001)) "pos 1" 40.0 vs.(1);
  Alcotest.(check (float 0.001)) "pos 2" 50.0 vs.(2);
  Alcotest.(check bool) "pos 3 is nan" true (Float.is_nan vs.(3));
  Alcotest.(check bool) "pos 4 is nan" true (Float.is_nan vs.(4))

let shift_zero_preserves_values () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  let shifted = Cairos.Series.shift 0 s in
  let vs = Nx.to_array (Cairos.Series.values shifted) in
  Alcotest.(check (float 0.001)) "pos 0" 10.0 vs.(0);
  Alcotest.(check (float 0.001)) "pos 1" 20.0 vs.(1);
  Alcotest.(check (float 0.001)) "pos 2" 30.0 vs.(2)

let shift_by_length_produces_all_nan () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 10.0; 20.0; 30.0; 40.0; 50.0 |]
  in
  let shifted = Cairos.Series.shift 5 s in
  let vs = Nx.to_array (Cairos.Series.values shifted) in
  Alcotest.(check int) "length preserved" 5 (Array.length vs);
  Array.iter (fun v -> Alcotest.(check bool) "is nan" true (Float.is_nan v)) vs

let shift_empty_series () =
  let s = Test_helpers.make_daily_series [||] [||] in
  let shifted = Cairos.Series.shift 3 s in
  Alcotest.(check int) "length is 0" 0 (Cairos.Series.length shifted)

let pct_change_computes_returns () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 100.0; 110.0; 99.0 |]
  in
  let result = Cairos.Series.pct_change s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check bool) "pos 0 is nan" true (Float.is_nan vs.(0));
  Alcotest.(check (float 0.001)) "pos 1" 0.1 vs.(1);
  Alcotest.(check (float 0.001)) "pos 2" (-0.1) vs.(2)

let pct_change_zero_denominator () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 0.0; 5.0; 0.0 |]
  in
  let result = Cairos.Series.pct_change s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check bool) "pos 0 is nan" true (Float.is_nan vs.(0));
  Alcotest.(check bool)
    "pos 1 is inf" true
    (Float.is_infinite vs.(1) && vs.(1) > 0.0);
  Alcotest.(check (float 0.001)) "pos 2" (-1.0) vs.(2)

let ffill_fills_interior_nans () =
  let s =
    Test_helpers.make_daily_series
      [|
        "2024-01-01";
        "2024-01-02";
        "2024-01-03";
        "2024-01-04";
        "2024-01-05";
        "2024-01-06";
      |]
      [| Float.nan; 1.0; Float.nan; Float.nan; 2.0; Float.nan |]
  in
  let result = Cairos.Series.ffill s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check bool) "pos 0 still nan" true (Float.is_nan vs.(0));
  Alcotest.(check (float 0.001)) "pos 1" 1.0 vs.(1);
  Alcotest.(check (float 0.001)) "pos 2" 1.0 vs.(2);
  Alcotest.(check (float 0.001)) "pos 3" 1.0 vs.(3);
  Alcotest.(check (float 0.001)) "pos 4" 2.0 vs.(4);
  Alcotest.(check (float 0.001)) "pos 5" 2.0 vs.(5)

let ffill_preserves_leading_nans () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |]
      [| Float.nan; Float.nan; 5.0; Float.nan |]
  in
  let result = Cairos.Series.ffill s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check bool) "pos 0 still nan" true (Float.is_nan vs.(0));
  Alcotest.(check bool) "pos 1 still nan" true (Float.is_nan vs.(1));
  Alcotest.(check (float 0.001)) "pos 2" 5.0 vs.(2);
  Alcotest.(check (float 0.001)) "pos 3" 5.0 vs.(3)

let bfill_fills_interior_nans () =
  let s =
    Test_helpers.make_daily_series
      [|
        "2024-01-01";
        "2024-01-02";
        "2024-01-03";
        "2024-01-04";
        "2024-01-05";
        "2024-01-06";
      |]
      [| Float.nan; 1.0; Float.nan; Float.nan; 2.0; Float.nan |]
  in
  let result = Cairos.Series.bfill s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check (float 0.001)) "pos 0" 1.0 vs.(0);
  Alcotest.(check (float 0.001)) "pos 1" 1.0 vs.(1);
  Alcotest.(check (float 0.001)) "pos 2" 2.0 vs.(2);
  Alcotest.(check (float 0.001)) "pos 3" 2.0 vs.(3);
  Alcotest.(check (float 0.001)) "pos 4" 2.0 vs.(4);
  Alcotest.(check bool) "pos 5 still nan" true (Float.is_nan vs.(5))

let bfill_preserves_trailing_nans () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |]
      [| Float.nan; 5.0; Float.nan; Float.nan |]
  in
  let result = Cairos.Series.bfill s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check (float 0.001)) "pos 0" 5.0 vs.(0);
  Alcotest.(check (float 0.001)) "pos 1" 5.0 vs.(1);
  Alcotest.(check bool) "pos 2 still nan" true (Float.is_nan vs.(2));
  Alcotest.(check bool) "pos 3 still nan" true (Float.is_nan vs.(3))

let ffill_no_nans_identity () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let result = Cairos.Series.ffill s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check (float 0.001)) "pos 0" 1.0 vs.(0);
  Alcotest.(check (float 0.001)) "pos 1" 2.0 vs.(1);
  Alcotest.(check (float 0.001)) "pos 2" 3.0 vs.(2)

let ffill_empty_series () =
  let s = Test_helpers.make_daily_series [||] [||] in
  let result = Cairos.Series.ffill s in
  Alcotest.(check int) "length is 0" 0 (Cairos.Series.length result)

let bfill_no_nans_identity () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let result = Cairos.Series.bfill s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check (float 0.001)) "pos 0" 1.0 vs.(0);
  Alcotest.(check (float 0.001)) "pos 1" 2.0 vs.(1);
  Alcotest.(check (float 0.001)) "pos 2" 3.0 vs.(2)

let bfill_empty_series () =
  let s = Test_helpers.make_daily_series [||] [||] in
  let result = Cairos.Series.bfill s in
  Alcotest.(check int) "length is 0" 0 (Cairos.Series.length result)

let pct_change_all_nan_input () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| Float.nan; Float.nan; Float.nan |]
  in
  let result = Cairos.Series.pct_change s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Array.iter (fun v -> Alcotest.(check bool) "is nan" true (Float.is_nan v)) vs

let head_returns_first_n_entries () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 10.0; 20.0; 30.0; 40.0; 50.0 |]
  in
  let h = Cairos.Series.head 3 s in
  Alcotest.(check int) "length" 3 (Cairos.Series.length h);
  let vs = Nx.to_array (Cairos.Series.values h) in
  Alcotest.(check (float 0.001)) "val 0" 10.0 vs.(0);
  Alcotest.(check (float 0.001)) "val 1" 20.0 vs.(1);
  Alcotest.(check (float 0.001)) "val 2" 30.0 vs.(2);
  let ts = Cairos.Index.timestamps (Cairos.Series.index h) in
  let orig_ts = Cairos.Index.timestamps (Cairos.Series.index s) in
  Alcotest.(check Test_helpers.ptime_testable) "ts 0" orig_ts.(0) ts.(0);
  Alcotest.(check Test_helpers.ptime_testable) "ts 1" orig_ts.(1) ts.(1);
  Alcotest.(check Test_helpers.ptime_testable) "ts 2" orig_ts.(2) ts.(2)

let head_clamps_to_length () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  let h = Cairos.Series.head 10 s in
  Alcotest.(check int) "length" 3 (Cairos.Series.length h)

let head_zero_returns_empty () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  let h = Cairos.Series.head 0 s in
  Alcotest.(check int) "length" 0 (Cairos.Series.length h)

let head_negative_returns_empty () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  let h = Cairos.Series.head (-1) s in
  Alcotest.(check int) "length" 0 (Cairos.Series.length h)

let tail_returns_last_n_entries () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 10.0; 20.0; 30.0; 40.0; 50.0 |]
  in
  let t = Cairos.Series.tail 3 s in
  Alcotest.(check int) "length" 3 (Cairos.Series.length t);
  let vs = Nx.to_array (Cairos.Series.values t) in
  Alcotest.(check (float 0.001)) "val 0" 30.0 vs.(0);
  Alcotest.(check (float 0.001)) "val 1" 40.0 vs.(1);
  Alcotest.(check (float 0.001)) "val 2" 50.0 vs.(2);
  let ts = Cairos.Index.timestamps (Cairos.Series.index t) in
  let orig_ts = Cairos.Index.timestamps (Cairos.Series.index s) in
  Alcotest.(check Test_helpers.ptime_testable) "ts 0" orig_ts.(2) ts.(0);
  Alcotest.(check Test_helpers.ptime_testable) "ts 1" orig_ts.(3) ts.(1);
  Alcotest.(check Test_helpers.ptime_testable) "ts 2" orig_ts.(4) ts.(2)

let tail_clamps_to_length () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  let t = Cairos.Series.tail 10 s in
  Alcotest.(check int) "length" 3 (Cairos.Series.length t)

let tail_zero_returns_empty () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  let t = Cairos.Series.tail 0 s in
  Alcotest.(check int) "length" 0 (Cairos.Series.length t)

let tail_negative_returns_empty () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  let t = Cairos.Series.tail (-1) s in
  Alcotest.(check int) "length" 0 (Cairos.Series.length t)

let first_valid_finds_first_non_nan () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |]
      [| Float.nan; Float.nan; 3.0; 4.0 |]
  in
  match Cairos.Series.first_valid s with
  | None -> Alcotest.fail "expected Some"
  | Some (i, v) ->
      Alcotest.(check int) "index" 2 i;
      Alcotest.(check (float 0.001)) "value" 3.0 v

let first_valid_returns_none_all_nan () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| Float.nan; Float.nan; Float.nan |]
  in
  Alcotest.(check bool)
    "is None" true
    (Option.is_none (Cairos.Series.first_valid s))

let first_valid_returns_none_empty () =
  let s = Test_helpers.make_daily_series [||] [||] in
  Alcotest.(check bool)
    "is None" true
    (Option.is_none (Cairos.Series.first_valid s))

let first_valid_first_element () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  match Cairos.Series.first_valid s with
  | None -> Alcotest.fail "expected Some"
  | Some (i, v) ->
      Alcotest.(check int) "index" 0 i;
      Alcotest.(check (float 0.001)) "value" 1.0 v

let tests =
  [
    ( "make_succeeds_with_matching_lengths",
      `Quick,
      make_succeeds_with_matching_lengths );
    ( "make_fails_with_mismatched_lengths",
      `Quick,
      make_fails_with_mismatched_lengths );
    ( "slice_produces_correct_subseries",
      `Quick,
      slice_produces_correct_subseries );
    ( "map_transforms_values_and_preserves_index",
      `Quick,
      map_transforms_values_and_preserves_index );
    ("empty_series_succeeds", `Quick, empty_series_succeeds);
    ( "slice_empty_range_produces_empty_series",
      `Quick,
      slice_empty_range_produces_empty_series );
    ("shift_positive_lags_values", `Quick, shift_positive_lags_values);
    ("shift_negative_leads_values", `Quick, shift_negative_leads_values);
    ("shift_zero_preserves_values", `Quick, shift_zero_preserves_values);
    ( "shift_by_length_produces_all_nan",
      `Quick,
      shift_by_length_produces_all_nan );
    ("shift_empty_series", `Quick, shift_empty_series);
    ("pct_change_computes_returns", `Quick, pct_change_computes_returns);
    ("pct_change_zero_denominator", `Quick, pct_change_zero_denominator);
    ("pct_change_all_nan_input", `Quick, pct_change_all_nan_input);
    ("ffill_fills_interior_nans", `Quick, ffill_fills_interior_nans);
    ("ffill_preserves_leading_nans", `Quick, ffill_preserves_leading_nans);
    ("bfill_fills_interior_nans", `Quick, bfill_fills_interior_nans);
    ("bfill_preserves_trailing_nans", `Quick, bfill_preserves_trailing_nans);
    ("ffill_no_nans_identity", `Quick, ffill_no_nans_identity);
    ("ffill_empty_series", `Quick, ffill_empty_series);
    ("bfill_no_nans_identity", `Quick, bfill_no_nans_identity);
    ("bfill_empty_series", `Quick, bfill_empty_series);
    ("head_returns_first_n_entries", `Quick, head_returns_first_n_entries);
    ("head_clamps_to_length", `Quick, head_clamps_to_length);
    ("head_zero_returns_empty", `Quick, head_zero_returns_empty);
    ("head_negative_returns_empty", `Quick, head_negative_returns_empty);
    ("tail_returns_last_n_entries", `Quick, tail_returns_last_n_entries);
    ("tail_clamps_to_length", `Quick, tail_clamps_to_length);
    ("tail_zero_returns_empty", `Quick, tail_zero_returns_empty);
    ("tail_negative_returns_empty", `Quick, tail_negative_returns_empty);
    ("first_valid_finds_first_non_nan", `Quick, first_valid_finds_first_non_nan);
    ( "first_valid_returns_none_all_nan",
      `Quick,
      first_valid_returns_none_all_nan );
    ("first_valid_returns_none_empty", `Quick, first_valid_returns_none_empty);
    ("first_valid_first_element", `Quick, first_valid_first_element);
    QCheck_alcotest.to_alcotest pct_change_preserves_length;
    QCheck_alcotest.to_alcotest pct_change_index_zero_is_nan;
    QCheck_alcotest.to_alcotest pct_change_value_semantics;
  ]

let () = Alcotest.run "Series" [ ("Series", tests) ]
