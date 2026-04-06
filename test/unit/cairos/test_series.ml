let make_succeeds_with_matching_lengths () =
  match Cairos.Index.daily [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] with
  | Error e -> Alcotest.fail e
  | Ok idx -> (
      let values = Nx.create Nx.float64 [| 3 |] [| 10.0; 20.0; 30.0 |] in
      match Cairos.Series.make idx values with
      | Error e -> Alcotest.fail e
      | Ok s -> Alcotest.(check int) "length is 3" 3 (Cairos.Series.length s))

let make_fails_with_mismatched_lengths () =
  match Cairos.Index.daily [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] with
  | Error e -> Alcotest.fail e
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
  | Error e -> Alcotest.fail e
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
  | Error e -> Alcotest.fail e
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
  | Error e -> Alcotest.fail e
  | Ok idx -> (
      let values = Nx.create Nx.float64 [| 0 |] [||] in
      match Cairos.Series.make idx values with
      | Error e -> Alcotest.fail e
      | Ok s -> Alcotest.(check int) "length is 0" 0 (Cairos.Series.length s))

let slice_empty_range_produces_empty_series () =
  match Cairos.Index.daily [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] with
  | Error e -> Alcotest.fail e
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
  ]
