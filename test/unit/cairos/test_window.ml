let mean_f w = Nx.mean w |> Nx.item []

(* --- Rolling --- *)

let rolling_known_values () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
  in
  let result = Cairos.Window.rolling ~n:3 mean_f s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check bool) "pos 0 is nan" true (Float.is_nan vs.(0));
  Alcotest.(check bool) "pos 1 is nan" true (Float.is_nan vs.(1));
  Alcotest.(check (float 0.001)) "pos 2" 2.0 vs.(2);
  Alcotest.(check (float 0.001)) "pos 3" 3.0 vs.(3);
  Alcotest.(check (float 0.001)) "pos 4" 4.0 vs.(4)

let rolling_n_equals_1 () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  let result = Cairos.Window.rolling ~n:1 mean_f s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check (float 0.001)) "pos 0" 10.0 vs.(0);
  Alcotest.(check (float 0.001)) "pos 1" 20.0 vs.(1);
  Alcotest.(check (float 0.001)) "pos 2" 30.0 vs.(2)

let rolling_n_equals_length () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
  in
  let result = Cairos.Window.rolling ~n:5 mean_f s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check bool) "pos 0 is nan" true (Float.is_nan vs.(0));
  Alcotest.(check bool) "pos 1 is nan" true (Float.is_nan vs.(1));
  Alcotest.(check bool) "pos 2 is nan" true (Float.is_nan vs.(2));
  Alcotest.(check bool) "pos 3 is nan" true (Float.is_nan vs.(3));
  Alcotest.(check (float 0.001)) "pos 4" 3.0 vs.(4)

let rolling_n_exceeds_length () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
  in
  let result = Cairos.Window.rolling ~n:10 mean_f s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check int) "length preserved" 5 (Array.length vs);
  Array.iter (fun v -> Alcotest.(check bool) "is nan" true (Float.is_nan v)) vs

let rolling_empty_series () =
  let s = Test_helpers.make_daily_series [||] [||] in
  let result = Cairos.Window.rolling ~n:3 mean_f s in
  Alcotest.(check int) "length is 0" 0 (Cairos.Series.length result)

let rolling_preserves_index () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let result = Cairos.Window.rolling ~n:2 mean_f s in
  let orig_ts = Cairos.Index.timestamps (Cairos.Series.index s) in
  let result_ts = Cairos.Index.timestamps (Cairos.Series.index result) in
  let ptime_testable =
    Alcotest.testable
      (fun ppf t -> Fmt.pf ppf "%a" (Ptime.pp_rfc3339 ()) t)
      Ptime.equal
  in
  Alcotest.(check int) "same length" 3 (Array.length result_ts);
  Alcotest.(check ptime_testable) "ts 0" orig_ts.(0) result_ts.(0);
  Alcotest.(check ptime_testable) "ts 1" orig_ts.(1) result_ts.(1);
  Alcotest.(check ptime_testable) "ts 2" orig_ts.(2) result_ts.(2)

(* --- Expanding --- *)

let expanding_known_values () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
  in
  let result = Cairos.Window.expanding mean_f s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check (float 0.001)) "pos 0" 1.0 vs.(0);
  Alcotest.(check (float 0.001)) "pos 1" 1.5 vs.(1);
  Alcotest.(check (float 0.001)) "pos 2" 2.0 vs.(2);
  Alcotest.(check (float 0.001)) "pos 3" 2.5 vs.(3);
  Alcotest.(check (float 0.001)) "pos 4" 3.0 vs.(4)

let expanding_single_element () =
  let s = Test_helpers.make_daily_series [| "2024-01-01" |] [| 42.0 |] in
  let result = Cairos.Window.expanding mean_f s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check (float 0.001)) "pos 0" 42.0 vs.(0)

let expanding_empty_series () =
  let s = Test_helpers.make_daily_series [||] [||] in
  let result = Cairos.Window.expanding mean_f s in
  Alcotest.(check int) "length is 0" 0 (Cairos.Series.length result)

let expanding_preserves_index () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let result = Cairos.Window.expanding mean_f s in
  let orig_ts = Cairos.Index.timestamps (Cairos.Series.index s) in
  let result_ts = Cairos.Index.timestamps (Cairos.Series.index result) in
  let ptime_testable =
    Alcotest.testable
      (fun ppf t -> Fmt.pf ppf "%a" (Ptime.pp_rfc3339 ()) t)
      Ptime.equal
  in
  Alcotest.(check int) "same length" 3 (Array.length result_ts);
  Alcotest.(check ptime_testable) "ts 0" orig_ts.(0) result_ts.(0);
  Alcotest.(check ptime_testable) "ts 1" orig_ts.(1) result_ts.(1);
  Alcotest.(check ptime_testable) "ts 2" orig_ts.(2) result_ts.(2)

(* --- Convenience wrappers --- *)

let sma_matches_rolling_mean () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
  in
  let sma_result = Cairos.Window.sma ~n:3 s in
  let rolling_result = Cairos.Window.rolling ~n:3 mean_f s in
  let sma_vs = Nx.to_array (Cairos.Series.values sma_result) in
  let rolling_vs = Nx.to_array (Cairos.Series.values rolling_result) in
  Alcotest.(check int) "same length" 5 (Array.length sma_vs);
  Array.iteri
    (fun i _ ->
      if Float.is_nan sma_vs.(i) then
        Alcotest.(check bool)
          (Printf.sprintf "pos %d both nan" i)
          true
          (Float.is_nan rolling_vs.(i))
      else
        Alcotest.(check (float 0.001))
          (Printf.sprintf "pos %d" i)
          rolling_vs.(i) sma_vs.(i))
    sma_vs

let rolling_std_known_values () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
  in
  let result = Cairos.Window.rolling_std ~n:3 s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  let expected_std = sqrt (2.0 /. 3.0) in
  Alcotest.(check bool) "pos 0 is nan" true (Float.is_nan vs.(0));
  Alcotest.(check bool) "pos 1 is nan" true (Float.is_nan vs.(1));
  Alcotest.(check (float 0.001)) "pos 2" expected_std vs.(2);
  Alcotest.(check (float 0.001)) "pos 3" expected_std vs.(3);
  Alcotest.(check (float 0.001)) "pos 4" expected_std vs.(4)

let rolling_min_known_values () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 5.0; 3.0; 4.0; 2.0; 1.0 |]
  in
  let result = Cairos.Window.rolling_min ~n:3 s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check bool) "pos 0 is nan" true (Float.is_nan vs.(0));
  Alcotest.(check bool) "pos 1 is nan" true (Float.is_nan vs.(1));
  Alcotest.(check (float 0.001)) "pos 2" 3.0 vs.(2);
  Alcotest.(check (float 0.001)) "pos 3" 2.0 vs.(3);
  Alcotest.(check (float 0.001)) "pos 4" 1.0 vs.(4)

let rolling_max_known_values () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 1.0; 3.0; 2.0; 5.0; 4.0 |]
  in
  let result = Cairos.Window.rolling_max ~n:3 s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check bool) "pos 0 is nan" true (Float.is_nan vs.(0));
  Alcotest.(check bool) "pos 1 is nan" true (Float.is_nan vs.(1));
  Alcotest.(check (float 0.001)) "pos 2" 3.0 vs.(2);
  Alcotest.(check (float 0.001)) "pos 3" 5.0 vs.(3);
  Alcotest.(check (float 0.001)) "pos 4" 5.0 vs.(4)

let rolling_n_zero_all_nan () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let result = Cairos.Window.rolling ~n:0 mean_f s in
  let vs = Nx.to_array (Cairos.Series.values result) in
  Alcotest.(check int) "length preserved" 3 (Array.length vs);
  Array.iter (fun v -> Alcotest.(check bool) "is nan" true (Float.is_nan v)) vs

let tests =
  [
    ("rolling_known_values", `Quick, rolling_known_values);
    ("rolling_n_equals_1", `Quick, rolling_n_equals_1);
    ("rolling_n_equals_length", `Quick, rolling_n_equals_length);
    ("rolling_n_exceeds_length", `Quick, rolling_n_exceeds_length);
    ("rolling_empty_series", `Quick, rolling_empty_series);
    ("rolling_preserves_index", `Quick, rolling_preserves_index);
    ("expanding_known_values", `Quick, expanding_known_values);
    ("expanding_single_element", `Quick, expanding_single_element);
    ("expanding_empty_series", `Quick, expanding_empty_series);
    ("expanding_preserves_index", `Quick, expanding_preserves_index);
    ("sma_matches_rolling_mean", `Quick, sma_matches_rolling_mean);
    ("rolling_std_known_values", `Quick, rolling_std_known_values);
    ("rolling_min_known_values", `Quick, rolling_min_known_values);
    ("rolling_max_known_values", `Quick, rolling_max_known_values);
    ("rolling_n_zero_all_nan", `Quick, rolling_n_zero_all_nan);
  ]
