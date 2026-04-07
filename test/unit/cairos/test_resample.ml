(* --- Frequency transitions (using Last aggregation) --- *)

let daily_to_weekly () =
  (* Mon-Fri week 1 (2024-01-01 is Monday), Mon-Fri week 2 *)
  let s =
    Test_helpers.make_daily_series
      [|
        "2024-01-01";
        "2024-01-02";
        "2024-01-03";
        "2024-01-04";
        "2024-01-05";
        "2024-01-08";
        "2024-01-09";
        "2024-01-10";
        "2024-01-11";
        "2024-01-12";
      |]
      [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0 |]
  in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Week s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      Alcotest.(check int) "2 weekly points" 2 (Cairos.Series.length result);
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "week 1 last" 5.0 vs.(0);
      Alcotest.(check (float 0.001)) "week 2 last" 10.0 vs.(1);
      (* Output timestamps are Monday 00:00 UTC *)
      let ts = Cairos.Index.timestamps (Cairos.Series.index result) in
      let expect_w1 =
        match Ptime.of_rfc3339 "2024-01-01T00:00:00Z" with
        | Ok (t, _, _) -> t
        | Error _ -> Alcotest.fail "bad rfc3339"
      in
      let expect_w2 =
        match Ptime.of_rfc3339 "2024-01-08T00:00:00Z" with
        | Ok (t, _, _) -> t
        | Error _ -> Alcotest.fail "bad rfc3339"
      in
      Alcotest.(check Test_helpers.ptime_testable)
        "week 1 boundary" expect_w1 ts.(0);
      Alcotest.(check Test_helpers.ptime_testable)
        "week 2 boundary" expect_w2 ts.(1)

let minute_to_hourly () =
  (* 120 minute timestamps spanning 2 hours: 10:00-10:59 and 11:00-11:59 *)
  let dates =
    Array.init 120 (fun i ->
        let h = 10 + (i / 60) in
        let m = i mod 60 in
        Printf.sprintf "2024-01-01T%02d:%02d:00Z" h m)
  in
  let values = Array.init 120 (fun i -> Float.of_int (i + 1)) in
  let s = Test_helpers.make_minute_series dates values in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Hour s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      Alcotest.(check int) "2 hourly points" 2 (Cairos.Series.length result);
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "hour 1 last" 60.0 vs.(0);
      Alcotest.(check (float 0.001)) "hour 2 last" 120.0 vs.(1)

let minute_to_daily () =
  (* 1440 minute timestamps (1 full day) *)
  let dates =
    Array.init 1440 (fun i ->
        let h = i / 60 in
        let m = i mod 60 in
        Printf.sprintf "2024-01-01T%02d:%02d:00Z" h m)
  in
  let values = Array.init 1440 (fun i -> Float.of_int (i + 1)) in
  let s = Test_helpers.make_minute_series dates values in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Day s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      Alcotest.(check int) "1 daily point" 1 (Cairos.Series.length result);
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "last value" 1440.0 vs.(0)

let minute_to_weekly () =
  (* Minute data spanning 2 weeks: one point per day at midnight, 14 days *)
  (* 2024-01-01 (Mon) through 2024-01-14 (Sun) = 2 ISO weeks *)
  let dates =
    Array.init 14 (fun i -> Printf.sprintf "2024-01-%02dT00:00:00Z" (i + 1))
  in
  let values = Array.init 14 (fun i -> Float.of_int (i + 1)) in
  let s = Test_helpers.make_minute_series dates values in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Week s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      Alcotest.(check int) "2 weekly points" 2 (Cairos.Series.length result);
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "week 1 last" 7.0 vs.(0);
      Alcotest.(check (float 0.001)) "week 2 last" 14.0 vs.(1)

let hourly_to_daily () =
  (* 48 hourly timestamps (2 days) *)
  let dates =
    Array.init 48 (fun i ->
        let d = 1 + (i / 24) in
        let h = i mod 24 in
        Printf.sprintf "2024-01-%02dT%02d:00:00Z" d h)
  in
  let values = Array.init 48 (fun i -> Float.of_int (i + 1)) in
  let s = Test_helpers.make_hourly_series dates values in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Day s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      Alcotest.(check int) "2 daily points" 2 (Cairos.Series.length result);
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "day 1 last" 24.0 vs.(0);
      Alcotest.(check (float 0.001)) "day 2 last" 48.0 vs.(1)

let hourly_to_weekly () =
  (* Hourly data: one point per day at midnight for 14 days = 2 weeks *)
  let dates =
    Array.init 14 (fun i -> Printf.sprintf "2024-01-%02dT00:00:00Z" (i + 1))
  in
  let values = Array.init 14 (fun i -> Float.of_int (i + 1)) in
  let s = Test_helpers.make_hourly_series dates values in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Week s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      Alcotest.(check int) "2 weekly points" 2 (Cairos.Series.length result);
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "week 1 last" 7.0 vs.(0);
      Alcotest.(check (float 0.001)) "week 2 last" 14.0 vs.(1)

(* --- Aggregation strategies (daily -> weekly, single week Mon-Fri) --- *)

let agg_first () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 10.0; 20.0; 30.0; 40.0; 50.0 |]
  in
  match Cairos.Resample.resample ~agg:`First Cairos.Freq.Week s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "first" 10.0 vs.(0)

let agg_last () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 10.0; 20.0; 30.0; 40.0; 50.0 |]
  in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Week s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "last" 50.0 vs.(0)

let agg_sum () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 10.0; 20.0; 30.0; 40.0; 50.0 |]
  in
  match Cairos.Resample.resample ~agg:`Sum Cairos.Freq.Week s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "sum" 150.0 vs.(0)

let agg_mean () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 10.0; 20.0; 30.0; 40.0; 50.0 |]
  in
  match Cairos.Resample.resample ~agg:`Mean Cairos.Freq.Week s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "mean" 30.0 vs.(0)

let agg_min () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 30.0; 10.0; 50.0; 20.0; 40.0 |]
  in
  match Cairos.Resample.resample ~agg:`Min Cairos.Freq.Week s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "min" 10.0 vs.(0)

let agg_max () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 30.0; 10.0; 50.0; 20.0; 40.0 |]
  in
  match Cairos.Resample.resample ~agg:`Max Cairos.Freq.Week s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "max" 50.0 vs.(0)

(* --- Error cases --- *)

let rejects_upsampling () =
  let s =
    Test_helpers.make_weekly_series
      [| "2024-01-01"; "2024-01-08" |]
      [| 1.0; 2.0 |]
  in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Day s with
  | Ok _ -> Alcotest.fail "expected Error for upsampling"
  | Error _ -> ()

let rejects_same_frequency () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 1.0; 2.0 |]
  in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Day s with
  | Ok _ -> Alcotest.fail "expected Error for same frequency"
  | Error _ -> ()

(* --- Edge cases --- *)

let empty_series () =
  let s = Test_helpers.make_daily_series [||] [||] in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Week s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      Alcotest.(check int) "empty output" 0 (Cairos.Series.length result)

let single_element () =
  let s = Test_helpers.make_daily_series [| "2024-01-01" |] [| 42.0 |] in
  match Cairos.Resample.resample ~agg:`Mean Cairos.Freq.Week s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      Alcotest.(check int) "1 output point" 1 (Cairos.Series.length result);
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "value preserved" 42.0 vs.(0)

let sparse_data_skips_empty_buckets () =
  (* Week 1 (Jan 1-5), skip week 2 entirely, week 3 (Jan 15-19) *)
  let s =
    Test_helpers.make_daily_series
      [|
        "2024-01-01";
        "2024-01-02";
        "2024-01-03";
        "2024-01-04";
        "2024-01-05";
        "2024-01-15";
        "2024-01-16";
        "2024-01-17";
        "2024-01-18";
        "2024-01-19";
      |]
      [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0 |]
  in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Week s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      Alcotest.(check int)
        "2 weekly points (gap skipped)" 2
        (Cairos.Series.length result);
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "week 1 last" 5.0 vs.(0);
      Alcotest.(check (float 0.001)) "week 3 last" 10.0 vs.(1)

(* --- Week 53 / year boundary edge cases --- *)

let week_53_boundary () =
  (* 2020-12-28 (Mon) to 2021-01-03 (Sun) is ISO week 53 of 2020.
     2021-01-04 (Mon) starts ISO week 1 of 2021.
     Daily data across this boundary should produce 2 weekly buckets. *)
  let s =
    Test_helpers.make_daily_series
      [|
        "2020-12-28";
        "2020-12-29";
        "2020-12-30";
        "2020-12-31";
        "2021-01-01";
        "2021-01-02";
        "2021-01-03";
        "2021-01-04";
        "2021-01-05";
      |]
      [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0 |]
  in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Week s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      Alcotest.(check int) "2 weekly buckets" 2 (Cairos.Series.length result);
      let vs = Nx.to_array (Cairos.Series.values result) in
      (* Week 53 of 2020: Dec 28 - Jan 3, last value = 7.0 (Jan 3) *)
      Alcotest.(check (float 0.001)) "week 53 last" 7.0 vs.(0);
      (* Week 1 of 2021: Jan 4-5, last value = 9.0 (Jan 5) *)
      Alcotest.(check (float 0.001)) "week 1 last" 9.0 vs.(1);
      let ts = Cairos.Index.timestamps (Cairos.Series.index result) in
      Alcotest.(check bool)
        "week 53 is Monday" true
        (Ptime.weekday ts.(0) = `Mon);
      Alcotest.(check bool) "week 1 is Monday" true (Ptime.weekday ts.(1) = `Mon)

let year_boundary_weekly () =
  (* 2024-12-30 (Mon) starts the last ISO week of 2024.
     2024-12-31 (Tue) and 2025-01-01 (Wed) are in the same ISO week.
     All 3 days should land in one weekly bucket. *)
  let s =
    Test_helpers.make_daily_series
      [| "2024-12-30"; "2024-12-31"; "2025-01-01" |]
      [| 10.0; 20.0; 30.0 |]
  in
  match Cairos.Resample.resample ~agg:`Sum Cairos.Freq.Week s with
  | Error e -> Alcotest.fail e
  | Ok result ->
      Alcotest.(check int) "1 weekly bucket" 1 (Cairos.Series.length result);
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "sum across year boundary" 60.0 vs.(0);
      let ts = Cairos.Index.timestamps (Cairos.Series.index result) in
      Alcotest.(check bool)
        "boundary is Monday" true
        (Ptime.weekday ts.(0) = `Mon)

(* --- Test list --- *)

let tests =
  [
    ("daily_to_weekly", `Quick, daily_to_weekly);
    ("minute_to_hourly", `Quick, minute_to_hourly);
    ("minute_to_daily", `Quick, minute_to_daily);
    ("minute_to_weekly", `Quick, minute_to_weekly);
    ("hourly_to_daily", `Quick, hourly_to_daily);
    ("hourly_to_weekly", `Quick, hourly_to_weekly);
    ("agg_first", `Quick, agg_first);
    ("agg_last", `Quick, agg_last);
    ("agg_sum", `Quick, agg_sum);
    ("agg_mean", `Quick, agg_mean);
    ("agg_min", `Quick, agg_min);
    ("agg_max", `Quick, agg_max);
    ("rejects_upsampling", `Quick, rejects_upsampling);
    ("rejects_same_frequency", `Quick, rejects_same_frequency);
    ("empty_series", `Quick, empty_series);
    ("single_element", `Quick, single_element);
    ("sparse_data_skips_empty_buckets", `Quick, sparse_data_skips_empty_buckets);
    ("week_53_boundary", `Quick, week_53_boundary);
    ("year_boundary_weekly", `Quick, year_boundary_weekly);
  ]
