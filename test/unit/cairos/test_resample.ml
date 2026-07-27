let ptime_exn s =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> t
  | Error _ -> Alcotest.fail (Printf.sprintf "bad rfc3339: %s" s)

let name_of_any = Test_helpers.name_of_any

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
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
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
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
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
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
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
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
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
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
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
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
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
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
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
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
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
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
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
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
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
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
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
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
  | Ok result ->
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "max" 50.0 vs.(0)

(* --- Error cases --- *)

(* Every rejection below asserts the variant rather than [Error _]: message
   prose is not contractual, so a substring assertion would pin something the
   library does not promise. The sibling variants are enumerated rather than
   collapsed into a wildcard, so a new [Resample.err] constructor breaks this
   file loudly instead of being silently absorbed. *)
let check_target_not_lower what result =
  match result with
  | Ok _ -> Alcotest.fail (Printf.sprintf "%s: expected Error" what)
  | Error (Cairos.Resample.Target_not_lower _) -> ()
  | Error (Cairos.Resample.Unrepresentable_week_start _)
  | Error (Cairos.Resample.Unrepresentable_bucket_timestamp _) ->
      Alcotest.fail (Printf.sprintf "%s: expected Target_not_lower" what)

let rejects_upsampling () =
  let s =
    Test_helpers.make_weekly_series
      [| "2024-01-01"; "2024-01-08" |]
      [| 1.0; 2.0 |]
  in
  check_target_not_lower "weekly -> daily"
    (Cairos.Resample.resample ~agg:`Last Cairos.Freq.Day s)

let rejects_same_frequency () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 1.0; 2.0 |]
  in
  check_target_not_lower "daily -> daily"
    (Cairos.Resample.resample ~agg:`Last Cairos.Freq.Day s)

(* An empty source series is still rejected on an upsample. [resample]
   short-circuits a zero-length input to an empty [Ok] output, so this pins the
   order of the two checks: the rank guard runs first. Reversing them turns an
   invalid upsample into a silent [Ok] for exactly the inputs no other rejection
   test covers — all four use non-empty series, and [empty_series] pairs its
   empty input with a valid downsample. *)
let rejects_upsampling_on_empty_series () =
  let s = Test_helpers.make_weekly_series [||] [||] in
  check_target_not_lower "empty weekly -> daily"
    (Cairos.Resample.resample ~agg:`Last Cairos.Freq.Day s)

(* The rejection payload carries both frequency witnesses. Weekly -> hourly is
   used rather than any pair above because its two witnesses are distinct: a
   construction site that filled [source] from the target (or vice versa) still
   passes a same-frequency or symmetric fixture. *)
let resample_target_not_lower_variant () =
  let s =
    Test_helpers.make_weekly_series
      [| "2024-01-01"; "2024-01-08" |]
      [| 1.0; 2.0 |]
  in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Hour s with
  | Ok _ -> Alcotest.fail "expected Error for weekly -> hourly upsample"
  | Error (Cairos.Resample.Target_not_lower { source; target }) ->
      Alcotest.(check string) "source witness" "Week" (name_of_any source);
      Alcotest.(check string) "target witness" "Hour" (name_of_any target)
  | Error (Cairos.Resample.Unrepresentable_week_start _)
  | Error (Cairos.Resample.Unrepresentable_bucket_timestamp _) ->
      Alcotest.fail "expected Target_not_lower"

(* Every constructor renders something, on one line. The two
   [Unrepresentable_*] variants are unreachable through [resample] but their
   renderer arms are not — they are constructed directly here, which is the
   only way those arms are exercised at all. Asserts existence and shape only:
   message text is not contractual. *)
let resample_err_to_string_nonempty () =
  let errs =
    [
      Cairos.Resample.Target_not_lower
        {
          source = Cairos.Freq.Any Cairos.Freq.Week;
          target = Cairos.Freq.Any Cairos.Freq.Day;
        };
      Cairos.Resample.Unrepresentable_week_start
        { timestamp = ptime_exn "2024-01-03T14:32:11Z" };
      Cairos.Resample.Unrepresentable_bucket_timestamp
        { year = 2024; month = 1; day = 1; hour = 0 };
    ]
  in
  List.iter
    (fun e ->
      let msg = Cairos.Resample.err_to_string e in
      Alcotest.(check bool) "non-empty message" true (String.length msg > 0);
      Alcotest.(check bool)
        "single-line message" true
        (not (String.contains msg '\n')))
    errs

(* Daily -> Minute is a multi-step upsample (rank gap of 2). The contract
   under test is a single branch of the [freq_rank] guard (lib/resample.ml)
   that does not depend on input shape, length, or values, so the rejection is
   deterministic — a single fixed input is sufficient to pin it. Originally
   landed as a [~count:200] QCheck property in
   test_resample_props.ml, then demoted to a
   deterministic Alcotest case here so the [~count:200] declaration in
   the property file does not promise random shape coverage that the
   contract neither needs nor benefits from. *)
let rejects_upsampling_daily_to_minute () =
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  check_target_not_lower "daily -> minute"
    (Cairos.Resample.resample ~agg:`Last Cairos.Freq.Minute s)

(* --- Edge cases --- *)

let empty_series () =
  let s = Test_helpers.make_daily_series [||] [||] in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Week s with
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
  | Ok result ->
      Alcotest.(check int) "empty output" 0 (Cairos.Series.length result)

let single_element () =
  let s = Test_helpers.make_daily_series [| "2024-01-01" |] [| 42.0 |] in
  match Cairos.Resample.resample ~agg:`Mean Cairos.Freq.Week s with
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
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
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
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
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
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
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
  | Ok result ->
      Alcotest.(check int) "1 weekly bucket" 1 (Cairos.Series.length result);
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "sum across year boundary" 60.0 vs.(0);
      let ts = Cairos.Index.timestamps (Cairos.Series.index result) in
      Alcotest.(check bool)
        "boundary is Monday" true
        (Ptime.weekday ts.(0) = `Mon)

(* --- Daily/weekly -> monthly (calendar-month bucketing) --- *)

let daily_to_monthly () =
  (* Daily bars across January and February 2024 group into two calendar
     months, each labelled with the first day of the month at 00:00 UTC. *)
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-15"; "2024-01-16"; "2024-01-17"; "2024-02-10"; "2024-02-11" |]
      [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
  in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Month s with
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
  | Ok result ->
      Alcotest.(check int) "2 monthly points" 2 (Cairos.Series.length result);
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "Jan last" 3.0 vs.(0);
      Alcotest.(check (float 0.001)) "Feb last" 5.0 vs.(1);
      let ts = Cairos.Index.timestamps (Cairos.Series.index result) in
      Alcotest.(check Test_helpers.ptime_testable)
        "Jan boundary"
        (ptime_exn "2024-01-01T00:00:00Z")
        ts.(0);
      Alcotest.(check Test_helpers.ptime_testable)
        "Feb boundary"
        (ptime_exn "2024-02-01T00:00:00Z")
        ts.(1)

let daily_to_monthly_year_boundary () =
  (* January 2024, December 2024, and January 2025 are three distinct
     buckets. A month-only key (ignoring the year) would collapse the two
     Januaries into one; the (year, month) key must not. *)
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-15"; "2024-12-15"; "2025-01-15" |]
      [| 10.0; 20.0; 30.0 |]
  in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Month s with
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
  | Ok result ->
      Alcotest.(check int) "3 monthly buckets" 3 (Cairos.Series.length result);
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "Jan 2024 last" 10.0 vs.(0);
      Alcotest.(check (float 0.001)) "Dec 2024 last" 20.0 vs.(1);
      Alcotest.(check (float 0.001)) "Jan 2025 last" 30.0 vs.(2);
      let ts = Cairos.Index.timestamps (Cairos.Series.index result) in
      Alcotest.(check Test_helpers.ptime_testable)
        "Jan 2024 label"
        (ptime_exn "2024-01-01T00:00:00Z")
        ts.(0);
      Alcotest.(check Test_helpers.ptime_testable)
        "Dec 2024 label"
        (ptime_exn "2024-12-01T00:00:00Z")
        ts.(1);
      Alcotest.(check Test_helpers.ptime_testable)
        "Jan 2025 label"
        (ptime_exn "2025-01-01T00:00:00Z")
        ts.(2)

let daily_to_monthly_variable_month_length () =
  (* February 2025 has 28 days while January and March have 31. A
     fixed-width (e.g. 30-day) bucket implementation passes a single-month
     case but misplaces these boundaries. *)
  let s =
    Test_helpers.make_daily_series
      [| "2025-01-31"; "2025-02-01"; "2025-02-28"; "2025-03-01"; "2025-03-31" |]
      [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
  in
  match Cairos.Resample.resample ~agg:`Sum Cairos.Freq.Month s with
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
  | Ok result ->
      Alcotest.(check int) "3 monthly buckets" 3 (Cairos.Series.length result);
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "Jan sum" 1.0 vs.(0);
      Alcotest.(check (float 0.001)) "Feb sum" 5.0 vs.(1);
      Alcotest.(check (float 0.001)) "Mar sum" 9.0 vs.(2);
      let ts = Cairos.Index.timestamps (Cairos.Series.index result) in
      Alcotest.(check Test_helpers.ptime_testable)
        "Feb label"
        (ptime_exn "2025-02-01T00:00:00Z")
        ts.(1)

let daily_to_monthly_label_not_in_source () =
  (* Source begins on the 15th; the January bucket is still labelled
     2024-01-01, a synthesised anchor absent from the input. *)
  let s =
    Test_helpers.make_daily_series
      [| "2024-01-15"; "2024-01-20"; "2024-02-05" |]
      [| 1.0; 2.0; 3.0 |]
  in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Month s with
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
  | Ok result ->
      Alcotest.(check int) "2 monthly buckets" 2 (Cairos.Series.length result);
      let ts = Cairos.Index.timestamps (Cairos.Series.index result) in
      Alcotest.(check Test_helpers.ptime_testable)
        "synthesised Jan-01 label"
        (ptime_exn "2024-01-01T00:00:00Z")
        ts.(0);
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "Jan last" 2.0 vs.(0);
      Alcotest.(check (float 0.001)) "Feb last" 3.0 vs.(1)

let weekly_to_monthly () =
  (* Weekly (Monday-labelled) bars. The 2024-01-29 week straddles into
     February, but is bucketed by the month of its own label — January. *)
  let s =
    Test_helpers.make_weekly_series
      [|
        "2024-01-01";
        "2024-01-08";
        "2024-01-15";
        "2024-01-22";
        "2024-01-29";
        "2024-02-05";
      |]
      [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0 |]
  in
  match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Month s with
  | Error e -> Alcotest.fail (Cairos.Resample.err_to_string e)
  | Ok result ->
      Alcotest.(check int) "2 monthly buckets" 2 (Cairos.Series.length result);
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001))
        "Jan last (incl. straddling week)" 5.0 vs.(0);
      Alcotest.(check (float 0.001)) "Feb last" 6.0 vs.(1);
      let ts = Cairos.Index.timestamps (Cairos.Series.index result) in
      Alcotest.(check Test_helpers.ptime_testable)
        "Jan label"
        (ptime_exn "2024-01-01T00:00:00Z")
        ts.(0);
      Alcotest.(check Test_helpers.ptime_testable)
        "Feb label"
        (ptime_exn "2024-02-01T00:00:00Z")
        ts.(1)

let rejects_monthly_to_weekly () =
  (* Monthly is the coarsest frequency (rank 4); Monthly -> Weekly is an
     upsample and must be rejected. *)
  let s =
    Test_helpers.make_monthly_series
      [| "2024-01-01"; "2024-02-01"; "2024-03-01" |]
      [| 1.0; 2.0; 3.0 |]
  in
  check_target_not_lower "monthly -> weekly"
    (Cairos.Resample.resample ~agg:`Last Cairos.Freq.Week s)

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
    ( "rejects_upsampling_daily_to_minute",
      `Quick,
      rejects_upsampling_daily_to_minute );
    ( "rejects_upsampling_on_empty_series",
      `Quick,
      rejects_upsampling_on_empty_series );
    ( "resample_target_not_lower_variant",
      `Quick,
      resample_target_not_lower_variant );
    ("resample_err_to_string_nonempty", `Quick, resample_err_to_string_nonempty);
    ("empty_series", `Quick, empty_series);
    ("single_element", `Quick, single_element);
    ("sparse_data_skips_empty_buckets", `Quick, sparse_data_skips_empty_buckets);
    ("week_53_boundary", `Quick, week_53_boundary);
    ("year_boundary_weekly", `Quick, year_boundary_weekly);
    ("daily_to_monthly", `Quick, daily_to_monthly);
    ("daily_to_monthly_year_boundary", `Quick, daily_to_monthly_year_boundary);
    ( "daily_to_monthly_variable_month_length",
      `Quick,
      daily_to_monthly_variable_month_length );
    ( "daily_to_monthly_label_not_in_source",
      `Quick,
      daily_to_monthly_label_not_in_source );
    ("weekly_to_monthly", `Quick, weekly_to_monthly);
    ("rejects_monthly_to_weekly", `Quick, rejects_monthly_to_weekly);
  ]

let () = Alcotest.run "Resample" [ ("Resample", tests) ]
