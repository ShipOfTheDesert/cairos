(* Unit tests for the generator-support library itself.

   [test_series_scan.ml] pins [float_approx_equal]'s branches on its own local
   copy of the comparator, not on the [Qcheck_gen] one that the property
   suites consume. An untested comparator can make a property vacuously
   green — [map2_nan_nan_exactly_at_union_of_input_nans] in
   [test_align_props.ml] rests on both NaN branches behaving as documented. *)

let float_approx_equal_both_nan_are_equal () =
  Alcotest.(check bool)
    "both nan compare equal" true
    (Qcheck_gen.float_approx_equal ~tol:1e-10 Float.nan Float.nan)

let float_approx_equal_one_sided_nan_is_unequal () =
  Alcotest.(check bool)
    "nan vs finite is unequal" false
    (Qcheck_gen.float_approx_equal ~tol:1e-10 Float.nan 1.0);
  Alcotest.(check bool)
    "finite vs nan is unequal" false
    (Qcheck_gen.float_approx_equal ~tol:1e-10 1.0 Float.nan)

(* Pins the [make_series_from_floats ~freq:Month] calendar-step synthesis the
   monthly-resample property rests on. An untested Month arm can make that
   property vacuously green (or crash) — same rationale as the comparator tests
   above. Asserts the day-1 anchor, the 2020-01 origin, midnight-UTC time, the
   year rollover (exercising [i / 12] / [i mod 12]), and monotonicity. A
   fixed-seconds proxy (a constant 30- or 91-day step) fails the year rollover
   and the day-1 anchor here. *)
let make_series_monthly_is_calendar_stepped () =
  let n = 14 in
  let s =
    Qcheck_gen.make_series_from_floats ~freq:Cairos.Freq.Month
      (Array.init n (fun i -> float_of_int i))
  in
  let ts = Cairos.Index.timestamps (Cairos.Series.index s) in
  Alcotest.(check int) "one timestamp per input value" n (Array.length ts);
  let check_month label idx (expected_year, expected_month) =
    let (y, m, d), time_of_day = Ptime.to_date_time ts.(idx) in
    Alcotest.(check (triple int int int))
      (label ^ ": (year, month, day)")
      (expected_year, expected_month, 1)
      (y, m, d);
    Alcotest.(check bool)
      (label ^ ": midnight UTC") true
      (time_of_day = ((0, 0, 0), 0))
  in
  check_month "first (origin)" 0 (2020, 1);
  check_month "december" 11 (2020, 12);
  check_month "year rollover" 12 (2021, 1);
  check_month "after rollover" 13 (2021, 2);
  let strictly_monotonic = ref true in
  for i = 1 to n - 1 do
    if not (Ptime.is_later ts.(i) ~than:ts.(i - 1)) then
      strictly_monotonic := false
  done;
  Alcotest.(check bool) "strictly monotonic" true !strictly_monotonic

let () =
  Alcotest.run "qcheck_gen"
    [
      ( "float_approx_equal",
        [
          Alcotest.test_case "both nan are equal" `Quick
            float_approx_equal_both_nan_are_equal;
          Alcotest.test_case "one-sided nan is unequal" `Quick
            float_approx_equal_one_sided_nan_is_unequal;
        ] );
      ( "make_series_from_floats",
        [
          Alcotest.test_case "monthly is calendar-stepped" `Quick
            make_series_monthly_is_calendar_stepped;
        ] );
    ]
