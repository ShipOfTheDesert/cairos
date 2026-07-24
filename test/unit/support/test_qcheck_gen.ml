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
    ]
