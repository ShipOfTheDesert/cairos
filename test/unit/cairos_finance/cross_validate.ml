(* Cross-validation infrastructure for cairos_finance.
   Tests CSV parsing and array comparison utilities. *)

let read_fixture path : (float array, string) result =
  match In_channel.with_open_text path In_channel.input_all with
  | exception Sys_error msg -> Error msg
  | content ->
      let lines =
        String.split_on_char '\n' content
        |> List.filter (fun s -> String.length (String.trim s) > 0)
      in
      let rec parse acc line_num = function
        | [] -> Ok (Array.of_list (List.rev acc))
        | line :: rest -> (
            match float_of_string_opt (String.trim line) with
            | Some v -> parse (v :: acc) (line_num + 1) rest
            | None ->
                Error
                  (Printf.sprintf "failed to parse line %d: %S" line_num
                     (String.trim line)))
      in
      parse [] 1 lines

let write_computed path (arr : float array) : unit =
  Out_channel.with_open_text path (fun oc ->
      Array.iter (fun v -> Printf.fprintf oc "%.17g\n" v) arr)

let compare_arrays ~tolerance ~expected ~actual : (unit, string) result =
  (* NaN handling (added for RFC 0022 Risk #2): two NaNs at the same index
     compare equal (the [sharpe] flat-series fixture relies on this), but
     NaN versus a finite value is a mismatch. The previous implementation
     silently passed both cases because [Float.abs nan > tolerance] is
     [false], which let nan-vs-real divergences slip through unnoticed. *)
  let elen = Array.length expected in
  let alen = Array.length actual in
  if elen <> alen then
    Error (Printf.sprintf "length mismatch: expected %d, got %d" elen alen)
  else
    let rec check i =
      if i >= elen then Ok ()
      else
        let e = expected.(i) and a = actual.(i) in
        let e_nan = Float.is_nan e and a_nan = Float.is_nan a in
        if e_nan && a_nan then check (i + 1)
        else if e_nan || a_nan then
          Error
            (Printf.sprintf
               "mismatch at index %d: expected %.17g, got %.17g (NaN)" i e a)
        else
          let diff = Float.abs (e -. a) in
          if diff > tolerance then
            Error
              (Printf.sprintf
                 "mismatch at index %d: expected %.17g, got %.17g (diff %.17g)"
                 i e a diff)
          else check (i + 1)
    in
    check 0

(* --- Tests --- *)

let string_contains haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if not !found then
        if String.sub haystack i nlen = needle then found := true
    done;
    !found

let read_fixture_parses_valid_csv () =
  let tmp = Filename.temp_file "cairos_cv_test_" ".csv" in
  Fun.protect
    ~finally:(fun () -> Sys.remove tmp)
    (fun () ->
      Out_channel.with_open_text tmp (fun oc ->
          List.iter
            (fun v -> Printf.fprintf oc "%.17g\n" v)
            [ 1.5; 2.0; 3.14159 ]);
      match read_fixture tmp with
      | Error e -> Alcotest.fail ("unexpected error: " ^ e)
      | Ok arr ->
          Alcotest.(check int) "length" 3 (Array.length arr);
          Alcotest.(check (float 0.0)) "first" 1.5 arr.(0);
          Alcotest.(check (float 0.0)) "second" 2.0 arr.(1);
          Alcotest.(check (float 1e-15)) "third" 3.14159 arr.(2))

let read_fixture_returns_error_on_malformed_line () =
  let tmp = Filename.temp_file "cairos_cv_test_" ".csv" in
  Fun.protect
    ~finally:(fun () -> Sys.remove tmp)
    (fun () ->
      Out_channel.with_open_text tmp (fun oc ->
          Printf.fprintf oc "1.0\nnot_a_number\n3.0\n");
      match read_fixture tmp with
      | Ok _ -> Alcotest.fail "expected Error for malformed CSV"
      | Error msg ->
          Alcotest.(check bool)
            "error mentions line number" true
            (string_contains msg "line 2"))

let compare_arrays_passes_within_tolerance () =
  let expected = [| 1.0; 2.0; 3.0 |] in
  let actual = [| 1.0 +. 1e-12; 2.0 -. 1e-12; 3.0 +. 1e-11 |] in
  match compare_arrays ~tolerance:1e-10 ~expected ~actual with
  | Ok () -> ()
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)

let compare_arrays_fails_on_divergence () =
  let expected = [| 1.0; 2.0; 3.0 |] in
  let actual = [| 1.0; 2.5; 3.0 |] in
  match compare_arrays ~tolerance:1e-10 ~expected ~actual with
  | Ok () -> Alcotest.fail "expected Error for divergent arrays"
  | Error msg ->
      Alcotest.(check bool)
        "error mentions index" true
        (string_contains msg "index 1")

let compare_arrays_treats_nan_at_same_index_as_equal () =
  let expected = [| 1.0; Float.nan; 3.0 |] in
  let actual = [| 1.0; Float.nan; 3.0 |] in
  match compare_arrays ~tolerance:1e-10 ~expected ~actual with
  | Ok () -> ()
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)

let compare_arrays_fails_when_only_one_side_is_nan () =
  let expected = [| 1.0; Float.nan; 3.0 |] in
  let actual = [| 1.0; 2.0; 3.0 |] in
  match compare_arrays ~tolerance:1e-10 ~expected ~actual with
  | Ok () -> Alcotest.fail "expected Error for nan/non-nan mismatch"
  | Error msg ->
      Alcotest.(check bool)
        "error mentions index" true
        (string_contains msg "index 1")

let compare_arrays_fails_on_length_mismatch () =
  let expected = [| 1.0; 2.0; 3.0 |] in
  let actual = [| 1.0; 2.0 |] in
  match compare_arrays ~tolerance:1e-10 ~expected ~actual with
  | Ok () -> Alcotest.fail "expected Error for length mismatch"
  | Error msg ->
      Alcotest.(check bool)
        "error mentions expected length" true (string_contains msg "3")

(* --- Cross-validation tests --- *)

let fixture_dir = "validation/fixtures"

let load_prices series_name : float array =
  let path = Filename.concat fixture_dir ("input_" ^ series_name ^ ".csv") in
  match read_fixture path with
  | Ok arr -> arr
  | Error msg ->
      Alcotest.fail (Printf.sprintf "load_prices(%s): %s" series_name msg)

let load_reference metric series_name : float array =
  let path =
    Filename.concat fixture_dir (metric ^ "_" ^ series_name ^ ".csv")
  in
  match read_fixture path with
  | Ok arr -> arr
  | Error msg ->
      Alcotest.fail
        (Printf.sprintf "load_reference(%s,%s): %s" metric series_name msg)

let validate_metric ~metric ~series_name ~compute () =
  let prices = load_prices series_name in
  let expected = load_reference metric series_name in
  let series = Finance_test_helpers.make_daily_series prices in
  let returns = Cairos.Series.pct_change series in
  let actual_scalar = compute returns in
  let actual = [| actual_scalar |] in
  match compare_arrays ~tolerance:1e-10 ~expected ~actual with
  | Ok () -> ()
  | Error msg ->
      let tmp =
        Filename.temp_file
          (Printf.sprintf "cairos_cv_%s_%s_" metric series_name)
          ".csv"
      in
      write_computed tmp actual;
      Alcotest.fail
        (Printf.sprintf "%s/%s mismatch: %s\n  expected: %s\n  computed: %s"
           metric series_name msg
           (Filename.concat fixture_dir (metric ^ "_" ^ series_name ^ ".csv"))
           tmp)

let validate_cumulative_return_normal () =
  validate_metric ~metric:"cumulative_return" ~series_name:"normal"
    ~compute:Cairos_finance.cumulative_return ()

let validate_cumulative_return_drawdown () =
  validate_metric ~metric:"cumulative_return" ~series_name:"drawdown"
    ~compute:Cairos_finance.cumulative_return ()

let validate_cumulative_return_flat () =
  validate_metric ~metric:"cumulative_return" ~series_name:"flat"
    ~compute:Cairos_finance.cumulative_return ()

let validate_cumulative_return_extreme () =
  validate_metric ~metric:"cumulative_return" ~series_name:"extreme"
    ~compute:Cairos_finance.cumulative_return ()

let validate_annualised_return_normal () =
  validate_metric ~metric:"annualised_return" ~series_name:"normal"
    ~compute:Cairos_finance.annualised_return ()

let validate_annualised_return_drawdown () =
  validate_metric ~metric:"annualised_return" ~series_name:"drawdown"
    ~compute:Cairos_finance.annualised_return ()

let validate_annualised_return_flat () =
  validate_metric ~metric:"annualised_return" ~series_name:"flat"
    ~compute:Cairos_finance.annualised_return ()

let validate_annualised_return_extreme () =
  validate_metric ~metric:"annualised_return" ~series_name:"extreme"
    ~compute:Cairos_finance.annualised_return ()

let validate_annualised_vol_normal () =
  validate_metric ~metric:"annualised_vol" ~series_name:"normal"
    ~compute:Cairos_finance.annualised_vol ()

let validate_annualised_vol_drawdown () =
  validate_metric ~metric:"annualised_vol" ~series_name:"drawdown"
    ~compute:Cairos_finance.annualised_vol ()

let validate_annualised_vol_flat () =
  validate_metric ~metric:"annualised_vol" ~series_name:"flat"
    ~compute:Cairos_finance.annualised_vol ()

let validate_annualised_vol_extreme () =
  validate_metric ~metric:"annualised_vol" ~series_name:"extreme"
    ~compute:Cairos_finance.annualised_vol ()

let validate_sharpe_rf0_normal () =
  validate_metric ~metric:"sharpe_rf0" ~series_name:"normal"
    ~compute:(Cairos_finance.sharpe ~risk_free:0.0)
    ()

let validate_sharpe_rf0_drawdown () =
  validate_metric ~metric:"sharpe_rf0" ~series_name:"drawdown"
    ~compute:(Cairos_finance.sharpe ~risk_free:0.0)
    ()

let validate_sharpe_rf0_flat () =
  validate_metric ~metric:"sharpe_rf0" ~series_name:"flat"
    ~compute:(Cairos_finance.sharpe ~risk_free:0.0)
    ()

let validate_sharpe_rf0_extreme () =
  validate_metric ~metric:"sharpe_rf0" ~series_name:"extreme"
    ~compute:(Cairos_finance.sharpe ~risk_free:0.0)
    ()

let validate_sharpe_rf4_normal () =
  validate_metric ~metric:"sharpe_rf4" ~series_name:"normal"
    ~compute:(Cairos_finance.sharpe ~risk_free:0.04)
    ()

let validate_sharpe_rf4_drawdown () =
  validate_metric ~metric:"sharpe_rf4" ~series_name:"drawdown"
    ~compute:(Cairos_finance.sharpe ~risk_free:0.04)
    ()

let validate_sharpe_rf4_flat () =
  validate_metric ~metric:"sharpe_rf4" ~series_name:"flat"
    ~compute:(Cairos_finance.sharpe ~risk_free:0.04)
    ()

let validate_sharpe_rf4_extreme () =
  validate_metric ~metric:"sharpe_rf4" ~series_name:"extreme"
    ~compute:(Cairos_finance.sharpe ~risk_free:0.04)
    ()

let () =
  Alcotest.run "cross_validate"
    [
      ( "read_fixture",
        [
          Alcotest.test_case "parses valid CSV" `Quick
            read_fixture_parses_valid_csv;
          Alcotest.test_case "returns error on malformed line" `Quick
            read_fixture_returns_error_on_malformed_line;
        ] );
      ( "compare_arrays",
        [
          Alcotest.test_case "passes within tolerance" `Quick
            compare_arrays_passes_within_tolerance;
          Alcotest.test_case "fails on divergence" `Quick
            compare_arrays_fails_on_divergence;
          Alcotest.test_case "fails on length mismatch" `Quick
            compare_arrays_fails_on_length_mismatch;
          Alcotest.test_case "treats nan at same index as equal" `Quick
            compare_arrays_treats_nan_at_same_index_as_equal;
          Alcotest.test_case "fails when only one side is nan" `Quick
            compare_arrays_fails_when_only_one_side_is_nan;
        ] );
      ( "cumulative_return",
        [
          Alcotest.test_case "normal vs Pandas" `Quick
            validate_cumulative_return_normal;
          Alcotest.test_case "drawdown vs Pandas" `Quick
            validate_cumulative_return_drawdown;
          Alcotest.test_case "flat vs Pandas" `Quick
            validate_cumulative_return_flat;
          Alcotest.test_case "extreme vs Pandas" `Quick
            validate_cumulative_return_extreme;
        ] );
      ( "annualised_return",
        [
          Alcotest.test_case "normal vs Pandas" `Quick
            validate_annualised_return_normal;
          Alcotest.test_case "drawdown vs Pandas" `Quick
            validate_annualised_return_drawdown;
          Alcotest.test_case "flat vs Pandas" `Quick
            validate_annualised_return_flat;
          Alcotest.test_case "extreme vs Pandas" `Quick
            validate_annualised_return_extreme;
        ] );
      ( "annualised_vol",
        [
          Alcotest.test_case "normal vs Pandas" `Quick
            validate_annualised_vol_normal;
          Alcotest.test_case "drawdown vs Pandas" `Quick
            validate_annualised_vol_drawdown;
          Alcotest.test_case "flat vs Pandas" `Quick
            validate_annualised_vol_flat;
          Alcotest.test_case "extreme vs Pandas" `Quick
            validate_annualised_vol_extreme;
        ] );
      ( "sharpe_rf0",
        [
          Alcotest.test_case "normal vs Pandas" `Quick
            validate_sharpe_rf0_normal;
          Alcotest.test_case "drawdown vs Pandas" `Quick
            validate_sharpe_rf0_drawdown;
          Alcotest.test_case "flat vs Pandas" `Quick validate_sharpe_rf0_flat;
          Alcotest.test_case "extreme vs Pandas" `Quick
            validate_sharpe_rf0_extreme;
        ] );
      ( "sharpe_rf4",
        [
          Alcotest.test_case "normal vs Pandas" `Quick
            validate_sharpe_rf4_normal;
          Alcotest.test_case "drawdown vs Pandas" `Quick
            validate_sharpe_rf4_drawdown;
          Alcotest.test_case "flat vs Pandas" `Quick validate_sharpe_rf4_flat;
          Alcotest.test_case "extreme vs Pandas" `Quick
            validate_sharpe_rf4_extreme;
        ] );
    ]
