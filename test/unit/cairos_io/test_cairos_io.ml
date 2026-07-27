let fixture name = Filename.concat "fixtures" name
let values_to_array s = Nx.to_array (Cairos.Series.values s)

let first_timestamp_from_date_string date =
  match Cairos.Index.daily [| date |] with
  | Error e -> Alcotest.fail ("test setup: " ^ Cairos.Index.err_to_string e)
  | Ok idx -> (Cairos.Index.timestamps idx).(0)

let ptime_testable =
  Alcotest.testable
    (fun ppf t -> Fmt.pf ppf "%a" (Ptime.pp_rfc3339 ()) t)
    Ptime.equal

let contains haystack needle =
  let hl = String.length haystack and nl = String.length needle in
  if nl = 0 then true
  else
    let rec loop i =
      if i + nl > hl then false
      else if String.sub haystack i nl = needle then true
      else loop (i + 1)
    in
    loop 0

let error_variant ~label result =
  match result with
  | Ok _ -> Alcotest.fail (label ^ ": expected Error, got Ok")
  | Error e -> e

let error_message ~label result =
  Cairos_io.err_to_string (error_variant ~label result)

(* Structural projection of [Cairos_io.err]: the constructor name and its
   payload fields, as an ordinary value the variant tests compare against.
   Asserting on this rather than on the rendered message keeps message prose
   out of the contract, and the match carries no wildcard arm, so a new
   constructor is a compile error here rather than something a test silently
   accepts.

   The sibling suites (test_series.ml, test_align.ml, test_frame.ml) spell the
   same assertion as a per-test "expected X, got Y" arm on the match itself.
   That does not scale to fourteen constructors — it would need a
   thirteen-constructor rejection arm at each of the eight assertion sites
   below — so the enumeration lives here once instead. *)
let describe (e : Cairos_io.err) =
  let i = string_of_int in
  match e with
  | Cairos_io.File_not_found { path; cause } ->
      ("File_not_found", [ ("path", path); ("cause", cause) ])
  | Cairos_io.Empty_file { path } -> ("Empty_file", [ ("path", path) ])
  | Cairos_io.Header_only { path } -> ("Header_only", [ ("path", path) ])
  | Cairos_io.Too_few_columns { line_no; expected; found } ->
      ( "Too_few_columns",
        [ ("line_no", i line_no); ("expected", i expected); ("found", i found) ]
      )
  | Cairos_io.Unparseable_timestamp { line_no; raw } ->
      ("Unparseable_timestamp", [ ("line_no", i line_no); ("raw", raw) ])
  | Cairos_io.Non_monotonic_timestamps { line_no } ->
      ("Non_monotonic_timestamps", [ ("line_no", i line_no) ])
  | Cairos_io.Non_finite_price { line_no; raw } ->
      ("Non_finite_price", [ ("line_no", i line_no); ("raw", raw) ])
  | Cairos_io.Unparseable_float_in_cell { line_no; col; raw } ->
      ( "Unparseable_float_in_cell",
        [ ("line_no", i line_no); ("col", i col); ("raw", raw) ] )
  | Cairos_io.Duplicate_header { col_a; col_b; name } ->
      ( "Duplicate_header",
        [ ("col_a", i col_a); ("col_b", i col_b); ("name", name) ] )
  | Cairos_io.Invalid_column_arg { arg; value } ->
      let arg_name =
        match arg with
        | Cairos_io.Timestamp_col -> "Timestamp_col"
        | Cairos_io.Price_col -> "Price_col"
      in
      ("Invalid_column_arg", [ ("arg", arg_name); ("value", i value) ])
  | Cairos_io.Duplicate_column_arg { value } ->
      ("Duplicate_column_arg", [ ("value", i value) ])
  | Cairos_io.Empty_frame_columns { path } ->
      ("Empty_frame_columns", [ ("path", path) ])
  | Cairos_io.Series_error inner ->
      ("Series_error", [ ("inner", Cairos.Series.err_to_string inner) ])
  | Cairos_io.Frame_error inner ->
      ("Frame_error", [ ("inner", Cairos.Frame.err_to_string inner) ])

let variant_testable = Alcotest.(pair string (list (pair string string)))

let check_variant ~label ~expect result =
  Alcotest.check variant_testable label expect
    (describe (error_variant ~label result))

let check_needles ~label ~needles msg =
  List.iter
    (fun needle ->
      Alcotest.(check bool)
        (label ^ ": message contains " ^ needle ^ " — was: " ^ msg)
        true (contains msg needle))
    needles

(* Retained for the tests whose subject is the failure *kind* alone —
   [Empty_file], [Header_only], [Empty_frame_columns], [File_not_found] — where
   the needle and the constructor name carry the same information and
   converting buys nothing. Every test that asserts a *payload* goes through
   [check_variant] instead: line numbers, column indices and offending cells
   are contract (cairos_io.mli documents the 1-indexed line number), and
   asserting them as substrings both re-pins prose that ADR 0061 declares
   non-contractual and aliases — "line 4" is a substring of "line 40", and
   "expected at least 2" of "expected at least 20", so a fixture growing past
   ten lines silently weakens the assertion.

   [File_not_found] is the one payload-carrying exception: its [cause] is the
   OS errno string, so a structural assertion would pin platform text. Its
   needles name the path only. *)
let check_error ~label ~needles result =
  check_needles ~label ~needles (error_message ~label result)

let of_csv_standard_shape_daily_loads_series () =
  let path = fixture "single_daily_standard.csv" in
  match Cairos_io.of_csv ~freq:Cairos.Freq.Day path with
  | Error e -> Alcotest.fail (Cairos_io.err_to_string e)
  | Ok series ->
      Alcotest.(check int) "series length" 5 (Cairos.Series.length series);
      let expected = [| 100.0; 101.5; 102.0; 100.75; 103.5 |] in
      let actual = values_to_array series in
      Alcotest.(check (array (float 0.0))) "price values" expected actual;
      let first_ts =
        (Cairos.Index.timestamps (Cairos.Series.index series)).(0)
      in
      Alcotest.(check ptime_testable)
        "first timestamp"
        (first_timestamp_from_date_string "2024-01-02")
        first_ts

let of_csv_with_no_header_explicit_cols () =
  let standard_path = fixture "single_daily_standard.csv" in
  let swapped_path = fixture "single_no_header_swapped.csv" in
  match Cairos_io.of_csv ~freq:Cairos.Freq.Day standard_path with
  | Error e -> Alcotest.fail ("standard load: " ^ Cairos_io.err_to_string e)
  | Ok standard -> (
      match
        Cairos_io.of_csv_with ~freq:Cairos.Freq.Day ~header:false
          ~timestamp_col:1 ~price_col:0 swapped_path
      with
      | Error e -> Alcotest.fail ("swapped load: " ^ Cairos_io.err_to_string e)
      | Ok swapped ->
          Alcotest.(check int)
            "length equality"
            (Cairos.Series.length standard)
            (Cairos.Series.length swapped);
          Alcotest.(check (array (float 0.0)))
            "values equality" (values_to_array standard)
            (values_to_array swapped);
          Alcotest.(check (array ptime_testable))
            "timestamps equality"
            (Cairos.Index.timestamps (Cairos.Series.index standard))
            (Cairos.Index.timestamps (Cairos.Series.index swapped)))

let of_csv_missing_file_returns_error_with_path () =
  let path = "does_not_exist.csv" in
  Cairos_io.of_csv ~freq:Cairos.Freq.Day path
  |> check_error ~label:"missing file"
       ~needles:[ "file not found"; "does_not_exist.csv" ]

let of_csv_empty_file_returns_error () =
  Cairos_io.of_csv ~freq:Cairos.Freq.Day (fixture "empty.csv")
  |> check_error ~label:"empty file" ~needles:[ "empty file" ]

let of_csv_header_only_file_returns_error () =
  Cairos_io.of_csv ~freq:Cairos.Freq.Day (fixture "header_only.csv")
  |> check_error ~label:"header-only" ~needles:[ "header-only" ]

let of_csv_row_shorter_than_needed_reports_line () =
  Cairos_io.of_csv ~freq:Cairos.Freq.Day (fixture "single_short_row_at_4.csv")
  |> check_variant ~label:"short row at line 4"
       ~expect:
         ( "Too_few_columns",
           [ ("line_no", "4"); ("expected", "2"); ("found", "1") ] )

let of_csv_nan_price_returns_error () =
  Cairos_io.of_csv ~freq:Cairos.Freq.Day (fixture "single_nan_price_at_5.csv")
  |> check_variant ~label:"nan price at line 5"
       ~expect:("Non_finite_price", [ ("line_no", "5"); ("raw", "nan") ])

let of_csv_inf_price_returns_error () =
  Cairos_io.of_csv ~freq:Cairos.Freq.Day (fixture "single_inf_price_at_6.csv")
  |> check_variant ~label:"inf price at line 6"
       ~expect:("Non_finite_price", [ ("line_no", "6"); ("raw", "inf") ])

let of_csv_unparseable_price_returns_error () =
  Cairos_io.of_csv ~freq:Cairos.Freq.Day (fixture "single_bad_price_at_2.csv")
  |> check_variant ~label:"unparseable price at line 2"
       ~expect:("Non_finite_price", [ ("line_no", "2"); ("raw", "N/A") ])

(* Covers all four column-argument rejection sites — three in [of_csv_with],
   one in [frame_of_csv_with].

   Every argument value is distinct (-1, -2, 3, -3), which pins the [value]
   each site carries: a site reporting a neighbouring column's value or a
   hardcoded constant passes a same-value fixture but fails this one. The [arg]
   field pins which argument each of the three negative sites names, and the
   two variants are told apart by the constructor rather than by the prose they
   share. *)
let cairos_io_invalid_column_arg_variants () =
  let path = fixture "single_daily_standard.csv" in
  check_variant ~label:"negative timestamp_col"
    ~expect:("Invalid_column_arg", [ ("arg", "Timestamp_col"); ("value", "-1") ])
    (Cairos_io.of_csv_with ~freq:Cairos.Freq.Day ~header:true
       ~timestamp_col:(-1) ~price_col:1 path);
  check_variant ~label:"negative price_col"
    ~expect:("Invalid_column_arg", [ ("arg", "Price_col"); ("value", "-2") ])
    (Cairos_io.of_csv_with ~freq:Cairos.Freq.Day ~header:true ~timestamp_col:0
       ~price_col:(-2) path);
  check_variant ~label:"equal cols"
    ~expect:("Duplicate_column_arg", [ ("value", "3") ])
    (Cairos_io.of_csv_with ~freq:Cairos.Freq.Day ~header:true ~timestamp_col:3
       ~price_col:3 path);
  check_variant ~label:"frame negative timestamp_col"
    ~expect:("Invalid_column_arg", [ ("arg", "Timestamp_col"); ("value", "-3") ])
    (Cairos_io.frame_of_csv_with ~freq:Cairos.Freq.Day ~header:true
       ~timestamp_col:(-3)
       (fixture "frame_three_tickers.csv"))

(* The two failures whose line number is translated rather than carried: an
   [Index.err] holds a 0-indexed position into the timestamp array, and
   cairos_io reports the 1-indexed CSV line the user can act on. Both fixtures
   put the offending row where position and line number differ (1 against 3, 2
   against 4), so an implementation that passed the position through — or that
   applied the wrong header offset — fails here.

   Both loading paths are covered because the translation is applied
   separately in each. Subsumes the four earlier tests that asserted only that
   "line 3" or "line 4" appeared somewhere in the message. *)
let cairos_io_line_numbers_preserved () =
  check_variant ~label:"single-series unparseable timestamp"
    ~expect:
      ("Unparseable_timestamp", [ ("line_no", "3"); ("raw", "not-a-date") ])
    (Cairos_io.of_csv ~freq:Cairos.Freq.Day (fixture "single_bad_ts_at_3.csv"));
  check_variant ~label:"single-series non-monotonic timestamps"
    ~expect:("Non_monotonic_timestamps", [ ("line_no", "4") ])
    (Cairos_io.of_csv ~freq:Cairos.Freq.Day
       (fixture "single_non_monotonic.csv"));
  check_variant ~label:"frame unparseable timestamp"
    ~expect:
      ("Unparseable_timestamp", [ ("line_no", "4"); ("raw", "not-a-date") ])
    (Cairos_io.frame_of_csv ~freq:Cairos.Freq.Day
       (fixture "frame_bad_ts_at_4.csv"));
  check_variant ~label:"frame non-monotonic timestamps"
    ~expect:("Non_monotonic_timestamps", [ ("line_no", "4") ])
    (Cairos_io.frame_of_csv ~freq:Cairos.Freq.Day
       (fixture "frame_non_monotonic.csv"))

(* [Series_error] and [Frame_error] are unreachable through the four public
   constructors — both loading paths build their values tensor as
   [Nx.create Nx.float64 [| n |] xs] with [n] the row count, every frame column
   is built from the same index value, and column names are duplicate-checked
   before the frame is assembled — so direct construction is the only way these
   renderer arms are exercised at all.

   The assertion is that the renderer recurses: the message must carry the
   cairos_io layer and the inner module's own rendering, so one call yields a
   complete message. A renderer that reported only the inner message loses the
   layer that rejected; one that reported only "a series error" loses which. *)
let cairos_io_series_error_wraps () =
  let series_err =
    Cairos.Series.Length_mismatch { index_length = 3; values_length = 5 }
  in
  let frame_err = Cairos.Frame.Duplicate_column { name = "AAPL" } in
  check_needles ~label:"Series_error"
    ~needles:[ "cairos_io"; Cairos.Series.err_to_string series_err ]
    (Cairos_io.err_to_string (Cairos_io.Series_error series_err));
  check_needles ~label:"Frame_error"
    ~needles:[ "cairos_io"; Cairos.Frame.err_to_string frame_err ]
    (Cairos_io.err_to_string (Cairos_io.Frame_error frame_err))

(* Every constructor renders. The list is the whole type: an arm that raised or
   returned the empty string would otherwise ship unnoticed, since ten of the
   fourteen are only reachable through a fixture and two are not reachable at
   all. Single-line because the message is one line of a log, not a report. *)
let cairos_io_err_to_string_nonempty () =
  let renders e =
    let msg = Cairos_io.err_to_string e in
    let tag = fst (describe e) in
    Alcotest.(check bool) (tag ^ ": non-empty") true (String.length msg > 0);
    Alcotest.(check bool)
      (tag ^ ": single line") true
      (not (String.contains msg '\n'))
  in
  List.iter renders
    [
      Cairos_io.File_not_found { path = "a.csv"; cause = "No such file" };
      Cairos_io.Empty_file { path = "a.csv" };
      Cairos_io.Header_only { path = "a.csv" };
      Cairos_io.Too_few_columns { line_no = 4; expected = 2; found = 1 };
      Cairos_io.Unparseable_timestamp { line_no = 3; raw = "not-a-date" };
      Cairos_io.Non_monotonic_timestamps { line_no = 4 };
      Cairos_io.Non_finite_price { line_no = 5; raw = "nan" };
      Cairos_io.Unparseable_float_in_cell { line_no = 3; col = 2; raw = "x" };
      Cairos_io.Duplicate_header { col_a = 1; col_b = 3; name = "AAPL" };
      Cairos_io.Invalid_column_arg { arg = Cairos_io.Price_col; value = -2 };
      Cairos_io.Duplicate_column_arg { value = 3 };
      Cairos_io.Empty_frame_columns { path = "a.csv" };
      Cairos_io.Series_error Cairos.Series.Zero_dimensional_values;
      Cairos_io.Frame_error (Cairos.Frame.Duplicate_column { name = "AAPL" });
    ]

let of_csv_with_narrow_first_row_reports_line () =
  (* First data row of [single_daily_standard.csv] has 2 columns; asking for
     [~price_col:2] needs 3. Mirrors the frame [narrow_first_row] test on the
     single-series path. *)
  Cairos_io.of_csv_with ~freq:Cairos.Freq.Day ~header:true ~timestamp_col:0
    ~price_col:2
    (fixture "single_daily_standard.csv")
  |> check_variant ~label:"single-series narrow first row"
       ~expect:
         ( "Too_few_columns",
           [ ("line_no", "2"); ("expected", "3"); ("found", "2") ] )

let get_series name frame =
  match Cairos.Frame.get name frame with
  | Some s -> s
  | None -> Alcotest.fail (Printf.sprintf "column %S missing from frame" name)

let frame_of_csv_standard_shape_loads_columns_by_header () =
  match
    Cairos_io.frame_of_csv ~freq:Cairos.Freq.Day
      (fixture "frame_three_tickers.csv")
  with
  | Error e -> Alcotest.fail (Cairos_io.err_to_string e)
  | Ok frame ->
      Alcotest.(check (list string))
        "columns in source-file order" [ "AAPL"; "MSFT"; "GOOG" ]
        (Cairos.Nonempty.to_list (Cairos.Frame.columns frame));
      let aapl = get_series "AAPL" frame in
      let msft = get_series "MSFT" frame in
      let goog = get_series "GOOG" frame in
      Alcotest.(check int) "AAPL length" 5 (Cairos.Series.length aapl);
      Alcotest.(check (array (float 0.0)))
        "AAPL values"
        [| 185.64; 184.25; 181.91; 181.18; 185.56 |]
        (values_to_array aapl);
      Alcotest.(check (array (float 0.0)))
        "MSFT values"
        [| 372.52; 370.60; 368.79; 367.75; 374.69 |]
        (values_to_array msft);
      Alcotest.(check (array (float 0.0)))
        "GOOG values"
        [| 140.93; 139.84; 137.42; 137.38; 140.12 |]
        (values_to_array goog)

let frame_of_csv_with_no_header_uses_positional_names () =
  match
    Cairos_io.frame_of_csv_with ~freq:Cairos.Freq.Day ~header:false
      ~timestamp_col:0
      (fixture "frame_no_header.csv")
  with
  | Error e -> Alcotest.fail (Cairos_io.err_to_string e)
  | Ok frame ->
      Alcotest.(check (list string))
        "positional column names"
        [ "col_1"; "col_2"; "col_3" ]
        (Cairos.Nonempty.to_list (Cairos.Frame.columns frame))

let frame_of_csv_nan_fills_offset_listings () =
  match
    Cairos_io.frame_of_csv ~freq:Cairos.Freq.Day
      (fixture "frame_offset_listings.csv")
  with
  | Error e -> Alcotest.fail (Cairos_io.err_to_string e)
  | Ok frame ->
      let b = get_series "B" frame in
      Alcotest.(check int) "B length" 5 (Cairos.Series.length b);
      let values = values_to_array b in
      Alcotest.(check bool) "B[0] is NaN" true (Float.is_nan values.(0));
      Alcotest.(check bool) "B[1] is NaN" true (Float.is_nan values.(1));
      Alcotest.(check (float 0.0)) "B[2] = 50.0" 50.0 values.(2);
      Alcotest.(check (float 0.0)) "B[3] = 51.0" 51.0 values.(3);
      Alcotest.(check (float 0.0)) "B[4] = 52.0" 52.0 values.(4)

let frame_of_csv_infinity_in_cell_is_accepted () =
  match
    Cairos_io.frame_of_csv ~freq:Cairos.Freq.Day (fixture "frame_with_inf.csv")
  with
  | Error e -> Alcotest.fail (Cairos_io.err_to_string e)
  | Ok frame ->
      let y = get_series "Y" frame in
      let v = (values_to_array y).(1) in
      Alcotest.(check bool)
        "Y[1] is positive infinity (not finite, > 0)" true
        ((not (Float.is_finite v)) && v > 0.0)

let frame_of_csv_duplicate_header_returns_error () =
  Cairos_io.frame_of_csv ~freq:Cairos.Freq.Day
    (fixture "frame_duplicate_header.csv")
  |> check_variant ~label:"duplicate header"
       ~expect:
         ( "Duplicate_header",
           [ ("col_a", "1"); ("col_b", "2"); ("name", "AAPL") ] )

let frame_of_csv_unparseable_present_cell_reports_line_and_col () =
  Cairos_io.frame_of_csv ~freq:Cairos.Freq.Day
    (fixture "frame_bad_cell_at_3_2.csv")
  |> check_variant ~label:"unparseable cell at line 3 col 2"
       ~expect:
         ( "Unparseable_float_in_cell",
           [ ("line_no", "3"); ("col", "2"); ("raw", "garbage") ] )

let frame_of_csv_empty_file_returns_error () =
  Cairos_io.frame_of_csv ~freq:Cairos.Freq.Day (fixture "empty.csv")
  |> check_error ~label:"frame empty file" ~needles:[ "empty file" ]

let frame_of_csv_header_only_returns_error () =
  Cairos_io.frame_of_csv ~freq:Cairos.Freq.Day (fixture "frame_header_only.csv")
  |> check_error ~label:"frame header-only" ~needles:[ "header-only" ]

let frame_of_csv_with_short_row_reports_line () =
  Cairos_io.frame_of_csv_with ~freq:Cairos.Freq.Day ~header:true
    ~timestamp_col:2
    (fixture "frame_short_row_at_4.csv")
  |> check_variant ~label:"frame short row at line 4"
       ~expect:
         ( "Too_few_columns",
           [ ("line_no", "4"); ("expected", "3"); ("found", "1") ] )

let frame_of_csv_with_narrow_first_row_returns_error () =
  Cairos_io.frame_of_csv_with ~freq:Cairos.Freq.Day ~header:false
    ~timestamp_col:3
    (fixture "frame_narrow_first_row.csv")
  |> check_variant ~label:"frame narrow first row"
       ~expect:
         ( "Too_few_columns",
           [ ("line_no", "1"); ("expected", "4"); ("found", "2") ] )

let frame_of_csv_with_no_header_wider_subsequent_rows_truncates_silently () =
  (* Documents current behaviour: when [~header:false], [collect_frame_columns]
     derives the column count from row 1's width. Cells in subsequent rows
     beyond that width are silently ignored. Pins the contract so a future
     change (e.g. erroring on width-drift) is a deliberate decision, not an
     accident. *)
  match
    Cairos_io.frame_of_csv_with ~freq:Cairos.Freq.Day ~header:false
      ~timestamp_col:0
      (fixture "frame_no_header_wider_subsequent.csv")
  with
  | Error e -> Alcotest.fail (Cairos_io.err_to_string e)
  | Ok frame ->
      Alcotest.(check (list string))
        "columns derived from first row width" [ "col_1"; "col_2" ]
        (Cairos.Nonempty.to_list (Cairos.Frame.columns frame));
      let c1 = get_series "col_1" frame in
      let c2 = get_series "col_2" frame in
      Alcotest.(check (array (float 0.0)))
        "col_1 values: extras in row 2 ignored" [| 100.0; 101.0; 102.0 |]
        (values_to_array c1);
      Alcotest.(check (array (float 0.0)))
        "col_2 values: extras in row 2 ignored" [| 200.0; 201.0; 202.0 |]
        (values_to_array c2)

let of_csv_quoted_field_is_unsupported () =
  (* Pins the "Quoting: none" boundary documented in cairos_io.mli. A
     timestamp wrapped in literal double quotes is not stripped — it flows
     to Index.daily verbatim and surfaces as Unparseable_timestamp at line 2.
     A future change that adopts ocaml-csv would need to delete or re-purpose
     this test. *)
  Cairos_io.of_csv ~freq:Cairos.Freq.Day (fixture "single_quoted_timestamp.csv")
  |> check_variant ~label:"quoted timestamp not supported"
       ~expect:
         ( "Unparseable_timestamp",
           [ ("line_no", "2"); ("raw", "\"2024-01-02\"") ] )

let frame_of_csv_with_timestamp_only_file_returns_empty_frame_columns () =
  (* Single-column file with [~header:false ~timestamp_col:0] leaves zero
     instrument columns after filtering — exercises the [Empty_frame_columns]
     branch that guards [Nonempty.of_list []]. *)
  Cairos_io.frame_of_csv_with ~freq:Cairos.Freq.Day ~header:false
    ~timestamp_col:0
    (fixture "frame_timestamps_only.csv")
  |> check_error ~label:"frame with only a timestamp column"
       ~needles:[ "no instrument columns" ]

let of_csv_minute_loads_series () =
  let path = fixture "single_minute_standard.csv" in
  match Cairos_io.of_csv ~freq:Cairos.Freq.Minute path with
  | Error e -> Alcotest.fail (Cairos_io.err_to_string e)
  | Ok series ->
      Alcotest.(check int)
        "minute series length" 3
        (Cairos.Series.length series);
      Alcotest.(check (array (float 0.0)))
        "minute price values" [| 100.0; 101.0; 102.0 |] (values_to_array series)

let of_csv_hourly_loads_series () =
  let path = fixture "single_hourly_standard.csv" in
  match Cairos_io.of_csv ~freq:Cairos.Freq.Hour path with
  | Error e -> Alcotest.fail (Cairos_io.err_to_string e)
  | Ok series ->
      Alcotest.(check int)
        "hourly series length" 3
        (Cairos.Series.length series);
      Alcotest.(check (array (float 0.0)))
        "hourly price values" [| 100.0; 101.0; 102.0 |] (values_to_array series)

let of_csv_weekly_loads_series () =
  let path = fixture "single_weekly_standard.csv" in
  match Cairos_io.of_csv ~freq:Cairos.Freq.Week path with
  | Error e -> Alcotest.fail (Cairos_io.err_to_string e)
  | Ok series ->
      Alcotest.(check int)
        "weekly series length" 3
        (Cairos.Series.length series);
      Alcotest.(check (array (float 0.0)))
        "weekly price values" [| 100.0; 101.0; 102.0 |] (values_to_array series)

let of_csv_daily_crlf_line_endings_match_lf () =
  let lf_path = fixture "single_daily_standard.csv" in
  let crlf_path = fixture "single_daily_crlf.csv" in
  match Cairos_io.of_csv ~freq:Cairos.Freq.Day lf_path with
  | Error e -> Alcotest.fail ("lf load: " ^ Cairos_io.err_to_string e)
  | Ok lf -> (
      match Cairos_io.of_csv ~freq:Cairos.Freq.Day crlf_path with
      | Error e -> Alcotest.fail ("crlf load: " ^ Cairos_io.err_to_string e)
      | Ok crlf ->
          Alcotest.(check int)
            "length equality" (Cairos.Series.length lf)
            (Cairos.Series.length crlf);
          Alcotest.(check (array (float 0.0)))
            "values equality" (values_to_array lf) (values_to_array crlf);
          Alcotest.(check (array ptime_testable))
            "timestamps equality"
            (Cairos.Index.timestamps (Cairos.Series.index lf))
            (Cairos.Index.timestamps (Cairos.Series.index crlf)))

let of_csv_daily_bom_prefix_matches_lf () =
  let lf_path = fixture "single_daily_standard.csv" in
  let bom_path = fixture "single_daily_bom.csv" in
  match Cairos_io.of_csv ~freq:Cairos.Freq.Day lf_path with
  | Error e -> Alcotest.fail ("lf load: " ^ Cairos_io.err_to_string e)
  | Ok lf -> (
      match Cairos_io.of_csv ~freq:Cairos.Freq.Day bom_path with
      | Error e -> Alcotest.fail ("bom load: " ^ Cairos_io.err_to_string e)
      | Ok bom ->
          Alcotest.(check int)
            "length equality" (Cairos.Series.length lf)
            (Cairos.Series.length bom);
          Alcotest.(check (array (float 0.0)))
            "values equality" (values_to_array lf) (values_to_array bom);
          Alcotest.(check (array ptime_testable))
            "timestamps equality"
            (Cairos.Index.timestamps (Cairos.Series.index lf))
            (Cairos.Index.timestamps (Cairos.Series.index bom)))

let of_csv_whitespace_only_file_returns_error () =
  Cairos_io.of_csv ~freq:Cairos.Freq.Day (fixture "empty_whitespace.csv")
  |> check_error ~label:"whitespace-only file" ~needles:[ "empty file" ]

let () =
  Alcotest.run "cairos_io"
    [
      ( "single-series happy paths",
        [
          Alcotest.test_case "of_csv standard daily shape" `Quick
            of_csv_standard_shape_daily_loads_series;
          Alcotest.test_case "of_csv_with no header swapped cols" `Quick
            of_csv_with_no_header_explicit_cols;
          Alcotest.test_case "of_csv minute shape" `Quick
            of_csv_minute_loads_series;
          Alcotest.test_case "of_csv hourly shape" `Quick
            of_csv_hourly_loads_series;
          Alcotest.test_case "of_csv weekly shape" `Quick
            of_csv_weekly_loads_series;
          Alcotest.test_case "of_csv CRLF line endings" `Quick
            of_csv_daily_crlf_line_endings_match_lf;
          Alcotest.test_case "of_csv UTF-8 BOM prefix" `Quick
            of_csv_daily_bom_prefix_matches_lf;
        ] );
      ( "single-series errors",
        [
          Alcotest.test_case "missing file reports path" `Quick
            of_csv_missing_file_returns_error_with_path;
          Alcotest.test_case "empty file" `Quick of_csv_empty_file_returns_error;
          Alcotest.test_case "whitespace-only file" `Quick
            of_csv_whitespace_only_file_returns_error;
          Alcotest.test_case "header-only file" `Quick
            of_csv_header_only_file_returns_error;
          Alcotest.test_case "row shorter than needed reports line" `Quick
            of_csv_row_shorter_than_needed_reports_line;
          Alcotest.test_case "nan price rejected" `Quick
            of_csv_nan_price_returns_error;
          Alcotest.test_case "inf price rejected" `Quick
            of_csv_inf_price_returns_error;
          Alcotest.test_case "unparseable price rejected" `Quick
            of_csv_unparseable_price_returns_error;
          Alcotest.test_case "narrow first row reports line" `Quick
            of_csv_with_narrow_first_row_reports_line;
        ] );
      ( "frame happy paths",
        [
          Alcotest.test_case "frame_of_csv columns by header" `Quick
            frame_of_csv_standard_shape_loads_columns_by_header;
          Alcotest.test_case "frame_of_csv_with no-header positional names"
            `Quick frame_of_csv_with_no_header_uses_positional_names;
          Alcotest.test_case "frame_of_csv NaN-fills offset listings" `Quick
            frame_of_csv_nan_fills_offset_listings;
          Alcotest.test_case "frame_of_csv accepts inf in cell" `Quick
            frame_of_csv_infinity_in_cell_is_accepted;
        ] );
      ( "frame errors",
        [
          Alcotest.test_case "duplicate header rejected" `Quick
            frame_of_csv_duplicate_header_returns_error;
          Alcotest.test_case "unparseable present cell reports line and col"
            `Quick frame_of_csv_unparseable_present_cell_reports_line_and_col;
          Alcotest.test_case "frame empty file rejected" `Quick
            frame_of_csv_empty_file_returns_error;
          Alcotest.test_case "frame header-only rejected" `Quick
            frame_of_csv_header_only_returns_error;
          Alcotest.test_case "frame short row reports line" `Quick
            frame_of_csv_with_short_row_reports_line;
          Alcotest.test_case "frame narrow first row rejected" `Quick
            frame_of_csv_with_narrow_first_row_returns_error;
          Alcotest.test_case "frame with only a timestamp column" `Quick
            frame_of_csv_with_timestamp_only_file_returns_empty_frame_columns;
        ] );
      ( "frame format quirks",
        [
          Alcotest.test_case
            "frame_of_csv_with no-header wider subsequent rows truncate" `Quick
            frame_of_csv_with_no_header_wider_subsequent_rows_truncates_silently;
        ] );
      ( "format boundary",
        [
          Alcotest.test_case "quoted fields are not supported" `Quick
            of_csv_quoted_field_is_unsupported;
        ] );
      ( "column-argument validation",
        [
          Alcotest.test_case
            "negative and duplicate column args yield distinct variants" `Quick
            cairos_io_invalid_column_arg_variants;
        ] );
      ( "structured errors",
        [
          Alcotest.test_case "translated line numbers are 1-indexed CSV lines"
            `Quick cairos_io_line_numbers_preserved;
          Alcotest.test_case "wrapping variants render both layers" `Quick
            cairos_io_series_error_wraps;
          Alcotest.test_case "every constructor renders" `Quick
            cairos_io_err_to_string_nonempty;
        ] );
    ]
