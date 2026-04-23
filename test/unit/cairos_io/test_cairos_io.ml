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

let check_error ~label ~needles result =
  match result with
  | Ok _ -> Alcotest.fail (label ^ ": expected Error, got Ok")
  | Error msg ->
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            (label ^ ": message contains " ^ needle ^ " — was: " ^ msg)
            true (contains msg needle))
        needles

let of_csv_standard_shape_daily_loads_series () =
  let path = fixture "single_daily_standard.csv" in
  match Cairos_io.of_csv ~freq:Cairos.Freq.Day path with
  | Error e -> Alcotest.fail e
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
  | Error e -> Alcotest.fail ("standard load: " ^ e)
  | Ok standard -> (
      match
        Cairos_io.of_csv_with ~freq:Cairos.Freq.Day ~header:false
          ~timestamp_col:1 ~price_col:0 swapped_path
      with
      | Error e -> Alcotest.fail ("swapped load: " ^ e)
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
  |> check_error ~label:"short row at line 4"
       ~needles:[ "line 4"; "expected at least 2" ]

let of_csv_unparseable_timestamp_reports_line () =
  Cairos_io.of_csv ~freq:Cairos.Freq.Day (fixture "single_bad_ts_at_3.csv")
  |> check_error ~label:"unparseable ts at line 3"
       ~needles:[ "line 3"; "not-a-date" ]

let of_csv_non_monotonic_timestamps_reports_line () =
  Cairos_io.of_csv ~freq:Cairos.Freq.Day (fixture "single_non_monotonic.csv")
  |> check_error ~label:"non-monotonic at line 4" ~needles:[ "line 4" ]

let of_csv_nan_price_returns_error () =
  Cairos_io.of_csv ~freq:Cairos.Freq.Day (fixture "single_nan_price_at_5.csv")
  |> check_error ~label:"nan price at line 5"
       ~needles:[ "line 5"; "non-finite" ]

let of_csv_inf_price_returns_error () =
  Cairos_io.of_csv ~freq:Cairos.Freq.Day (fixture "single_inf_price_at_6.csv")
  |> check_error ~label:"inf price at line 6"
       ~needles:[ "line 6"; "non-finite" ]

let of_csv_unparseable_price_returns_error () =
  Cairos_io.of_csv ~freq:Cairos.Freq.Day (fixture "single_bad_price_at_2.csv")
  |> check_error ~label:"unparseable price at line 2"
       ~needles:[ "line 2"; "non-finite" ]

let of_csv_with_negative_col_returns_error () =
  Cairos_io.of_csv_with ~freq:Cairos.Freq.Day ~header:true ~timestamp_col:(-1)
    ~price_col:1
    (fixture "single_daily_standard.csv")
  |> check_error ~label:"negative timestamp_col" ~needles:[ "invalid argument" ]

let of_csv_with_equal_cols_returns_error () =
  Cairos_io.of_csv_with ~freq:Cairos.Freq.Day ~header:true ~timestamp_col:0
    ~price_col:0
    (fixture "single_daily_standard.csv")
  |> check_error ~label:"equal cols" ~needles:[ "invalid argument" ]

let of_csv_with_narrow_first_row_reports_line () =
  (* First data row of [single_daily_standard.csv] has 2 columns; asking for
     [~price_col:2] needs 3. Mirrors the frame [narrow_first_row] test on the
     single-series path. *)
  Cairos_io.of_csv_with ~freq:Cairos.Freq.Day ~header:true ~timestamp_col:0
    ~price_col:2
    (fixture "single_daily_standard.csv")
  |> check_error ~label:"single-series narrow first row"
       ~needles:[ "line 2"; "expected at least 3"; "found 2" ]

let get_series name frame =
  match Cairos.Frame.get name frame with
  | Some s -> s
  | None -> Alcotest.fail (Printf.sprintf "column %S missing from frame" name)

let frame_of_csv_standard_shape_loads_columns_by_header () =
  match
    Cairos_io.frame_of_csv ~freq:Cairos.Freq.Day
      (fixture "frame_three_tickers.csv")
  with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      Alcotest.(check (list string))
        "columns in source-file order" [ "AAPL"; "MSFT"; "GOOG" ]
        (Cairos.Frame.columns frame);
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
  | Error e -> Alcotest.fail e
  | Ok frame ->
      Alcotest.(check (list string))
        "positional column names"
        [ "col_1"; "col_2"; "col_3" ]
        (Cairos.Frame.columns frame)

let frame_of_csv_nan_fills_offset_listings () =
  match
    Cairos_io.frame_of_csv ~freq:Cairos.Freq.Day
      (fixture "frame_offset_listings.csv")
  with
  | Error e -> Alcotest.fail e
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
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let y = get_series "Y" frame in
      let v = (values_to_array y).(1) in
      Alcotest.(check bool)
        "Y[1] is positive infinity (not finite, > 0)" true
        ((not (Float.is_finite v)) && v > 0.0)

let frame_of_csv_duplicate_header_returns_error () =
  Cairos_io.frame_of_csv ~freq:Cairos.Freq.Day
    (fixture "frame_duplicate_header.csv")
  |> check_error ~label:"duplicate header"
       ~needles:[ "line 1"; "duplicate header"; "AAPL" ]

let frame_of_csv_unparseable_present_cell_reports_line_and_col () =
  Cairos_io.frame_of_csv ~freq:Cairos.Freq.Day
    (fixture "frame_bad_cell_at_3_2.csv")
  |> check_error ~label:"unparseable cell at line 3 col 2"
       ~needles:[ "line 3"; "col 2" ]

let frame_of_csv_unparseable_timestamp_reports_line () =
  Cairos_io.frame_of_csv ~freq:Cairos.Freq.Day (fixture "frame_bad_ts_at_4.csv")
  |> check_error ~label:"frame bad timestamp at line 4"
       ~needles:[ "line 4"; "not-a-date" ]

let frame_of_csv_with_negative_timestamp_col_returns_error () =
  Cairos_io.frame_of_csv_with ~freq:Cairos.Freq.Day ~header:true
    ~timestamp_col:(-3)
    (fixture "frame_three_tickers.csv")
  |> check_error ~label:"frame negative timestamp_col"
       ~needles:[ "invalid argument" ]

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
  |> check_error ~label:"frame short row at line 4"
       ~needles:[ "line 4"; "expected at least 3"; "found 1" ]

let frame_of_csv_with_narrow_first_row_returns_error () =
  Cairos_io.frame_of_csv_with ~freq:Cairos.Freq.Day ~header:false
    ~timestamp_col:3
    (fixture "frame_narrow_first_row.csv")
  |> check_error ~label:"frame narrow first row"
       ~needles:[ "line 1"; "expected at least 4"; "found 2" ]

let frame_of_csv_non_monotonic_timestamps_reports_line () =
  (* Mirror of [of_csv_non_monotonic_timestamps_reports_line] on the frame
     path: pins that the index-error → CSV-line translation fires identically
     for [frame_of_csv]. Line 4 = data row with the back-step timestamp. *)
  Cairos_io.frame_of_csv ~freq:Cairos.Freq.Day
    (fixture "frame_non_monotonic.csv")
  |> check_error ~label:"frame non-monotonic at line 4" ~needles:[ "line 4" ]

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
  | Error e -> Alcotest.fail e
  | Ok frame ->
      Alcotest.(check (list string))
        "columns derived from first row width" [ "col_1"; "col_2" ]
        (Cairos.Frame.columns frame);
      let c1 = get_series "col_1" frame in
      let c2 = get_series "col_2" frame in
      Alcotest.(check (array (float 0.0)))
        "col_1 values: extras in row 2 ignored" [| 100.0; 101.0; 102.0 |]
        (values_to_array c1);
      Alcotest.(check (array (float 0.0)))
        "col_2 values: extras in row 2 ignored" [| 200.0; 201.0; 202.0 |]
        (values_to_array c2)

let of_csv_quoted_field_is_unsupported () =
  (* Pins FR-9's "Quoting: none" boundary documented in cairos_io.mli. A
     timestamp wrapped in literal double quotes is not stripped — it flows
     to Index.daily verbatim and surfaces as Unparseable_timestamp at line 2.
     A future PRD that adopts ocaml-csv would need to delete or re-purpose
     this test. *)
  Cairos_io.of_csv ~freq:Cairos.Freq.Day (fixture "single_quoted_timestamp.csv")
  |> check_error ~label:"quoted timestamp not supported" ~needles:[ "line 2" ]

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
  | Error e -> Alcotest.fail e
  | Ok series ->
      Alcotest.(check int)
        "minute series length" 3
        (Cairos.Series.length series);
      Alcotest.(check (array (float 0.0)))
        "minute price values" [| 100.0; 101.0; 102.0 |] (values_to_array series)

let of_csv_hourly_loads_series () =
  let path = fixture "single_hourly_standard.csv" in
  match Cairos_io.of_csv ~freq:Cairos.Freq.Hour path with
  | Error e -> Alcotest.fail e
  | Ok series ->
      Alcotest.(check int)
        "hourly series length" 3
        (Cairos.Series.length series);
      Alcotest.(check (array (float 0.0)))
        "hourly price values" [| 100.0; 101.0; 102.0 |] (values_to_array series)

let of_csv_weekly_loads_series () =
  let path = fixture "single_weekly_standard.csv" in
  match Cairos_io.of_csv ~freq:Cairos.Freq.Week path with
  | Error e -> Alcotest.fail e
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
  | Error e -> Alcotest.fail ("lf load: " ^ e)
  | Ok lf -> (
      match Cairos_io.of_csv ~freq:Cairos.Freq.Day crlf_path with
      | Error e -> Alcotest.fail ("crlf load: " ^ e)
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
  | Error e -> Alcotest.fail ("lf load: " ^ e)
  | Ok lf -> (
      match Cairos_io.of_csv ~freq:Cairos.Freq.Day bom_path with
      | Error e -> Alcotest.fail ("bom load: " ^ e)
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
          Alcotest.test_case "unparseable timestamp reports line" `Quick
            of_csv_unparseable_timestamp_reports_line;
          Alcotest.test_case "non-monotonic timestamps reports line" `Quick
            of_csv_non_monotonic_timestamps_reports_line;
          Alcotest.test_case "nan price rejected" `Quick
            of_csv_nan_price_returns_error;
          Alcotest.test_case "inf price rejected" `Quick
            of_csv_inf_price_returns_error;
          Alcotest.test_case "unparseable price rejected" `Quick
            of_csv_unparseable_price_returns_error;
          Alcotest.test_case "negative col rejected" `Quick
            of_csv_with_negative_col_returns_error;
          Alcotest.test_case "equal cols rejected" `Quick
            of_csv_with_equal_cols_returns_error;
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
          Alcotest.test_case "frame unparseable timestamp reports line" `Quick
            frame_of_csv_unparseable_timestamp_reports_line;
          Alcotest.test_case "frame negative timestamp_col rejected" `Quick
            frame_of_csv_with_negative_timestamp_col_returns_error;
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
          Alcotest.test_case "frame non-monotonic timestamps reports line"
            `Quick frame_of_csv_non_monotonic_timestamps_reports_line;
        ] );
      ( "frame format quirks",
        [
          Alcotest.test_case
            "frame_of_csv_with no-header wider subsequent rows truncate" `Quick
            frame_of_csv_with_no_header_wider_subsequent_rows_truncates_silently;
        ] );
      ( "format boundary",
        [
          Alcotest.test_case "quoted fields are not supported (FR-9)" `Quick
            of_csv_quoted_field_is_unsupported;
        ] );
    ]
