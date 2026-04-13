let dates =
  [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]

let values = [| 100.0; 102.0; 101.0; 105.0; 103.0 |]
let make_series () = Test_helpers.make_daily_series dates values

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

let count_coordinate_pairs points_attr =
  String.split_on_char ' ' (String.trim points_attr)
  |> List.filter (fun s -> String.contains s ',')
  |> List.length

let extract_points_attr svg =
  let tag = "points=\"" in
  let tlen = String.length tag in
  let slen = String.length svg in
  let rec search i =
    if i > slen - tlen then None
    else if String.sub svg i tlen = tag then
      let start = i + tlen in
      let rec find_end j =
        if j >= slen then None
        else if svg.[j] = '"' then Some (String.sub svg start (j - start))
        else find_end (j + 1)
      in
      find_end start
    else search (i + 1)
  in
  search 0

let line_chart_polyline_matches_series_length () =
  let s = make_series () in
  let svg = Cairos_plot.line_chart s in
  Alcotest.(check bool)
    "SVG contains <polyline" true
    (string_contains svg "<polyline");
  match extract_points_attr svg with
  | None -> Alcotest.fail "no points attribute found in polyline"
  | Some pts ->
      let n = count_coordinate_pairs pts in
      Alcotest.(check int) "5 coordinate pairs" 5 n

let line_chart_renders_title () =
  let s = make_series () in
  let with_title = Cairos_plot.line_chart ~title:"Test Title" s in
  let without_title = Cairos_plot.line_chart s in
  Alcotest.(check bool)
    "title present" true
    (string_contains with_title ">Test Title<");
  Alcotest.(check bool)
    "title absent" false
    (string_contains without_title ">Test Title<")

let line_chart_renders_timestamp_labels () =
  let s = make_series () in
  let svg = Cairos_plot.line_chart s in
  let has_any = Array.exists (fun d -> string_contains svg d) dates in
  Alcotest.(check bool) "at least one date label" true has_any

let drawdown_chart_contains_filled_path () =
  let dd_values = [| 0.0; -0.05; -0.10; -0.03; -0.15 |] in
  let s = Test_helpers.make_daily_series dates dd_values in
  let svg = Cairos_plot.drawdown_chart s in
  Alcotest.(check bool) "SVG contains <path" true (string_contains svg "<path");
  let has_non_none_fill =
    string_contains svg "fill=\"" && not (string_contains svg "fill=\"none\"")
  in
  Alcotest.(check bool) "path has non-none fill" true has_non_none_fill

let drawdown_chart_y_axis_includes_zero () =
  let dd_values = [| 0.0; -0.05; -0.10; -0.03; -0.15 |] in
  let s = Test_helpers.make_daily_series dates dd_values in
  let svg = Cairos_plot.drawdown_chart s in
  Alcotest.(check bool)
    "zero baseline tick label" true
    (string_contains svg ">0.0%<")

let hourly_dates =
  [|
    "2024-01-01T10:00:00Z";
    "2024-01-01T11:00:00Z";
    "2024-01-01T12:00:00Z";
    "2024-01-01T13:00:00Z";
    "2024-01-01T14:00:00Z";
  |]

let hourly_expected_labels =
  [|
    "2024-01-01 10:00";
    "2024-01-01 11:00";
    "2024-01-01 12:00";
    "2024-01-01 13:00";
    "2024-01-01 14:00";
  |]

let line_chart_renders_hourly_timestamp_labels () =
  let s = Test_helpers.make_hourly_series hourly_dates values in
  let svg = Cairos_plot.line_chart s in
  let has_any =
    Array.exists (fun label -> string_contains svg label) hourly_expected_labels
  in
  Alcotest.(check bool) "at least one hourly label" true has_any

let () =
  Alcotest.run "cairos_plot"
    [
      ( "line_chart",
        [
          Alcotest.test_case "polyline matches series length" `Quick
            line_chart_polyline_matches_series_length;
          Alcotest.test_case "renders title" `Quick line_chart_renders_title;
          Alcotest.test_case "renders timestamp labels" `Quick
            line_chart_renders_timestamp_labels;
          Alcotest.test_case "renders hourly timestamp labels" `Quick
            line_chart_renders_hourly_timestamp_labels;
        ] );
      ( "drawdown_chart",
        [
          Alcotest.test_case "contains filled path" `Quick
            drawdown_chart_contains_filled_path;
          Alcotest.test_case "y axis includes zero" `Quick
            drawdown_chart_y_axis_includes_zero;
        ] );
    ]
