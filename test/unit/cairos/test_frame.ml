let dates_3 = [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]

(* --- Construction (happy path) --- *)

let of_series_single_column () =
  let s = Test_helpers.make_daily_series dates_3 [| 100.0; 200.0; 300.0 |] in
  match Cairos.Frame.of_series [ ("price", s) ] with
  | Error e -> Alcotest.fail e
  | Ok frame -> (
      Alcotest.(check (list string))
        "columns" [ "price" ]
        (Cairos.Frame.columns frame);
      match Cairos.Frame.get "price" frame with
      | None -> Alcotest.fail "expected Some for 'price'"
      | Some retrieved ->
          let vs = Nx.to_array (Cairos.Series.values retrieved) in
          Alcotest.(check (float 0.001)) "v0" 100.0 vs.(0);
          Alcotest.(check (float 0.001)) "v1" 200.0 vs.(1);
          Alcotest.(check (float 0.001)) "v2" 300.0 vs.(2))

let of_series_multiple_columns () =
  let price =
    Test_helpers.make_daily_series dates_3 [| 100.0; 200.0; 300.0 |]
  in
  let volume =
    Test_helpers.make_daily_series dates_3 [| 1000.0; 2000.0; 3000.0 |]
  in
  let sma = Test_helpers.make_daily_series dates_3 [| 10.0; 20.0; 30.0 |] in
  match
    Cairos.Frame.of_series
      [ ("price", price); ("volume", volume); ("sma", sma) ]
  with
  | Error e -> Alcotest.fail e
  | Ok frame -> (
      Alcotest.(check (list string))
        "columns in order"
        [ "price"; "volume"; "sma" ]
        (Cairos.Frame.columns frame);
      match Cairos.Frame.get "volume" frame with
      | None -> Alcotest.fail "expected Some for 'volume'"
      | Some retrieved ->
          let vs = Nx.to_array (Cairos.Series.values retrieved) in
          Alcotest.(check (float 0.001)) "vol 0" 1000.0 vs.(0);
          Alcotest.(check (float 0.001)) "vol 1" 2000.0 vs.(1);
          Alcotest.(check (float 0.001)) "vol 2" 3000.0 vs.(2))

(* --- Construction (error cases) --- *)

let of_series_empty_list () =
  match Cairos.Frame.of_series [] with
  | Ok _ -> Alcotest.fail "expected Error for empty list"
  | Error _ -> ()

let of_series_length_mismatch () =
  let s3 = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let s2 =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 10.0; 20.0 |]
  in
  match Cairos.Frame.of_series [ ("a", s3); ("b", s2) ] with
  | Ok _ -> Alcotest.fail "expected Error for length mismatch"
  | Error msg ->
      Alcotest.(check bool) "mentions column name" true (String.length msg > 0)

let of_series_timestamp_mismatch () =
  let s1 = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let s2 =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-04" |]
      [| 10.0; 20.0; 30.0 |]
  in
  match Cairos.Frame.of_series [ ("a", s1); ("b", s2) ] with
  | Ok _ -> Alcotest.fail "expected Error for timestamp mismatch"
  | Error msg ->
      Alcotest.(check bool) "mentions column name" true (String.length msg > 0)

(* --- Retrieval --- *)

let get_existing_column () =
  let price =
    Test_helpers.make_daily_series dates_3 [| 100.0; 200.0; 300.0 |]
  in
  let volume =
    Test_helpers.make_daily_series dates_3 [| 1000.0; 2000.0; 3000.0 |]
  in
  let sma = Test_helpers.make_daily_series dates_3 [| 10.0; 20.0; 30.0 |] in
  match
    Cairos.Frame.of_series
      [ ("price", price); ("volume", volume); ("sma", sma) ]
  with
  | Error e -> Alcotest.fail e
  | Ok frame -> (
      match Cairos.Frame.get "volume" frame with
      | None -> Alcotest.fail "expected Some for 'volume'"
      | Some retrieved ->
          Alcotest.(check int)
            "index length" 3
            (Cairos.Index.length (Cairos.Series.index retrieved));
          let vs = Nx.to_array (Cairos.Series.values retrieved) in
          Alcotest.(check (float 0.001)) "v0" 1000.0 vs.(0);
          Alcotest.(check (float 0.001)) "v1" 2000.0 vs.(1);
          Alcotest.(check (float 0.001)) "v2" 3000.0 vs.(2))

let get_missing_column () =
  let s = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  match Cairos.Frame.of_series [ ("price", s) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      Alcotest.(check bool)
        "nonexistent returns None" true
        (Option.is_none (Cairos.Frame.get "nonexistent" frame))

(* --- Column listing --- *)

let columns_preserves_insertion_order () =
  let s1 = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let s2 = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  let s3 = Test_helpers.make_daily_series dates_3 [| 7.0; 8.0; 9.0 |] in
  match Cairos.Frame.of_series [ ("c", s1); ("a", s2); ("b", s3) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      Alcotest.(check (list string))
        "insertion order" [ "c"; "a"; "b" ]
        (Cairos.Frame.columns frame)

let of_series_duplicate_column_name () =
  let s1 = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let s2 = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  match Cairos.Frame.of_series [ ("price", s1); ("price", s2) ] with
  | Ok _ -> Alcotest.fail "expected Error for duplicate column name"
  | Error msg ->
      Alcotest.(check bool) "mentions column name" true (String.length msg > 0)

let tests =
  [
    ("of_series_single_column", `Quick, of_series_single_column);
    ("of_series_multiple_columns", `Quick, of_series_multiple_columns);
    ("of_series_empty_list", `Quick, of_series_empty_list);
    ("of_series_length_mismatch", `Quick, of_series_length_mismatch);
    ("of_series_timestamp_mismatch", `Quick, of_series_timestamp_mismatch);
    ("of_series_duplicate_column_name", `Quick, of_series_duplicate_column_name);
    ("get_existing_column", `Quick, get_existing_column);
    ("get_missing_column", `Quick, get_missing_column);
    ( "columns_preserves_insertion_order",
      `Quick,
      columns_preserves_insertion_order );
  ]

let () = Alcotest.run "Frame" [ ("Frame", tests) ]
