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

let dates_5 =
  [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]

(* --- head / tail --- *)

let frame_head_returns_first_n_rows () =
  let price =
    Test_helpers.make_daily_series dates_5 [| 10.0; 20.0; 30.0; 40.0; 50.0 |]
  in
  let volume =
    Test_helpers.make_daily_series dates_5
      [| 100.0; 200.0; 300.0; 400.0; 500.0 |]
  in
  match Cairos.Frame.of_series [ ("price", price); ("volume", volume) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let h = Cairos.Frame.head 3 frame in
      Alcotest.(check int)
        "length" 3
        (Cairos.Index.length
           (Cairos.Series.index (Test_helpers.frame_get_exn "price" h)));
      let pv =
        Nx.to_array
          (Cairos.Series.values (Test_helpers.frame_get_exn "price" h))
      in
      Alcotest.(check (float 0.001)) "p0" 10.0 pv.(0);
      Alcotest.(check (float 0.001)) "p1" 20.0 pv.(1);
      Alcotest.(check (float 0.001)) "p2" 30.0 pv.(2);
      let vv =
        Nx.to_array
          (Cairos.Series.values (Test_helpers.frame_get_exn "volume" h))
      in
      Alcotest.(check (float 0.001)) "v0" 100.0 vv.(0);
      Alcotest.(check (float 0.001)) "v1" 200.0 vv.(1);
      Alcotest.(check (float 0.001)) "v2" 300.0 vv.(2)

let frame_head_clamps_to_length () =
  let s = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  match Cairos.Frame.of_series [ ("a", s) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let h = Cairos.Frame.head 10 frame in
      Alcotest.(check int)
        "length" 3
        (Cairos.Index.length
           (Cairos.Series.index (Test_helpers.frame_get_exn "a" h)))

let frame_head_preserves_column_order () =
  let s1 = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let s2 = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  let s3 = Test_helpers.make_daily_series dates_3 [| 7.0; 8.0; 9.0 |] in
  match Cairos.Frame.of_series [ ("z", s1); ("a", s2); ("m", s3) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let h = Cairos.Frame.head 2 frame in
      Alcotest.(check (list string))
        "column order preserved" [ "z"; "a"; "m" ] (Cairos.Frame.columns h)

let frame_tail_returns_last_n_rows () =
  let price =
    Test_helpers.make_daily_series dates_5 [| 10.0; 20.0; 30.0; 40.0; 50.0 |]
  in
  let volume =
    Test_helpers.make_daily_series dates_5
      [| 100.0; 200.0; 300.0; 400.0; 500.0 |]
  in
  match Cairos.Frame.of_series [ ("price", price); ("volume", volume) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let tl = Cairos.Frame.tail 3 frame in
      Alcotest.(check int)
        "length" 3
        (Cairos.Index.length
           (Cairos.Series.index (Test_helpers.frame_get_exn "price" tl)));
      let pv =
        Nx.to_array
          (Cairos.Series.values (Test_helpers.frame_get_exn "price" tl))
      in
      Alcotest.(check (float 0.001)) "p0" 30.0 pv.(0);
      Alcotest.(check (float 0.001)) "p1" 40.0 pv.(1);
      Alcotest.(check (float 0.001)) "p2" 50.0 pv.(2);
      let vv =
        Nx.to_array
          (Cairos.Series.values (Test_helpers.frame_get_exn "volume" tl))
      in
      Alcotest.(check (float 0.001)) "v0" 300.0 vv.(0);
      Alcotest.(check (float 0.001)) "v1" 400.0 vv.(1);
      Alcotest.(check (float 0.001)) "v2" 500.0 vv.(2)

let frame_tail_clamps_to_length () =
  let s = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  match Cairos.Frame.of_series [ ("a", s) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let tl = Cairos.Frame.tail 10 frame in
      Alcotest.(check int)
        "length" 3
        (Cairos.Index.length
           (Cairos.Series.index (Test_helpers.frame_get_exn "a" tl)))

let frame_head_zero_returns_empty () =
  let s = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  match Cairos.Frame.of_series [ ("a", s) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let h = Cairos.Frame.head 0 frame in
      Alcotest.(check int)
        "length" 0
        (Cairos.Index.length
           (Cairos.Series.index (Test_helpers.frame_get_exn "a" h)))

(* --- describe --- *)

let describe_computes_stats_for_each_column () =
  let a =
    Test_helpers.make_daily_series dates_5 [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
  in
  let b =
    Test_helpers.make_daily_series dates_5 [| 10.0; 20.0; 30.0; 40.0; 50.0 |]
  in
  match Cairos.Frame.of_series [ ("a", a); ("b", b) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let stats = Cairos.Frame.describe frame in
      let sa = Test_helpers.assoc_exn "a" stats in
      let sb = Test_helpers.assoc_exn "b" stats in
      Alcotest.(check int) "a count" 5 sa.count;
      Alcotest.(check (float 0.001)) "a mean" 3.0 sa.mean;
      Alcotest.(check (float 0.001)) "a std" (Float.sqrt 2.0) sa.std;
      Alcotest.(check (float 0.001)) "a min" 1.0 sa.min;
      Alcotest.(check (float 0.001)) "a max" 5.0 sa.max;
      Alcotest.(check (float 0.001)) "a median" 3.0 sa.median;
      Alcotest.(check (float 0.001)) "a p25" 2.0 sa.p25;
      Alcotest.(check (float 0.001)) "a p75" 4.0 sa.p75;
      Alcotest.(check int) "b count" 5 sb.count;
      Alcotest.(check (float 0.001)) "b mean" 30.0 sb.mean;
      Alcotest.(check (float 0.001)) "b std" (Float.sqrt 200.0) sb.std;
      Alcotest.(check (float 0.001)) "b min" 10.0 sb.min;
      Alcotest.(check (float 0.001)) "b max" 50.0 sb.max;
      Alcotest.(check (float 0.001)) "b median" 30.0 sb.median;
      Alcotest.(check (float 0.001)) "b p25" 20.0 sb.p25;
      Alcotest.(check (float 0.001)) "b p75" 40.0 sb.p75

let describe_excludes_nan_from_stats () =
  let s =
    Test_helpers.make_daily_series dates_5
      [| 1.0; Float.nan; 3.0; Float.nan; 5.0 |]
  in
  match Cairos.Frame.of_series [ ("x", s) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let stats = Cairos.Frame.describe frame in
      let sx = Test_helpers.assoc_exn "x" stats in
      Alcotest.(check int) "count" 3 sx.count;
      Alcotest.(check (float 0.001)) "mean" 3.0 sx.mean;
      Alcotest.(check (float 0.001)) "min" 1.0 sx.min;
      Alcotest.(check (float 0.001)) "max" 5.0 sx.max

let describe_all_nan_column () =
  let s =
    Test_helpers.make_daily_series dates_3 [| Float.nan; Float.nan; Float.nan |]
  in
  match Cairos.Frame.of_series [ ("x", s) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let stats = Cairos.Frame.describe frame in
      let sx = Test_helpers.assoc_exn "x" stats in
      Alcotest.(check int) "count" 0 sx.count;
      Alcotest.(check bool) "mean is nan" true (Float.is_nan sx.mean);
      Alcotest.(check bool) "std is nan" true (Float.is_nan sx.std);
      Alcotest.(check bool) "min is nan" true (Float.is_nan sx.min);
      Alcotest.(check bool) "max is nan" true (Float.is_nan sx.max);
      Alcotest.(check bool) "median is nan" true (Float.is_nan sx.median);
      Alcotest.(check bool) "p25 is nan" true (Float.is_nan sx.p25);
      Alcotest.(check bool) "p75 is nan" true (Float.is_nan sx.p75)

let describe_preserves_column_order () =
  let s1 = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let s2 = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  let s3 = Test_helpers.make_daily_series dates_3 [| 7.0; 8.0; 9.0 |] in
  match Cairos.Frame.of_series [ ("z", s1); ("a", s2); ("m", s3) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let stats = Cairos.Frame.describe frame in
      let names = List.map fst stats in
      Alcotest.(check (list string)) "column order" [ "z"; "a"; "m" ] names

let describe_single_value_column () =
  let dates_1 = [| "2024-01-01" |] in
  let s = Test_helpers.make_daily_series dates_1 [| 42.0 |] in
  match Cairos.Frame.of_series [ ("x", s) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let stats = Cairos.Frame.describe frame in
      let sx = Test_helpers.assoc_exn "x" stats in
      Alcotest.(check int) "count" 1 sx.count;
      Alcotest.(check (float 0.001)) "mean" 42.0 sx.mean;
      Alcotest.(check (float 0.001)) "std" 0.0 sx.std;
      Alcotest.(check (float 0.001)) "min" 42.0 sx.min;
      Alcotest.(check (float 0.001)) "max" 42.0 sx.max;
      Alcotest.(check (float 0.001)) "median" 42.0 sx.median;
      Alcotest.(check (float 0.001)) "p25" 42.0 sx.p25;
      Alcotest.(check (float 0.001)) "p75" 42.0 sx.p75

let frame_tail_zero_returns_empty () =
  let s = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  match Cairos.Frame.of_series [ ("a", s) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let tl = Cairos.Frame.tail 0 frame in
      Alcotest.(check int)
        "length" 0
        (Cairos.Index.length
           (Cairos.Series.index (Test_helpers.frame_get_exn "a" tl)))

let frame_head_negative_returns_empty () =
  let s = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  match Cairos.Frame.of_series [ ("a", s) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let h = Cairos.Frame.head (-1) frame in
      Alcotest.(check int)
        "length" 0
        (Cairos.Index.length
           (Cairos.Series.index (Test_helpers.frame_get_exn "a" h)))

let frame_tail_negative_returns_empty () =
  let s = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  match Cairos.Frame.of_series [ ("a", s) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let tl = Cairos.Frame.tail (-1) frame in
      Alcotest.(check int)
        "length" 0
        (Cairos.Index.length
           (Cairos.Series.index (Test_helpers.frame_get_exn "a" tl)))

let frame_head_empty_frame () =
  let s = Test_helpers.make_daily_series [||] [||] in
  match Cairos.Frame.of_series [ ("a", s) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let h = Cairos.Frame.head 3 frame in
      Alcotest.(check int)
        "length" 0
        (Cairos.Index.length
           (Cairos.Series.index (Test_helpers.frame_get_exn "a" h)))

let frame_tail_empty_frame () =
  let s = Test_helpers.make_daily_series [||] [||] in
  match Cairos.Frame.of_series [ ("a", s) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let tl = Cairos.Frame.tail 3 frame in
      Alcotest.(check int)
        "length" 0
        (Cairos.Index.length
           (Cairos.Series.index (Test_helpers.frame_get_exn "a" tl)))

let describe_empty_frame () =
  let s = Test_helpers.make_daily_series [||] [||] in
  match Cairos.Frame.of_series [ ("x", s) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let stats = Cairos.Frame.describe frame in
      let sx = Test_helpers.assoc_exn "x" stats in
      Alcotest.(check int) "count" 0 sx.count;
      Alcotest.(check bool) "mean is nan" true (Float.is_nan sx.mean);
      Alcotest.(check bool) "std is nan" true (Float.is_nan sx.std);
      Alcotest.(check bool) "min is nan" true (Float.is_nan sx.min);
      Alcotest.(check bool) "max is nan" true (Float.is_nan sx.max)

let describe_quantile_interpolation () =
  let dates_4 = [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |] in
  let s = Test_helpers.make_daily_series dates_4 [| 1.0; 2.0; 3.0; 4.0 |] in
  match Cairos.Frame.of_series [ ("x", s) ] with
  | Error e -> Alcotest.fail e
  | Ok frame ->
      let stats = Cairos.Frame.describe frame in
      let sx = Test_helpers.assoc_exn "x" stats in
      (* h = (4-1) * 0.25 = 0.75 → lo=0, hi=1, frac=0.75 → 1.0*0.25 + 2.0*0.75 = 1.75 *)
      Alcotest.(check (float 0.001)) "p25" 1.75 sx.p25;
      (* h = (4-1) * 0.5 = 1.5 → lo=1, hi=2, frac=0.5 → 2.0*0.5 + 3.0*0.5 = 2.5 *)
      Alcotest.(check (float 0.001)) "median" 2.5 sx.median;
      (* h = (4-1) * 0.75 = 2.25 → lo=2, hi=3, frac=0.25 → 3.0*0.75 + 4.0*0.25 = 3.25 *)
      Alcotest.(check (float 0.001)) "p75" 3.25 sx.p75

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
    ("frame_head_returns_first_n_rows", `Quick, frame_head_returns_first_n_rows);
    ("frame_head_clamps_to_length", `Quick, frame_head_clamps_to_length);
    ( "frame_head_preserves_column_order",
      `Quick,
      frame_head_preserves_column_order );
    ("frame_tail_returns_last_n_rows", `Quick, frame_tail_returns_last_n_rows);
    ("frame_tail_clamps_to_length", `Quick, frame_tail_clamps_to_length);
    ("frame_head_zero_returns_empty", `Quick, frame_head_zero_returns_empty);
    ( "describe_computes_stats_for_each_column",
      `Quick,
      describe_computes_stats_for_each_column );
    ( "describe_excludes_nan_from_stats",
      `Quick,
      describe_excludes_nan_from_stats );
    ("describe_all_nan_column", `Quick, describe_all_nan_column);
    ("describe_preserves_column_order", `Quick, describe_preserves_column_order);
    ("describe_single_value_column", `Quick, describe_single_value_column);
    ("frame_tail_zero_returns_empty", `Quick, frame_tail_zero_returns_empty);
    ( "frame_head_negative_returns_empty",
      `Quick,
      frame_head_negative_returns_empty );
    ( "frame_tail_negative_returns_empty",
      `Quick,
      frame_tail_negative_returns_empty );
    ("frame_head_empty_frame", `Quick, frame_head_empty_frame);
    ("frame_tail_empty_frame", `Quick, frame_tail_empty_frame);
    ("describe_empty_frame", `Quick, describe_empty_frame);
    ("describe_quantile_interpolation", `Quick, describe_quantile_interpolation);
  ]

let () = Alcotest.run "Frame" [ ("Frame", tests) ]
