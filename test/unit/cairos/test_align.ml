let make_daily_series dates values =
  match Cairos.Index.daily dates with
  | Error e -> Alcotest.fail e
  | Ok idx -> (
      let vals = Nx.create Nx.float64 [| Array.length values |] values in
      match Cairos.Series.make idx vals with
      | Error e -> Alcotest.fail e
      | Ok s -> s)

(* --- Inner strategy --- *)

let inner_overlapping_series () =
  (* Left: Jan 1-5, Right: Jan 3-7 -> overlap on Jan 3,4,5 *)
  let left =
    make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
  in
  let right =
    make_daily_series
      [| "2024-01-03"; "2024-01-04"; "2024-01-05"; "2024-01-06"; "2024-01-07" |]
      [| 30.0; 40.0; 50.0; 60.0; 70.0 |]
  in
  match Cairos.Align.align ~strategy:`Inner left right with
  | Error e -> Alcotest.fail e
  | Ok aligned ->
      let idx = Cairos.Align.index aligned in
      Alcotest.(check int) "aligned length" 3 (Cairos.Index.length idx);
      let lv = Nx.to_array (Cairos.Align.left aligned) in
      Alcotest.(check (float 0.001)) "left 0" 3.0 lv.(0);
      Alcotest.(check (float 0.001)) "left 1" 4.0 lv.(1);
      Alcotest.(check (float 0.001)) "left 2" 5.0 lv.(2);
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check (float 0.001)) "right 0" 30.0 rv.(0);
      Alcotest.(check (float 0.001)) "right 1" 40.0 rv.(1);
      Alcotest.(check (float 0.001)) "right 2" 50.0 rv.(2)

let inner_disjoint_series () =
  let left =
    make_daily_series [| "2024-01-01"; "2024-01-02" |] [| 1.0; 2.0 |]
  in
  let right =
    make_daily_series [| "2024-01-03"; "2024-01-04" |] [| 3.0; 4.0 |]
  in
  match Cairos.Align.align ~strategy:`Inner left right with
  | Ok _ -> Alcotest.fail "expected Error for disjoint series"
  | Error _ -> ()

let inner_subset_series () =
  (* Left is a subset of right *)
  let left =
    make_daily_series
      [| "2024-01-02"; "2024-01-03"; "2024-01-04" |]
      [| 20.0; 30.0; 40.0 |]
  in
  let right =
    make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 100.0; 200.0; 300.0; 400.0; 500.0 |]
  in
  match Cairos.Align.align ~strategy:`Inner left right with
  | Error e -> Alcotest.fail e
  | Ok aligned ->
      let idx = Cairos.Align.index aligned in
      Alcotest.(check int) "aligned length" 3 (Cairos.Index.length idx);
      let lv = Nx.to_array (Cairos.Align.left aligned) in
      Alcotest.(check (float 0.001)) "left 0" 20.0 lv.(0);
      Alcotest.(check (float 0.001)) "left 1" 30.0 lv.(1);
      Alcotest.(check (float 0.001)) "left 2" 40.0 lv.(2);
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check (float 0.001)) "right 0" 200.0 rv.(0);
      Alcotest.(check (float 0.001)) "right 1" 300.0 rv.(1);
      Alcotest.(check (float 0.001)) "right 2" 400.0 rv.(2)

let inner_single_common_timestamp () =
  let left =
    make_daily_series [| "2024-01-01"; "2024-01-03" |] [| 1.0; 3.0 |]
  in
  let right =
    make_daily_series [| "2024-01-02"; "2024-01-03" |] [| 20.0; 30.0 |]
  in
  match Cairos.Align.align ~strategy:`Inner left right with
  | Error e -> Alcotest.fail e
  | Ok aligned ->
      let idx = Cairos.Align.index aligned in
      Alcotest.(check int) "aligned length" 1 (Cairos.Index.length idx);
      let lv = Nx.to_array (Cairos.Align.left aligned) in
      Alcotest.(check (float 0.001)) "left 0" 3.0 lv.(0);
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check (float 0.001)) "right 0" 30.0 rv.(0)

(* --- Left strategy --- *)

let left_fills_missing_with_nan () =
  (* Left: Jan 1-4, Right: Jan 1,3 -> positions 1,3 missing in right *)
  let left =
    make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |]
      [| 1.0; 2.0; 3.0; 4.0 |]
  in
  let right =
    make_daily_series [| "2024-01-01"; "2024-01-03" |] [| 100.0; 300.0 |]
  in
  match Cairos.Align.align ~strategy:`Left left right with
  | Error e -> Alcotest.fail e
  | Ok aligned ->
      let idx = Cairos.Align.index aligned in
      Alcotest.(check int) "aligned length" 4 (Cairos.Index.length idx);
      let lv = Nx.to_array (Cairos.Align.left aligned) in
      Alcotest.(check (float 0.001)) "left preserved 0" 1.0 lv.(0);
      Alcotest.(check (float 0.001)) "left preserved 1" 2.0 lv.(1);
      Alcotest.(check (float 0.001)) "left preserved 2" 3.0 lv.(2);
      Alcotest.(check (float 0.001)) "left preserved 3" 4.0 lv.(3);
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check (float 0.001)) "right 0 matched" 100.0 rv.(0);
      Alcotest.(check bool) "right 1 is nan" true (Float.is_nan rv.(1));
      Alcotest.(check (float 0.001)) "right 2 matched" 300.0 rv.(2);
      Alcotest.(check bool) "right 3 is nan" true (Float.is_nan rv.(3))

let left_full_overlap () =
  let left =
    make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let right =
    make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  match Cairos.Align.align ~strategy:`Left left right with
  | Error e -> Alcotest.fail e
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check (float 0.001)) "right 0" 10.0 rv.(0);
      Alcotest.(check (float 0.001)) "right 1" 20.0 rv.(1);
      Alcotest.(check (float 0.001)) "right 2" 30.0 rv.(2)

let left_no_overlap () =
  let left =
    make_daily_series [| "2024-01-01"; "2024-01-02" |] [| 1.0; 2.0 |]
  in
  let right =
    make_daily_series [| "2024-01-03"; "2024-01-04" |] [| 30.0; 40.0 |]
  in
  match Cairos.Align.align ~strategy:`Left left right with
  | Error e -> Alcotest.fail e
  | Ok aligned ->
      let lv = Nx.to_array (Cairos.Align.left aligned) in
      Alcotest.(check (float 0.001)) "left 0 preserved" 1.0 lv.(0);
      Alcotest.(check (float 0.001)) "left 1 preserved" 2.0 lv.(1);
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check bool) "right 0 is nan" true (Float.is_nan rv.(0));
      Alcotest.(check bool) "right 1 is nan" true (Float.is_nan rv.(1))

(* --- Asof Backward strategy --- *)

let asof_backward_exact_match () =
  let left =
    make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let right =
    make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  match Cairos.Align.align ~strategy:(`Asof `Backward) left right with
  | Error e -> Alcotest.fail e
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check (float 0.001)) "right 0" 10.0 rv.(0);
      Alcotest.(check (float 0.001)) "right 1" 20.0 rv.(1);
      Alcotest.(check (float 0.001)) "right 2" 30.0 rv.(2)

let asof_backward_uses_previous () =
  (* Left: Jan 1,3,5. Right: Jan 2,4. Backward: Jan 1->nan, Jan 3->Jan 2, Jan 5->Jan 4 *)
  let left =
    make_daily_series
      [| "2024-01-01"; "2024-01-03"; "2024-01-05" |]
      [| 1.0; 3.0; 5.0 |]
  in
  let right =
    make_daily_series [| "2024-01-02"; "2024-01-04" |] [| 20.0; 40.0 |]
  in
  match Cairos.Align.align ~strategy:(`Asof `Backward) left right with
  | Error e -> Alcotest.fail e
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check bool) "right 0 is nan" true (Float.is_nan rv.(0));
      Alcotest.(check (float 0.001)) "right 1" 20.0 rv.(1);
      Alcotest.(check (float 0.001)) "right 2" 40.0 rv.(2)

let asof_backward_no_prior () =
  (* Left: Jan 1. Right: Jan 2,3. Backward: Jan 1 has no right <= it -> nan *)
  let left = make_daily_series [| "2024-01-01" |] [| 1.0 |] in
  let right =
    make_daily_series [| "2024-01-02"; "2024-01-03" |] [| 20.0; 30.0 |]
  in
  match Cairos.Align.align ~strategy:(`Asof `Backward) left right with
  | Error e -> Alcotest.fail e
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check bool) "right 0 is nan" true (Float.is_nan rv.(0))

(* --- Asof Forward strategy --- *)

let asof_forward_exact_match () =
  let left =
    make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let right =
    make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  match Cairos.Align.align ~strategy:(`Asof `Forward) left right with
  | Error e -> Alcotest.fail e
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check (float 0.001)) "right 0" 10.0 rv.(0);
      Alcotest.(check (float 0.001)) "right 1" 20.0 rv.(1);
      Alcotest.(check (float 0.001)) "right 2" 30.0 rv.(2)

let asof_forward_uses_next () =
  (* Left: Jan 1,3,5. Right: Jan 2,4. Forward: Jan 1->Jan 2, Jan 3->Jan 4, Jan 5->nan *)
  let left =
    make_daily_series
      [| "2024-01-01"; "2024-01-03"; "2024-01-05" |]
      [| 1.0; 3.0; 5.0 |]
  in
  let right =
    make_daily_series [| "2024-01-02"; "2024-01-04" |] [| 20.0; 40.0 |]
  in
  match Cairos.Align.align ~strategy:(`Asof `Forward) left right with
  | Error e -> Alcotest.fail e
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check (float 0.001)) "right 0" 20.0 rv.(0);
      Alcotest.(check (float 0.001)) "right 1" 40.0 rv.(1);
      Alcotest.(check bool) "right 2 is nan" true (Float.is_nan rv.(2))

let asof_forward_no_subsequent () =
  (* Left: Jan 3. Right: Jan 1,2. Forward: Jan 3 has no right >= it -> nan *)
  let left = make_daily_series [| "2024-01-03" |] [| 3.0 |] in
  let right =
    make_daily_series [| "2024-01-01"; "2024-01-02" |] [| 10.0; 20.0 |]
  in
  match Cairos.Align.align ~strategy:(`Asof `Forward) left right with
  | Error e -> Alcotest.fail e
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check bool) "right 0 is nan" true (Float.is_nan rv.(0))

(* --- map2 --- *)

let map2_adds_aligned_values () =
  let left =
    make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let right =
    make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  match Cairos.Align.align ~strategy:`Inner left right with
  | Error e -> Alcotest.fail e
  | Ok aligned ->
      let result = Cairos.Align.map2 ( +. ) aligned in
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "sum 0" 11.0 vs.(0);
      Alcotest.(check (float 0.001)) "sum 1" 22.0 vs.(1);
      Alcotest.(check (float 0.001)) "sum 2" 33.0 vs.(2);
      Alcotest.(check int)
        "result index length" 3
        (Cairos.Index.length (Cairos.Series.index result))

let map2_propagates_nan () =
  (* Left join with missing right -> NaN positions propagate through map2 *)
  let left =
    make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let right =
    make_daily_series [| "2024-01-01"; "2024-01-03" |] [| 10.0; 30.0 |]
  in
  match Cairos.Align.align ~strategy:`Left left right with
  | Error e -> Alcotest.fail e
  | Ok aligned ->
      let result = Cairos.Align.map2 ( +. ) aligned in
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "sum 0" 11.0 vs.(0);
      Alcotest.(check bool) "sum 1 is nan" true (Float.is_nan vs.(1));
      Alcotest.(check (float 0.001)) "sum 2" 33.0 vs.(2)

(* --- Edge cases --- *)

let inner_empty_left_returns_error () =
  let left = make_daily_series [||] [||] in
  let right =
    make_daily_series [| "2024-01-01"; "2024-01-02" |] [| 10.0; 20.0 |]
  in
  match Cairos.Align.align ~strategy:`Inner left right with
  | Ok _ -> Alcotest.fail "expected Error for empty left with Inner"
  | Error _ -> ()

let left_empty_left_returns_ok () =
  let left = make_daily_series [||] [||] in
  let right =
    make_daily_series [| "2024-01-01"; "2024-01-02" |] [| 10.0; 20.0 |]
  in
  match Cairos.Align.align ~strategy:`Left left right with
  | Error e -> Alcotest.fail ("expected Ok for empty left with Left, got: " ^ e)
  | Ok aligned ->
      Alcotest.(check int)
        "aligned length" 0
        (Cairos.Index.length (Cairos.Align.index aligned))

let asof_empty_left_returns_ok () =
  let left = make_daily_series [||] [||] in
  let right =
    make_daily_series [| "2024-01-01"; "2024-01-02" |] [| 10.0; 20.0 |]
  in
  match Cairos.Align.align ~strategy:(`Asof `Backward) left right with
  | Error e -> Alcotest.fail ("expected Ok for empty left with Asof, got: " ^ e)
  | Ok aligned ->
      Alcotest.(check int)
        "aligned length" 0
        (Cairos.Index.length (Cairos.Align.index aligned))

let left_empty_right_all_nan () =
  let left =
    make_daily_series [| "2024-01-01"; "2024-01-02" |] [| 1.0; 2.0 |]
  in
  let right = make_daily_series [||] [||] in
  match Cairos.Align.align ~strategy:`Left left right with
  | Error e -> Alcotest.fail ("expected Ok for empty right with Left, got: " ^ e)
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check bool) "right 0 is nan" true (Float.is_nan rv.(0));
      Alcotest.(check bool) "right 1 is nan" true (Float.is_nan rv.(1))

let asof_forward_empty_right_all_nan () =
  let left =
    make_daily_series [| "2024-01-01"; "2024-01-02" |] [| 1.0; 2.0 |]
  in
  let right = make_daily_series [||] [||] in
  match Cairos.Align.align ~strategy:(`Asof `Forward) left right with
  | Error e -> Alcotest.fail ("expected Ok for empty right with Asof, got: " ^ e)
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check bool) "right 0 is nan" true (Float.is_nan rv.(0));
      Alcotest.(check bool) "right 1 is nan" true (Float.is_nan rv.(1))

let inner_empty_right_returns_error () =
  let left =
    make_daily_series [| "2024-01-01"; "2024-01-02" |] [| 1.0; 2.0 |]
  in
  let right = make_daily_series [||] [||] in
  match Cairos.Align.align ~strategy:`Inner left right with
  | Ok _ -> Alcotest.fail "expected Error for empty right with Inner"
  | Error _ -> ()

let asof_backward_empty_right_all_nan () =
  let left =
    make_daily_series [| "2024-01-01"; "2024-01-02" |] [| 1.0; 2.0 |]
  in
  let right = make_daily_series [||] [||] in
  match Cairos.Align.align ~strategy:(`Asof `Backward) left right with
  | Error e -> Alcotest.fail ("expected Ok for empty right with Asof, got: " ^ e)
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check bool) "right 0 is nan" true (Float.is_nan rv.(0));
      Alcotest.(check bool) "right 1 is nan" true (Float.is_nan rv.(1))

let tests =
  [
    ("inner_overlapping_series", `Quick, inner_overlapping_series);
    ("inner_disjoint_series", `Quick, inner_disjoint_series);
    ("inner_subset_series", `Quick, inner_subset_series);
    ("inner_single_common_timestamp", `Quick, inner_single_common_timestamp);
    ("left_fills_missing_with_nan", `Quick, left_fills_missing_with_nan);
    ("left_full_overlap", `Quick, left_full_overlap);
    ("left_no_overlap", `Quick, left_no_overlap);
    ("asof_backward_exact_match", `Quick, asof_backward_exact_match);
    ("asof_backward_uses_previous", `Quick, asof_backward_uses_previous);
    ("asof_backward_no_prior", `Quick, asof_backward_no_prior);
    ("asof_forward_exact_match", `Quick, asof_forward_exact_match);
    ("asof_forward_uses_next", `Quick, asof_forward_uses_next);
    ("asof_forward_no_subsequent", `Quick, asof_forward_no_subsequent);
    ("map2_adds_aligned_values", `Quick, map2_adds_aligned_values);
    ("map2_propagates_nan", `Quick, map2_propagates_nan);
    ("inner_empty_left_returns_error", `Quick, inner_empty_left_returns_error);
    ("left_empty_left_returns_ok", `Quick, left_empty_left_returns_ok);
    ("asof_empty_left_returns_ok", `Quick, asof_empty_left_returns_ok);
    ("left_empty_right_all_nan", `Quick, left_empty_right_all_nan);
    ( "asof_forward_empty_right_all_nan",
      `Quick,
      asof_forward_empty_right_all_nan );
    ("inner_empty_right_returns_error", `Quick, inner_empty_right_returns_error);
    ( "asof_backward_empty_right_all_nan",
      `Quick,
      asof_backward_empty_right_all_nan );
  ]
