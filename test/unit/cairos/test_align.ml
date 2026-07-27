(* --- Inner strategy --- *)

let inner_overlapping_series () =
  (* Left: Jan 1-5, Right: Jan 3-7 -> overlap on Jan 3,4,5 *)
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-03"; "2024-01-04"; "2024-01-05"; "2024-01-06"; "2024-01-07" |]
      [| 30.0; 40.0; 50.0; 60.0; 70.0 |]
  in
  match Cairos.Align.align ~strategy:`Inner left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
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

(* Deliberately asymmetric input lengths: 3 on the left, 2 on the right. A
   symmetric pair would pass just as well against an implementation that
   filled the two payload fields the wrong way round. *)
let align_empty_index_variant () =
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-05"; "2024-01-06" |]
      [| 5.0; 6.0 |]
  in
  match Cairos.Align.align ~strategy:`Inner left right with
  | Ok _ -> Alcotest.fail "expected Error for disjoint series"
  | Error (Cairos.Align.Empty_index { left_length; right_length }) ->
      Alcotest.(check int) "left_length" 3 left_length;
      Alcotest.(check int) "right_length" 2 right_length

(* Message prose is not contractual, so this asserts only that every
   constructor renders something a caller can put in a log line — never that
   the message contains particular words. *)
let align_err_to_string_nonempty () =
  let msg =
    Cairos.Align.err_to_string
      (Cairos.Align.Empty_index { left_length = 3; right_length = 2 })
  in
  Alcotest.(check bool) "Empty_index is non-empty" true (String.length msg > 0);
  Alcotest.(check bool)
    "Empty_index is single-line" true
    (not (String.contains msg '\n'))

let inner_subset_series () =
  (* Left is a subset of right *)
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-02"; "2024-01-03"; "2024-01-04" |]
      [| 20.0; 30.0; 40.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
      [| 100.0; 200.0; 300.0; 400.0; 500.0 |]
  in
  match Cairos.Align.align ~strategy:`Inner left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
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
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-03" |]
      [| 1.0; 3.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-02"; "2024-01-03" |]
      [| 20.0; 30.0 |]
  in
  match Cairos.Align.align ~strategy:`Inner left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
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
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |]
      [| 1.0; 2.0; 3.0; 4.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-03" |]
      [| 100.0; 300.0 |]
  in
  match Cairos.Align.align ~strategy:`Left left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
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
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  match Cairos.Align.align ~strategy:`Left left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check (float 0.001)) "right 0" 10.0 rv.(0);
      Alcotest.(check (float 0.001)) "right 1" 20.0 rv.(1);
      Alcotest.(check (float 0.001)) "right 2" 30.0 rv.(2)

let left_no_overlap () =
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 1.0; 2.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-03"; "2024-01-04" |]
      [| 30.0; 40.0 |]
  in
  match Cairos.Align.align ~strategy:`Left left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
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
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  match Cairos.Align.align ~strategy:(`Asof `Backward) left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check (float 0.001)) "right 0" 10.0 rv.(0);
      Alcotest.(check (float 0.001)) "right 1" 20.0 rv.(1);
      Alcotest.(check (float 0.001)) "right 2" 30.0 rv.(2)

let asof_backward_uses_previous () =
  (* Left: Jan 1,3,5. Right: Jan 2,4. Backward: Jan 1->nan, Jan 3->Jan 2, Jan 5->Jan 4 *)
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-03"; "2024-01-05" |]
      [| 1.0; 3.0; 5.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-02"; "2024-01-04" |]
      [| 20.0; 40.0 |]
  in
  match Cairos.Align.align ~strategy:(`Asof `Backward) left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check bool) "right 0 is nan" true (Float.is_nan rv.(0));
      Alcotest.(check (float 0.001)) "right 1" 20.0 rv.(1);
      Alcotest.(check (float 0.001)) "right 2" 40.0 rv.(2)

let asof_backward_no_prior () =
  (* Left: Jan 1. Right: Jan 2,3. Backward: Jan 1 has no right <= it -> nan *)
  let left = Test_helpers.make_daily_series [| "2024-01-01" |] [| 1.0 |] in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-02"; "2024-01-03" |]
      [| 20.0; 30.0 |]
  in
  match Cairos.Align.align ~strategy:(`Asof `Backward) left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check bool) "right 0 is nan" true (Float.is_nan rv.(0))

(* --- Asof Forward strategy --- *)

let asof_forward_exact_match () =
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  match Cairos.Align.align ~strategy:(`Asof `Forward) left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check (float 0.001)) "right 0" 10.0 rv.(0);
      Alcotest.(check (float 0.001)) "right 1" 20.0 rv.(1);
      Alcotest.(check (float 0.001)) "right 2" 30.0 rv.(2)

let asof_forward_uses_next () =
  (* Left: Jan 1,3,5. Right: Jan 2,4. Forward: Jan 1->Jan 2, Jan 3->Jan 4, Jan 5->nan *)
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-03"; "2024-01-05" |]
      [| 1.0; 3.0; 5.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-02"; "2024-01-04" |]
      [| 20.0; 40.0 |]
  in
  match Cairos.Align.align ~strategy:(`Asof `Forward) left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check (float 0.001)) "right 0" 20.0 rv.(0);
      Alcotest.(check (float 0.001)) "right 1" 40.0 rv.(1);
      Alcotest.(check bool) "right 2 is nan" true (Float.is_nan rv.(2))

let asof_forward_no_subsequent () =
  (* Left: Jan 3. Right: Jan 1,2. Forward: Jan 3 has no right >= it -> nan *)
  let left = Test_helpers.make_daily_series [| "2024-01-03" |] [| 3.0 |] in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 10.0; 20.0 |]
  in
  match Cairos.Align.align ~strategy:(`Asof `Forward) left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check bool) "right 0 is nan" true (Float.is_nan rv.(0))

(* --- map2 --- *)

let map2_adds_aligned_values () =
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  match Cairos.Align.align ~strategy:`Inner left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
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
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-03" |]
      [| 10.0; 30.0 |]
  in
  match Cairos.Align.align ~strategy:`Left left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
  | Ok aligned ->
      let result = Cairos.Align.map2 ( +. ) aligned in
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check (float 0.001)) "sum 0" 11.0 vs.(0);
      Alcotest.(check bool) "sum 1 is nan" true (Float.is_nan vs.(1));
      Alcotest.(check (float 0.001)) "sum 2" 33.0 vs.(2)

(* --- map2_nan --- *)

let gt_indicator a b = if a > b then 1.0 else 0.0

let aligned_inner_exn left right =
  match Cairos.Align.align ~strategy:`Inner left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
  | Ok aligned -> aligned

let map2_nan_propagates_left_nan () =
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; Float.nan; 300.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  let aligned = aligned_inner_exn left right in
  let vs =
    Nx.to_array
      (Cairos.Series.values (Cairos.Align.map2_nan aligned ~f:gt_indicator))
  in
  Alcotest.(check (float 0.001)) "clean 0" 0.0 vs.(0);
  Alcotest.(check bool) "left nan propagates" true (Float.is_nan vs.(1));
  Alcotest.(check (float 0.001)) "clean 2" 1.0 vs.(2)

let map2_nan_propagates_right_nan () =
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 300.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; Float.nan; 30.0 |]
  in
  let aligned = aligned_inner_exn left right in
  let vs =
    Nx.to_array
      (Cairos.Series.values (Cairos.Align.map2_nan aligned ~f:gt_indicator))
  in
  Alcotest.(check (float 0.001)) "clean 0" 0.0 vs.(0);
  Alcotest.(check bool) "right nan propagates" true (Float.is_nan vs.(1));
  Alcotest.(check (float 0.001)) "clean 2" 1.0 vs.(2)

let map2_nan_propagates_both_nan () =
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| Float.nan; 2.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| Float.nan; 20.0 |]
  in
  let aligned = aligned_inner_exn left right in
  let vs =
    Nx.to_array
      (Cairos.Series.values (Cairos.Align.map2_nan aligned ~f:gt_indicator))
  in
  Alcotest.(check bool) "both nan propagates" true (Float.is_nan vs.(0));
  Alcotest.(check (float 0.001)) "clean 1" 0.0 vs.(1)

let map2_nan_matches_map2_on_clean_input () =
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  let aligned = aligned_inner_exn left right in
  let plain =
    Nx.to_array (Cairos.Series.values (Cairos.Align.map2 ( +. ) aligned))
  in
  let nan_aware =
    Nx.to_array (Cairos.Series.values (Cairos.Align.map2_nan aligned ~f:( +. )))
  in
  Alcotest.(check (array (float 0.001)))
    "identical on nan-free input" plain nan_aware

let map2_nan_comparison_yields_nan_not_zero () =
  (* The logged defect: a fill-produced NaN must read as undefined, not flat. *)
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-03" |]
      [| 10.0; 30.0 |]
  in
  match Cairos.Align.align ~strategy:`Left left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
  | Ok aligned ->
      let vs =
        Nx.to_array
          (Cairos.Series.values (Cairos.Align.map2_nan aligned ~f:gt_indicator))
      in
      Alcotest.(check (float 0.001)) "matched 0" 0.0 vs.(0);
      Alcotest.(check bool) "warmup reads undefined" true (Float.is_nan vs.(1));
      Alcotest.(check (float 0.001)) "matched 2" 0.0 vs.(2);
      (* The claim [map2_nan] exists to answer, on the same fixture: under
         [map2] the fill NaN takes the [else] branch and reads as a confident
         "flat" rather than as undefined. If this ever stops holding, the
         justification for [map2_nan] in align.mli has gone stale. *)
      let under_map2 =
        Nx.to_array
          (Cairos.Series.values (Cairos.Align.map2 gt_indicator aligned))
      in
      Alcotest.(check (float 0.001))
        "map2 emits a confident flat at the same warmup position" 0.0
        under_map2.(1);
      Alcotest.(check bool)
        "map2 does not propagate the fill nan" false
        (Float.is_nan under_map2.(1))

let map2_nan_passes_through_f_produced_nan () =
  (* Gating is on inputs only: f's own NaN on a clean pair is not intercepted. *)
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 1.0; 200.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 10.0; 20.0 |]
  in
  let aligned = aligned_inner_exn left right in
  let vs =
    Nx.to_array
      (Cairos.Series.values
         (Cairos.Align.map2_nan aligned ~f:(fun a b ->
              if a < b then Float.nan else 0.0)))
  in
  Alcotest.(check bool)
    "f-produced nan passes through" true
    (Float.is_nan vs.(0));
  Alcotest.(check (float 0.001)) "f's non-nan branch still applies" 0.0 vs.(1)

(* --- Edge cases --- *)

let inner_empty_left_returns_error () =
  let left = Test_helpers.make_daily_series [||] [||] in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 10.0; 20.0 |]
  in
  match Cairos.Align.align ~strategy:`Inner left right with
  | Ok _ -> Alcotest.fail "expected Error for empty left with Inner"
  | Error (Cairos.Align.Empty_index { left_length; right_length }) ->
      Alcotest.(check int) "left_length" 0 left_length;
      Alcotest.(check int) "right_length" 2 right_length

let left_empty_left_returns_ok () =
  let left = Test_helpers.make_daily_series [||] [||] in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 10.0; 20.0 |]
  in
  match Cairos.Align.align ~strategy:`Left left right with
  | Error e ->
      Alcotest.fail
        ("expected Ok for empty left with Left, got: "
        ^ Cairos.Align.err_to_string e)
  | Ok aligned ->
      Alcotest.(check int)
        "aligned length" 0
        (Cairos.Index.length (Cairos.Align.index aligned))

let asof_empty_left_returns_ok () =
  let left = Test_helpers.make_daily_series [||] [||] in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 10.0; 20.0 |]
  in
  match Cairos.Align.align ~strategy:(`Asof `Backward) left right with
  | Error e ->
      Alcotest.fail
        ("expected Ok for empty left with Asof, got: "
        ^ Cairos.Align.err_to_string e)
  | Ok aligned ->
      Alcotest.(check int)
        "aligned length" 0
        (Cairos.Index.length (Cairos.Align.index aligned))

let left_empty_right_all_nan () =
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 1.0; 2.0 |]
  in
  let right = Test_helpers.make_daily_series [||] [||] in
  match Cairos.Align.align ~strategy:`Left left right with
  | Error e ->
      Alcotest.fail
        ("expected Ok for empty right with Left, got: "
        ^ Cairos.Align.err_to_string e)
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check bool) "right 0 is nan" true (Float.is_nan rv.(0));
      Alcotest.(check bool) "right 1 is nan" true (Float.is_nan rv.(1))

let asof_forward_empty_right_all_nan () =
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 1.0; 2.0 |]
  in
  let right = Test_helpers.make_daily_series [||] [||] in
  match Cairos.Align.align ~strategy:(`Asof `Forward) left right with
  | Error e ->
      Alcotest.fail
        ("expected Ok for empty right with Asof, got: "
        ^ Cairos.Align.err_to_string e)
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check bool) "right 0 is nan" true (Float.is_nan rv.(0));
      Alcotest.(check bool) "right 1 is nan" true (Float.is_nan rv.(1))

let inner_empty_right_returns_error () =
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 1.0; 2.0 |]
  in
  let right = Test_helpers.make_daily_series [||] [||] in
  match Cairos.Align.align ~strategy:`Inner left right with
  | Ok _ -> Alcotest.fail "expected Error for empty right with Inner"
  | Error (Cairos.Align.Empty_index { left_length; right_length }) ->
      Alcotest.(check int) "left_length" 2 left_length;
      Alcotest.(check int) "right_length" 0 right_length

let asof_backward_empty_right_all_nan () =
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 1.0; 2.0 |]
  in
  let right = Test_helpers.make_daily_series [||] [||] in
  match Cairos.Align.align ~strategy:(`Asof `Backward) left right with
  | Error e ->
      Alcotest.fail
        ("expected Ok for empty right with Asof, got: "
        ^ Cairos.Align.err_to_string e)
  | Ok aligned ->
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check bool) "right 0 is nan" true (Float.is_nan rv.(0));
      Alcotest.(check bool) "right 1 is nan" true (Float.is_nan rv.(1))

let map2_with_asof_backward () =
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-03"; "2024-01-05" |]
      [| 1.0; 3.0; 5.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-02"; "2024-01-04" |]
      [| 20.0; 40.0 |]
  in
  match Cairos.Align.align ~strategy:(`Asof `Backward) left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
  | Ok aligned ->
      let result = Cairos.Align.map2 ( +. ) aligned in
      let vs = Nx.to_array (Cairos.Series.values result) in
      Alcotest.(check bool) "sum 0 is nan" true (Float.is_nan vs.(0));
      Alcotest.(check (float 0.001)) "sum 1" 23.0 vs.(1);
      Alcotest.(check (float 0.001)) "sum 2" 45.0 vs.(2)

let inner_identical_timestamps () =
  let left =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 1.0; 2.0; 3.0 |]
  in
  let right =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]
      [| 10.0; 20.0; 30.0 |]
  in
  match Cairos.Align.align ~strategy:`Inner left right with
  | Error e -> Alcotest.fail (Cairos.Align.err_to_string e)
  | Ok aligned ->
      let idx = Cairos.Align.index aligned in
      Alcotest.(check int) "aligned length" 3 (Cairos.Index.length idx);
      let lv = Nx.to_array (Cairos.Align.left aligned) in
      Alcotest.(check (float 0.001)) "left 0" 1.0 lv.(0);
      Alcotest.(check (float 0.001)) "left 1" 2.0 lv.(1);
      Alcotest.(check (float 0.001)) "left 2" 3.0 lv.(2);
      let rv = Nx.to_array (Cairos.Align.right aligned) in
      Alcotest.(check (float 0.001)) "right 0" 10.0 rv.(0);
      Alcotest.(check (float 0.001)) "right 1" 20.0 rv.(1);
      Alcotest.(check (float 0.001)) "right 2" 30.0 rv.(2)

let tests =
  [
    ("inner_overlapping_series", `Quick, inner_overlapping_series);
    ("align_empty_index_variant", `Quick, align_empty_index_variant);
    ("align_err_to_string_nonempty", `Quick, align_err_to_string_nonempty);
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
    ("map2_nan_propagates_left_nan", `Quick, map2_nan_propagates_left_nan);
    ("map2_nan_propagates_right_nan", `Quick, map2_nan_propagates_right_nan);
    ("map2_nan_propagates_both_nan", `Quick, map2_nan_propagates_both_nan);
    ( "map2_nan_matches_map2_on_clean_input",
      `Quick,
      map2_nan_matches_map2_on_clean_input );
    ( "map2_nan_comparison_yields_nan_not_zero",
      `Quick,
      map2_nan_comparison_yields_nan_not_zero );
    ( "map2_nan_passes_through_f_produced_nan",
      `Quick,
      map2_nan_passes_through_f_produced_nan );
    ("map2_with_asof_backward", `Quick, map2_with_asof_backward);
    ("inner_identical_timestamps", `Quick, inner_identical_timestamps);
  ]

let () = Alcotest.run "Align" [ ("Align", tests) ]
