let dates_3 = [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]

(* --- Construction (happy path) --- *)

let of_series_single_column () =
  let s = Test_helpers.make_daily_series dates_3 [| 100.0; 200.0; 300.0 |] in
  match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("price", s)) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame -> (
      Alcotest.(check (list string))
        "columns" [ "price" ]
        (Cairos.Nonempty.to_list (Cairos.Frame.columns frame));
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
      (Cairos.Nonempty.make ("price", price)
         [ ("volume", volume); ("sma", sma) ])
  with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame -> (
      Alcotest.(check (list string))
        "columns in order"
        [ "price"; "volume"; "sma" ]
        (Cairos.Nonempty.to_list (Cairos.Frame.columns frame));
      match Cairos.Frame.get "volume" frame with
      | None -> Alcotest.fail "expected Some for 'volume'"
      | Some retrieved ->
          let vs = Nx.to_array (Cairos.Series.values retrieved) in
          Alcotest.(check (float 0.001)) "vol 0" 1000.0 vs.(0);
          Alcotest.(check (float 0.001)) "vol 1" 2000.0 vs.(1);
          Alcotest.(check (float 0.001)) "vol 2" 3000.0 vs.(2))

(* --- Construction (error cases) --- *)

(* Three columns, with the offending one third and last: "a" is the reference,
   "b" matches it, and only "c" is short. A two-column fixture would pass
   against an implementation that names the first column it validates rather
   than the one that failed, since there the two coincide. The lengths are
   asymmetric (3 against 2) so that a swapped pair of payload fields is
   likewise visible. *)
let frame_index_mismatch_variant () =
  let s3 = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let s3' = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  let s2 =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 10.0; 20.0 |]
  in
  match
    Cairos.Frame.of_series
      (Cairos.Nonempty.make ("a", s3) [ ("b", s3'); ("c", s2) ])
  with
  | Ok _ -> Alcotest.fail "expected Error for length mismatch"
  | Error (Cairos.Frame.Duplicate_column _) ->
      Alcotest.fail "expected Index_mismatch, got Duplicate_column"
  | Error
      (Cairos.Frame.Index_mismatch { column; expected_length; found_length }) ->
      Alcotest.(check string) "column" "c" column;
      Alcotest.(check int) "expected_length" 3 expected_length;
      Alcotest.(check int) "found_length" 2 found_length

(* The same variant reached by the other route: equal lengths, different
   timestamps. The payload lengths agree here, which is what distinguishes
   this input class from [frame_index_mismatch_variant] — the rejection turns
   on [Ptime] equality, not on a length comparison. *)
let of_series_timestamp_mismatch () =
  let s1 = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let s2 =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02"; "2024-01-04" |]
      [| 10.0; 20.0; 30.0 |]
  in
  match
    Cairos.Frame.of_series (Cairos.Nonempty.make ("a", s1) [ ("b", s2) ])
  with
  | Ok _ -> Alcotest.fail "expected Error for timestamp mismatch"
  | Error (Cairos.Frame.Duplicate_column _) ->
      Alcotest.fail "expected Index_mismatch, got Duplicate_column"
  | Error
      (Cairos.Frame.Index_mismatch { column; expected_length; found_length }) ->
      Alcotest.(check string) "column" "b" column;
      Alcotest.(check int) "expected_length" 3 expected_length;
      Alcotest.(check int) "found_length" 3 found_length

(* Message prose is not contractual, so this asserts only that every
   constructor renders something a caller can put in a log line — never that
   the message contains particular words. Both [Index_mismatch] shapes are
   rendered: the renderer distinguishes unequal lengths from equal ones, and
   the equal-length arm is reachable only through the timestamp route. *)
let frame_err_to_string_nonempty () =
  let renders label err =
    let msg = Cairos.Frame.err_to_string err in
    Alcotest.(check bool) (label ^ " is non-empty") true (String.length msg > 0);
    Alcotest.(check bool)
      (label ^ " is single-line")
      true
      (not (String.contains msg '\n'))
  in
  renders "Duplicate_column" (Cairos.Frame.Duplicate_column { name = "price" });
  renders "Index_mismatch (unequal lengths)"
    (Cairos.Frame.Index_mismatch
       { column = "c"; expected_length = 3; found_length = 2 });
  renders "Index_mismatch (equal lengths)"
    (Cairos.Frame.Index_mismatch
       { column = "b"; expected_length = 3; found_length = 3 })

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
      (Cairos.Nonempty.make ("price", price)
         [ ("volume", volume); ("sma", sma) ])
  with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
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
  match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("price", s)) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame ->
      Alcotest.(check bool)
        "nonexistent returns None" true
        (Option.is_none (Cairos.Frame.get "nonexistent" frame))

(* --- Column listing --- *)

let columns_preserves_insertion_order () =
  let s1 = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let s2 = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  let s3 = Test_helpers.make_daily_series dates_3 [| 7.0; 8.0; 9.0 |] in
  match
    Cairos.Frame.of_series
      (Cairos.Nonempty.make ("c", s1) [ ("a", s2); ("b", s3) ])
  with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame ->
      Alcotest.(check (list string))
        "insertion order" [ "c"; "a"; "b" ]
        (Cairos.Nonempty.to_list (Cairos.Frame.columns frame))

let frame_columns_nonempty_roundtrip () =
  let s1 = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let s2 = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  let s3 = Test_helpers.make_daily_series dates_3 [| 7.0; 8.0; 9.0 |] in
  match
    Cairos.Frame.of_series
      (Cairos.Nonempty.make ("z", s1) [ ("a", s2); ("m", s3) ])
  with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame ->
      let cols = Cairos.Frame.columns frame in
      Alcotest.(check string)
        "head is the first inserted column" "z" (Cairos.Nonempty.hd cols);
      Alcotest.(check (list string))
        "insertion order" [ "z"; "a"; "m" ]
        (Cairos.Nonempty.to_list cols);
      Alcotest.(check (list (list (float 0.001))))
        "every listed name retrieves its constructing values"
        [ [ 1.0; 2.0; 3.0 ]; [ 4.0; 5.0; 6.0 ]; [ 7.0; 8.0; 9.0 ] ]
        (List.map
           (fun name ->
             Array.to_list
               (Nx.to_array
                  (Cairos.Series.values (Test_helpers.frame_get_exn name frame))))
           (Cairos.Nonempty.to_list cols))

(* --- Shared index and total decomposition --- *)

let frame_index_matches_member_series () =
  let price =
    Test_helpers.make_daily_series dates_3 [| 100.0; 200.0; 300.0 |]
  in
  let volume =
    Test_helpers.make_daily_series dates_3 [| 1000.0; 2000.0; 3000.0 |]
  in
  match
    Cairos.Frame.of_series
      (Cairos.Nonempty.make ("price", price) [ ("volume", volume) ])
  with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame ->
      let member = Test_helpers.frame_get_exn "volume" frame in
      Alcotest.(check (array Test_helpers.ptime_testable))
        "timestamps"
        (Cairos.Index.timestamps (Cairos.Series.index member))
        (Cairos.Index.timestamps (Cairos.Frame.index frame))

let frame_to_series_inverts_of_series () =
  let a = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let b = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  let c = Test_helpers.make_daily_series dates_3 [| 7.0; 8.0; 9.0 |] in
  match
    Cairos.Frame.of_series
      (Cairos.Nonempty.make ("c", c) [ ("a", a); ("b", b) ])
  with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame ->
      let pairs = Cairos.Nonempty.to_list (Cairos.Frame.to_series frame) in
      Alcotest.(check (list string))
        "names in insertion order" [ "c"; "a"; "b" ] (List.map fst pairs);
      Alcotest.(check (list (list (float 0.001))))
        "values per column"
        [ [ 7.0; 8.0; 9.0 ]; [ 1.0; 2.0; 3.0 ]; [ 4.0; 5.0; 6.0 ] ]
        (List.map
           (fun (_, s) -> Array.to_list (Nx.to_array (Cairos.Series.values s)))
           pairs);
      List.iter
        (fun (name, s) ->
          Alcotest.(check (array Test_helpers.ptime_testable))
            ("shared index for " ^ name)
            (Cairos.Index.timestamps (Cairos.Frame.index frame))
            (Cairos.Index.timestamps (Cairos.Series.index s)))
        pairs

(* Encodes all four arguments [mapi_cells] hands to [f] into one float, so a
   swapped col/row, a name paired with the wrong column's data, or a dropped
   cell value each produce a different expected digit position. *)
let mapi_cells_probe ~col ~name ~row v =
  let name_digit =
    match name with
    | "z" -> 1.0
    | "a" -> 2.0
    | "m" -> 3.0
    | _ -> 9.0
  in
  (v *. 1000.0)
  +. (Float.of_int col *. 100.0)
  +. (Float.of_int row *. 10.0)
  +. name_digit

let frame_mapi_cells_preserves_shape_and_index () =
  let s1 = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let s2 = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  let s3 = Test_helpers.make_daily_series dates_3 [| 7.0; 8.0; 9.0 |] in
  match
    Cairos.Frame.of_series
      (Cairos.Nonempty.make ("z", s1) [ ("a", s2); ("m", s3) ])
  with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame ->
      let out = Cairos.Frame.mapi_cells ~f:mapi_cells_probe frame in
      Alcotest.(check (list string))
        "column names in input order" [ "z"; "a"; "m" ]
        (Cairos.Nonempty.to_list (Cairos.Frame.columns out));
      Alcotest.(check (array Test_helpers.ptime_testable))
        "shared index"
        (Cairos.Index.timestamps (Cairos.Frame.index frame))
        (Cairos.Index.timestamps (Cairos.Frame.index out));
      Alcotest.(check (list (list (float 0.001))))
        "cells carry col, name, row, and the input value"
        [
          [ 1001.0; 2011.0; 3021.0 ];
          [ 4102.0; 5112.0; 6122.0 ];
          [ 7203.0; 8213.0; 9223.0 ];
        ]
        (List.map
           (fun name ->
             Array.to_list
               (Nx.to_array
                  (Cairos.Series.values (Test_helpers.frame_get_exn name out))))
           (Cairos.Nonempty.to_list (Cairos.Frame.columns out)));
      Alcotest.(check (list (list (float 0.001))))
        "input frame is unchanged"
        [ [ 1.0; 2.0; 3.0 ]; [ 4.0; 5.0; 6.0 ]; [ 7.0; 8.0; 9.0 ] ]
        (List.map
           (fun name ->
             Array.to_list
               (Nx.to_array
                  (Cairos.Series.values (Test_helpers.frame_get_exn name frame))))
           (Cairos.Nonempty.to_list (Cairos.Frame.columns frame)))

(* Physical equality, not structural. [frame.mli] states that [index] returns
   "the index shared by every column" and that [mapi_cells] returns "the same
   [Index.t]"; the engine relies on that sharing to avoid re-deriving
   timestamps for [equity_curve], [returns] and the [weights] frame. A future
   implementation that rebuilt an equal index — a slice, a round-trip through
   [of_unix_floats] — would keep every structural check in this file green
   while falsifying the docstring. [==] is the only assertion that can see the
   difference. *)
let frame_index_is_physically_shared () =
  let a = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let b = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  match Cairos.Frame.of_series (Cairos.Nonempty.make ("a", a) [ ("b", b) ]) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame ->
      let idx = Cairos.Frame.index frame in
      Cairos.Nonempty.to_list (Cairos.Frame.to_series frame)
      |> List.iter (fun (name, s) ->
          Alcotest.(check bool)
            ("to_series shares the frame index for " ^ name)
            true
            (Cairos.Series.index s == idx));
      let out =
        Cairos.Frame.mapi_cells ~f:(fun ~col:_ ~name:_ ~row:_ v -> v) frame
      in
      Alcotest.(check bool)
        "mapi_cells carries the input index through" true
        (Cairos.Frame.index out == idx)

(* [mapi_cells] at the shapes the dense 3x3 case above cannot reach: a
   single-column frame exercises only the [Nonempty] head branch, an empty
   frame exercises the zero-length [Nx.create], and a sliced frame hands the
   implementation non-contiguous [Nx] views to materialise. *)
let frame_mapi_cells_edge_shapes () =
  let a = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let b = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  let cells frame =
    List.map
      (fun name ->
        Array.to_list
          (Nx.to_array
             (Cairos.Series.values (Test_helpers.frame_get_exn name frame))))
      (Cairos.Nonempty.to_list (Cairos.Frame.columns frame))
  in
  let double ~col:_ ~name:_ ~row:_ v = v *. 2.0 in
  (match Cairos.Frame.of_series (Cairos.Nonempty.make ("a", a) []) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok one ->
      let out = Cairos.Frame.mapi_cells ~f:double one in
      Alcotest.(check (list string))
        "single column preserved" [ "a" ]
        (Cairos.Nonempty.to_list (Cairos.Frame.columns out));
      Alcotest.(check (list (list (float 0.001))))
        "single-column cells"
        [ [ 2.0; 4.0; 6.0 ] ]
        (cells out));
  match Cairos.Frame.of_series (Cairos.Nonempty.make ("a", a) [ ("b", b) ]) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame ->
      let empty = Cairos.Frame.head 0 frame in
      let out_empty = Cairos.Frame.mapi_cells ~f:double empty in
      Alcotest.(check int)
        "zero rows survive" 0
        (Cairos.Index.length (Cairos.Frame.index out_empty));
      Alcotest.(check (list (list (float 0.001))))
        "zero-row cells" [ []; [] ] (cells out_empty);
      let sliced = Cairos.Frame.tail 2 frame in
      let out_sliced = Cairos.Frame.mapi_cells ~f:double sliced in
      Alcotest.(check (list (list (float 0.001))))
        "sliced views materialise correctly"
        [ [ 4.0; 6.0 ]; [ 10.0; 12.0 ] ]
        (cells out_sliced)

(* The repeated name is "price" and it is deliberately not the head: a fixture
   whose first column is the duplicated one passes against an implementation
   that reports the head name regardless of which name repeated. "volume"
   separates the two occurrences so they are not adjacent either. The name is
   the whole payload — the scan reports the first repeat it reaches and holds
   no column position, so there is nothing else to assert. *)
let frame_duplicate_column_variant () =
  let s1 = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let s2 = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  let s3 = Test_helpers.make_daily_series dates_3 [| 7.0; 8.0; 9.0 |] in
  let s4 = Test_helpers.make_daily_series dates_3 [| 10.0; 11.0; 12.0 |] in
  match
    Cairos.Frame.of_series
      (Cairos.Nonempty.make ("sma", s1)
         [ ("price", s2); ("volume", s3); ("price", s4) ])
  with
  | Ok _ -> Alcotest.fail "expected Error for duplicate column name"
  | Error (Cairos.Frame.Index_mismatch _) ->
      Alcotest.fail "expected Duplicate_column, got Index_mismatch"
  | Error (Cairos.Frame.Duplicate_column { name }) ->
      Alcotest.(check string) "name" "price" name

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
  match
    Cairos.Frame.of_series
      (Cairos.Nonempty.make ("price", price) [ ("volume", volume) ])
  with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
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
  match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("a", s)) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
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
  match
    Cairos.Frame.of_series
      (Cairos.Nonempty.make ("z", s1) [ ("a", s2); ("m", s3) ])
  with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame ->
      let h = Cairos.Frame.head 2 frame in
      Alcotest.(check (list string))
        "column order preserved" [ "z"; "a"; "m" ]
        (Cairos.Nonempty.to_list (Cairos.Frame.columns h))

let frame_tail_returns_last_n_rows () =
  let price =
    Test_helpers.make_daily_series dates_5 [| 10.0; 20.0; 30.0; 40.0; 50.0 |]
  in
  let volume =
    Test_helpers.make_daily_series dates_5
      [| 100.0; 200.0; 300.0; 400.0; 500.0 |]
  in
  match
    Cairos.Frame.of_series
      (Cairos.Nonempty.make ("price", price) [ ("volume", volume) ])
  with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
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
  match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("a", s)) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame ->
      let tl = Cairos.Frame.tail 10 frame in
      Alcotest.(check int)
        "length" 3
        (Cairos.Index.length
           (Cairos.Series.index (Test_helpers.frame_get_exn "a" tl)))

let frame_head_zero_returns_empty () =
  let s = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("a", s)) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
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
  match Cairos.Frame.of_series (Cairos.Nonempty.make ("a", a) [ ("b", b) ]) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
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
  match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("x", s)) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
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
  match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("x", s)) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
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
  match
    Cairos.Frame.of_series
      (Cairos.Nonempty.make ("z", s1) [ ("a", s2); ("m", s3) ])
  with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame ->
      let stats = Cairos.Frame.describe frame in
      let names = List.map fst stats in
      Alcotest.(check (list string)) "column order" [ "z"; "a"; "m" ] names

let describe_single_value_column () =
  let dates_1 = [| "2024-01-01" |] in
  let s = Test_helpers.make_daily_series dates_1 [| 42.0 |] in
  match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("x", s)) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
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
  match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("a", s)) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame ->
      let tl = Cairos.Frame.tail 0 frame in
      Alcotest.(check int)
        "length" 0
        (Cairos.Index.length
           (Cairos.Series.index (Test_helpers.frame_get_exn "a" tl)))

let frame_head_negative_returns_empty () =
  let s = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("a", s)) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame ->
      let h = Cairos.Frame.head (-1) frame in
      Alcotest.(check int)
        "length" 0
        (Cairos.Index.length
           (Cairos.Series.index (Test_helpers.frame_get_exn "a" h)))

let frame_tail_negative_returns_empty () =
  let s = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("a", s)) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame ->
      let tl = Cairos.Frame.tail (-1) frame in
      Alcotest.(check int)
        "length" 0
        (Cairos.Index.length
           (Cairos.Series.index (Test_helpers.frame_get_exn "a" tl)))

let frame_head_empty_frame () =
  let s = Test_helpers.make_daily_series [||] [||] in
  match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("a", s)) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame ->
      let h = Cairos.Frame.head 3 frame in
      Alcotest.(check int)
        "length" 0
        (Cairos.Index.length
           (Cairos.Series.index (Test_helpers.frame_get_exn "a" h)))

let frame_tail_empty_frame () =
  let s = Test_helpers.make_daily_series [||] [||] in
  match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("a", s)) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame ->
      let tl = Cairos.Frame.tail 3 frame in
      Alcotest.(check int)
        "length" 0
        (Cairos.Index.length
           (Cairos.Series.index (Test_helpers.frame_get_exn "a" tl)))

let describe_empty_frame () =
  let s = Test_helpers.make_daily_series [||] [||] in
  match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("x", s)) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
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
  match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("x", s)) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
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
    ("frame_index_mismatch_variant", `Quick, frame_index_mismatch_variant);
    ("of_series_timestamp_mismatch", `Quick, of_series_timestamp_mismatch);
    ("frame_err_to_string_nonempty", `Quick, frame_err_to_string_nonempty);
    ("frame_duplicate_column_variant", `Quick, frame_duplicate_column_variant);
    ("get_existing_column", `Quick, get_existing_column);
    ("get_missing_column", `Quick, get_missing_column);
    ( "columns_preserves_insertion_order",
      `Quick,
      columns_preserves_insertion_order );
    ( "frame_columns_nonempty_roundtrip",
      `Quick,
      frame_columns_nonempty_roundtrip );
    ( "frame_index_matches_member_series",
      `Quick,
      frame_index_matches_member_series );
    ( "frame_to_series_inverts_of_series",
      `Quick,
      frame_to_series_inverts_of_series );
    ( "frame_mapi_cells_preserves_shape_and_index",
      `Quick,
      frame_mapi_cells_preserves_shape_and_index );
    ( "frame_index_is_physically_shared",
      `Quick,
      frame_index_is_physically_shared );
    ("frame_mapi_cells_edge_shapes", `Quick, frame_mapi_cells_edge_shapes);
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
