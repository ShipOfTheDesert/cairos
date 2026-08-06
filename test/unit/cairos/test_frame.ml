let dates_3 = [| "2024-01-01"; "2024-01-02"; "2024-01-03" |]

(* Reads every column of [frame] as a plain float list, in [columns] order.
   Cases that check names and values together go through this: a correct name
   list paired with values that moved is the failure a names-only assertion
   cannot see. Defined here rather than beside its first use so every case in
   the file can reach it. *)
let frame_cells frame =
  List.map
    (fun name ->
      Array.to_list
        (Nx.to_array
           (Cairos.Series.values (Test_helpers.frame_get_exn name frame))))
    (Cairos.Nonempty.to_list (Cairos.Frame.columns frame))

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
        (frame_cells frame)

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
        (frame_cells out);
      Alcotest.(check (list (list (float 0.001))))
        "input frame is unchanged"
        [ [ 1.0; 2.0; 3.0 ]; [ 4.0; 5.0; 6.0 ]; [ 7.0; 8.0; 9.0 ] ]
        (frame_cells frame)

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
  let cells = frame_cells in
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

(* --- add_column --- *)

(* Ordering is the whole claim, so the expected list is enumerated in full
   rather than checked for membership — a prepend, or a rebuild that reorders,
   is visible only against the whole list. The added name sorts first
   alphabetically while landing last positionally, so an implementation that
   sorted rather than appended would also be caught here. *)
let frame_add_column_appends_last () =
  let z = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let m = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  let a = Test_helpers.make_daily_series dates_3 [| 7.0; 8.0; 9.0 |] in
  match Cairos.Frame.of_series (Cairos.Nonempty.make ("z", z) [ ("m", m) ]) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame -> (
      match Cairos.Frame.add_column "a" a frame with
      | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
      | Ok out ->
          Alcotest.(check (list string))
            "appended in last position" [ "z"; "m"; "a" ]
            (Cairos.Nonempty.to_list (Cairos.Frame.columns out));
          Alcotest.(check (list (list (float 0.001))))
            "values follow their names"
            [ [ 1.0; 2.0; 3.0 ]; [ 4.0; 5.0; 6.0 ]; [ 7.0; 8.0; 9.0 ] ]
            (frame_cells out))

(* The duplicated name is deliberately not the frame's head: a fixture whose
   first column is the repeated one passes against an implementation that
   reports the head name regardless of which name repeated.

   The first arm adds a fresh name to the same frame and is the affirmative
   arm the rejection needs — without it an [add_column] returning
   [Duplicate_column] for every input would pass this case.

   The third arm pins the check order the signature states: a column that is
   both a repeat and index-incompatible reports the repeat. Nothing else in
   the suite can see that ordering, since every other input trips at most one
   of the two checks. *)
let frame_add_column_duplicate_name () =
  let z = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let m = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  let extra = Test_helpers.make_daily_series dates_3 [| 7.0; 8.0; 9.0 |] in
  let short =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 10.0; 20.0 |]
  in
  match Cairos.Frame.of_series (Cairos.Nonempty.make ("z", z) [ ("m", m) ]) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame -> (
      (match Cairos.Frame.add_column "fresh" extra frame with
      | Error e ->
          Alcotest.fail ("fresh name rejected: " ^ Cairos.Frame.err_to_string e)
      | Ok out ->
          Alcotest.(check (list string))
            "a fresh name on this frame is accepted" [ "z"; "m"; "fresh" ]
            (Cairos.Nonempty.to_list (Cairos.Frame.columns out)));
      (match Cairos.Frame.add_column "m" short frame with
      | Ok _ ->
          Alcotest.fail "expected Error for a repeated, index-incompatible name"
      | Error (Cairos.Frame.Index_mismatch _) ->
          Alcotest.fail
            "expected the duplicate name to be reported ahead of the index"
      | Error (Cairos.Frame.Duplicate_column { name }) ->
          Alcotest.(check string) "duplicate is checked first" "m" name);
      (match Cairos.Frame.add_column "m" extra frame with
      | Ok _ -> Alcotest.fail "expected Error for duplicate column name"
      | Error (Cairos.Frame.Index_mismatch _) ->
          Alcotest.fail "expected Duplicate_column, got Index_mismatch"
      | Error (Cairos.Frame.Duplicate_column { name }) ->
          Alcotest.(check string) "name" "m" name);
      (* The head name, which the arms above deliberately avoid. A membership
         test over the whole column list answers both the same way; one that
         reached only the tail would accept this and produce a frame carrying
         "z" twice, which no later operation could undo. *)
      match Cairos.Frame.add_column "z" extra frame with
      | Ok _ -> Alcotest.fail "expected Error for a repeated head column name"
      | Error (Cairos.Frame.Index_mismatch _) ->
          Alcotest.fail "expected Duplicate_column, got Index_mismatch"
      | Error (Cairos.Frame.Duplicate_column { name }) ->
          Alcotest.(check string) "head name" "z" name)

(* Lengths are asymmetric (3 against 2) so a swapped pair of payload fields is
   visible. The first arm adds a column whose index matches, which is the
   affirmative arm: it rules out an [add_column] that rejects every column on
   index grounds.

   The last arm is the equal-length route the signature documents — same length,
   different timestamps. It is the branch worth pinning: an [add_column] that
   compared lengths instead of timestamps passes every other arm here, and the
   payload it produces is the one [err_to_string] carries a comment about, with
   [expected_length] and [found_length] equal. *)
let frame_add_column_index_mismatch () =
  let z = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let m = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  let matching = Test_helpers.make_daily_series dates_3 [| 7.0; 8.0; 9.0 |] in
  let short =
    Test_helpers.make_daily_series
      [| "2024-01-01"; "2024-01-02" |]
      [| 10.0; 20.0 |]
  in
  let shifted =
    Test_helpers.make_daily_series
      [| "2024-01-02"; "2024-01-03"; "2024-01-04" |]
      [| 10.0; 20.0; 30.0 |]
  in
  match Cairos.Frame.of_series (Cairos.Nonempty.make ("z", z) [ ("m", m) ]) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame -> (
      (match Cairos.Frame.add_column "matching" matching frame with
      | Error e ->
          Alcotest.fail
            ("matching index rejected: " ^ Cairos.Frame.err_to_string e)
      | Ok out ->
          Alcotest.(check (list string))
            "a matching index on this frame is accepted"
            [ "z"; "m"; "matching" ]
            (Cairos.Nonempty.to_list (Cairos.Frame.columns out)));
      (match Cairos.Frame.add_column "short" short frame with
      | Ok _ -> Alcotest.fail "expected Error for index length mismatch"
      | Error (Cairos.Frame.Duplicate_column _) ->
          Alcotest.fail "expected Index_mismatch, got Duplicate_column"
      | Error
          (Cairos.Frame.Index_mismatch { column; expected_length; found_length })
        ->
          Alcotest.(check string) "column" "short" column;
          Alcotest.(check int) "expected_length" 3 expected_length;
          Alcotest.(check int) "found_length" 2 found_length);
      match Cairos.Frame.add_column "shifted" shifted frame with
      | Ok _ ->
          Alcotest.fail
            "expected Error for an equal-length index with different timestamps"
      | Error (Cairos.Frame.Duplicate_column _) ->
          Alcotest.fail "expected Index_mismatch, got Duplicate_column"
      | Error
          (Cairos.Frame.Index_mismatch { column; expected_length; found_length })
        ->
          Alcotest.(check string) "column" "shifted" column;
          Alcotest.(check int) "expected_length" 3 expected_length;
          Alcotest.(check int) "found_length" 3 found_length)

(* --- drop / select --- *)

(* The first drop is the affirmative arm: it removes a column the frame does
   carry, so a [drop] answering [None] for everything cannot reach the second
   arm at all. The second then takes the survivor away, which is the one
   condition under which [drop] answers [None] — the result would have no
   columns. It also drops the frame's head, so an implementation that only
   ever removes from the tail is caught here rather than passing on a
   coincidence. *)
let frame_drop_last_column_is_none () =
  let z = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let m = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  match Cairos.Frame.of_series (Cairos.Nonempty.make ("z", z) [ ("m", m) ]) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame -> (
      match Cairos.Frame.drop "m" frame with
      | None -> Alcotest.fail "dropping one of two columns must leave a frame"
      | Some out ->
          Alcotest.(check (list string))
            "the survivor is kept" [ "z" ]
            (Cairos.Nonempty.to_list (Cairos.Frame.columns out));
          Alcotest.(check (list (list (float 0.001))))
            "values follow the survivor"
            [ [ 1.0; 2.0; 3.0 ] ]
            (frame_cells out);
          Alcotest.(check bool)
            "dropping the only remaining column is None" true
            (Option.is_none (Cairos.Frame.drop "z" out)))

(* An absent name is a no-op returning an equal frame, not [None] and not an
   error, following [get]'s stance that a name the frame does not carry is not
   a failure. The full column list and every value are enumerated: a rebuild
   that reordered the columns, or paired a name with another column's values,
   would pass a membership check.

   The second arm removes a name the frame does carry, on the same fixture.
   Without it a [drop] that never removes anything passes the first. *)
let frame_drop_absent_name_is_noop () =
  let z = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let m = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  let a = Test_helpers.make_daily_series dates_3 [| 7.0; 8.0; 9.0 |] in
  match
    Cairos.Frame.of_series
      (Cairos.Nonempty.make ("z", z) [ ("m", m); ("a", a) ])
  with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame -> (
      (match Cairos.Frame.drop "nope" frame with
      | None -> Alcotest.fail "dropping an absent name must not be None"
      | Some out ->
          Alcotest.(check (list string))
            "every column survives" [ "z"; "m"; "a" ]
            (Cairos.Nonempty.to_list (Cairos.Frame.columns out));
          Alcotest.(check (list (list (float 0.001))))
            "values are untouched"
            [ [ 1.0; 2.0; 3.0 ]; [ 4.0; 5.0; 6.0 ]; [ 7.0; 8.0; 9.0 ] ]
            (frame_cells out));
      match Cairos.Frame.drop "m" frame with
      | None -> Alcotest.fail "dropping a present name must leave a frame"
      | Some out ->
          Alcotest.(check (list string))
            "a name this frame does carry is removed" [ "z"; "a" ]
            (Cairos.Nonempty.to_list (Cairos.Frame.columns out));
          Alcotest.(check (list (list (float 0.001))))
            "the survivors keep their own values"
            [ [ 1.0; 2.0; 3.0 ]; [ 7.0; 8.0; 9.0 ] ]
            (frame_cells out))

(* The request is the reverse of the frame's insertion order and the expected
   list is enumerated in full: request order deliberately does not override
   insertion order, matching [columns]. Only a whole-list comparison sees
   that.

   The second arm requests one name twice. A filter over the frame's columns
   names it once for free; a lookup driven by the request list would emit the
   column twice, and a length or membership check would not notice. *)
let frame_select_uses_frame_order () =
  let z = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let m = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  let a = Test_helpers.make_daily_series dates_3 [| 7.0; 8.0; 9.0 |] in
  match
    Cairos.Frame.of_series
      (Cairos.Nonempty.make ("z", z) [ ("m", m); ("a", a) ])
  with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame -> (
      (match Cairos.Frame.select (Cairos.Nonempty.make "a" [ "z" ]) frame with
      | None -> Alcotest.fail "two present names must select a frame"
      | Some out ->
          Alcotest.(check (list string))
            "frame order, not request order" [ "z"; "a" ]
            (Cairos.Nonempty.to_list (Cairos.Frame.columns out));
          Alcotest.(check (list (list (float 0.001))))
            "values follow their names"
            [ [ 1.0; 2.0; 3.0 ]; [ 7.0; 8.0; 9.0 ] ]
            (frame_cells out));
      match
        Cairos.Frame.select (Cairos.Nonempty.make "a" [ "a"; "m" ]) frame
      with
      | None -> Alcotest.fail "a repeated present name must select a frame"
      | Some out ->
          Alcotest.(check (list string))
            "a repeated request names one column" [ "m"; "a" ]
            (Cairos.Nonempty.to_list (Cairos.Frame.columns out));
          Alcotest.(check (list (list (float 0.001))))
            "the repeat carries its values once"
            [ [ 4.0; 5.0; 6.0 ]; [ 7.0; 8.0; 9.0 ] ]
            (frame_cells out))

(* [None] means here what it means for [drop]: the result would have no
   columns. The empty request cannot reach this — [Nonempty.t] rules it out at
   compile time — so a request naming only absent columns is the only route.

   The second arm mixes an absent name with a present one on the same fixture.
   It pins that absent names are skipped rather than rejected, and it is the
   affirmative arm: a [select] returning [None] for every input passes the
   first assertion alone. *)
let frame_select_no_name_present_is_none () =
  let z = Test_helpers.make_daily_series dates_3 [| 1.0; 2.0; 3.0 |] in
  let m = Test_helpers.make_daily_series dates_3 [| 4.0; 5.0; 6.0 |] in
  match Cairos.Frame.of_series (Cairos.Nonempty.make ("z", z) [ ("m", m) ]) with
  | Error e -> Alcotest.fail (Cairos.Frame.err_to_string e)
  | Ok frame -> (
      Alcotest.(check bool)
        "no requested name is present" true
        (Option.is_none
           (Cairos.Frame.select (Cairos.Nonempty.make "nope" [ "other" ]) frame));
      match Cairos.Frame.select (Cairos.Nonempty.make "nope" [ "m" ]) frame with
      | None -> Alcotest.fail "an absent name must be skipped, not rejected"
      | Some out ->
          Alcotest.(check (list string))
            "the present name is selected" [ "m" ]
            (Cairos.Nonempty.to_list (Cairos.Frame.columns out));
          Alcotest.(check (list (list (float 0.001))))
            "values follow the selected name"
            [ [ 4.0; 5.0; 6.0 ] ]
            (frame_cells out))

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
    ("frame_add_column_appends_last", `Quick, frame_add_column_appends_last);
    ("frame_add_column_duplicate_name", `Quick, frame_add_column_duplicate_name);
    ("frame_add_column_index_mismatch", `Quick, frame_add_column_index_mismatch);
    ("frame_drop_last_column_is_none", `Quick, frame_drop_last_column_is_none);
    ("frame_drop_absent_name_is_noop", `Quick, frame_drop_absent_name_is_noop);
    ("frame_select_uses_frame_order", `Quick, frame_select_uses_frame_order);
    ( "frame_select_no_name_present_is_none",
      `Quick,
      frame_select_no_name_present_is_none );
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
