(* Property suite for [Cairos.Frame].

   Each property runs at [~count:200]; CI/local
   reproducibility is provided by [Qcheck_gen.pin_seed_from_env].

   Frame retrieval round-trips through {!Series.make_unsafe}, called from
   [get] and [columns_with_values] in lib/frame.ml,
   pairing the frame's shared index with the stored values tensor. The
   property suite asserts on the public Frame surface only — column names,
   the [Ok]/[Error] outcome of [of_series], and the values arrays exposed
   via [get] composed with [Series.values]. *)

(* Round-tripping a singleton column through [Frame.of_series]
   and [Frame.get] returns the original values bitwise. The singleton path
   has no other column to mismatch against and no name to duplicate, so
   [of_series] cannot fail for any input from [daily_finite_float_series_arb];
   the [Error] branch is structurally unreachable and terminated with
   [failwith]. The retrieval branch is similarly unreachable: the column
   "col" was just inserted, so [Frame.get "col"] cannot return [None]. *)
let single_column_round_trip =
  QCheck.Test.make ~count:200 ~name:"single_column_round_trip"
    Qcheck_gen.daily_finite_float_series_arb (fun s ->
      match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("col", s)) with
      | Error _ ->
          (* Unreachable: a singleton has no sibling columns to mismatch
             indices against and no other names to duplicate. *)
          failwith
            "unreachable: Frame.of_series on a singleton cannot fail validation"
      | Ok frame -> (
          match Cairos.Frame.get "col" frame with
          | None ->
              (* Unreachable: the column was just inserted under this name. *)
              failwith
                "unreachable: just-inserted column \"col\" must be retrievable"
          | Some retrieved ->
              let expected = Nx.to_array (Cairos.Series.values s) in
              let actual = Nx.to_array (Cairos.Series.values retrieved) in
              Qcheck_gen.float_arrays_bitwise_equal expected actual))

(* Column names appear in insertion order. The pair generator
   builds two daily series with structurally identical indices (same length
   and timestamp arrays — qcheck_gen.ml:153-168), and the column names
   "a" and "b" are distinct, so [of_series] cannot fail; the [Error] branch
   is unreachable. Catches a regression that sorts column
   names (e.g. alphabetically or by hash). *)
let columns_in_insertion_order =
  QCheck.Test.make ~count:200 ~name:"columns_in_insertion_order"
    Qcheck_gen.paired_aligned_daily_arb (fun (left, right) ->
      match
        Cairos.Frame.of_series
          (Cairos.Nonempty.make ("a", left) [ ("b", right) ])
      with
      | Error _ ->
          (* Unreachable: paired_aligned_daily_arb gives identical indices and
             the column names "a" and "b" are distinct. *)
          failwith
            "unreachable: paired_aligned_daily_arb guarantees identical \
             indices and distinct column names"
      | Ok frame ->
          Cairos.Nonempty.to_list (Cairos.Frame.columns frame) = [ "a"; "b" ])

(* Two-column construction succeeds whenever both series share
   the same index by construction. The negative case (mismatched indices →
   [Error]) is pinned by [of_series_timestamp_mismatch] /
   [frame_index_mismatch_variant] in test_frame.ml; this property pins the
   positive case across all input shapes [paired_aligned_daily_arb] produces. *)
let two_column_construction_succeeds_on_identical_indices =
  QCheck.Test.make ~count:200
    ~name:"two_column_construction_succeeds_on_identical_indices"
    Qcheck_gen.paired_aligned_daily_arb (fun (left, right) ->
      match
        Cairos.Frame.of_series
          (Cairos.Nonempty.make ("a", left) [ ("b", right) ])
      with
      | Ok _ -> true
      | Error _ -> false)

(* [(name, values)] for every column, in frame order. Values come out through
   [to_series] composed with [Series.values], the same public path the three
   properties above use, so a column-operation regression is observed on the
   surface a caller sees rather than on the record literal. *)
let named_values frame =
  List.map
    (fun (name, s) -> (name, Nx.to_array (Cairos.Series.values s)))
    (Cairos.Nonempty.to_list (Cairos.Frame.to_series frame))

(* Bitwise on the values, so a NaN cell — ~5% of the cells
   [daily_frame_finite_floats_with_nan_arb] draws — is compared honestly
   instead of failing on IEEE inequality. *)
let named_values_equal xs ys =
  List.length xs = List.length ys
  && List.for_all2
       (fun (nx, vx) (ny, vy) ->
         String.equal nx ny && Qcheck_gen.float_arrays_bitwise_equal vx vy)
       xs ys

(* The name added by the two properties below. The frame arbitraries name their
   columns [c0..c{C-1}] (qcheck_gen.mli, "Frame arbitraries"), so this name is
   absent from every generated frame and [add_column] cannot answer
   [Duplicate_column]. *)
let fresh_name = "added"

(* Borrow a column of [frame] as the series to add back under {!fresh_name}.
   Taking it from the frame itself is what makes [add_column]'s index check
   pass by construction: [get] pairs the frame's own shared index with the
   stored values, so [Index_mismatch] is unreachable too. *)
let borrow_first_column frame =
  match
    Cairos.Frame.get (Cairos.Nonempty.hd (Cairos.Frame.columns frame)) frame
  with
  | Some s -> s
  | None ->
      (* Unreachable: the name came from [columns frame]. *)
      failwith "unreachable: a name from [columns frame] must be retrievable"

let extend frame =
  match
    Cairos.Frame.add_column fresh_name (borrow_first_column frame) frame
  with
  | Ok extended -> extended
  | Error _ ->
      (* Unreachable: [fresh_name] is absent from the generated frames and the
         borrowed series carries the frame's own index. *)
      failwith
        "unreachable: Frame.add_column with a fresh name and the frame's own \
         index cannot fail"

(* Adding a column and dropping it by name is identity on both names and
   values; select-of-all-columns is identity; and dropping one column leaves
   the survivors in their original relative order with their values intact.

   Each [None] arm below is a contract violation by the function under test —
   never a broken library invariant — so it fails the property rather than
   raising.

   The three conjuncts are reported separately rather than being [&&]-ed into
   one boolean: they pin three different claims about three different
   functions, and a bare [false] against a shrunk frame does not say which. *)
let frame_add_then_drop_round_trips =
  QCheck.Test.make ~count:200 ~name:"frame_add_then_drop_round_trips"
    Qcheck_gen.daily_frame_finite_floats_with_nan_arb (fun frame ->
      match named_values frame with
      | [] ->
          (* Unreachable: [to_series] returns a [Nonempty.t]. Raising rather
             than answering [false] because this arm means the non-emptiness
             invariant broke, not that a column operation misbehaved. *)
          failwith "unreachable: a Frame.t always has at least one column"
      | (first_name, first_values) :: rest as original ->
          let extended = extend frame in
          let names f = String.concat "," (List.map fst (named_values f)) in
          let round_trips =
            List.map fst (named_values extended)
            = List.map fst original @ [ fresh_name ]
            &&
            match Cairos.Frame.drop fresh_name extended with
            | None -> false
            | Some restored ->
                named_values_equal (named_values restored) original
          in
          let select_all_is_identity =
            match Cairos.Frame.select (Cairos.Frame.columns frame) frame with
            | None -> false
            | Some selected ->
                named_values_equal (named_values selected) original
          in
          (* Dropped from [extended] rather than from [frame] so a real column
             is removed for every generated shape — a one-column [frame] would
             force the absent-name no-op here, which pins nothing about
             ordering. The survivors are [rest] followed by the added column,
             so this catches both a reorder and a wrong column removed. *)
          let drop_preserves_survivor_order =
            match Cairos.Frame.drop first_name extended with
            | None -> false
            | Some dropped ->
                named_values_equal (named_values dropped)
                  (rest @ [ (fresh_name, first_values) ])
          in
          if not round_trips then
            QCheck.Test.fail_reportf
              "add-then-drop is not identity on a frame with columns [%s] \
               (extended: [%s])"
              (names frame) (names extended)
          else if not select_all_is_identity then
            QCheck.Test.fail_reportf
              "select of every column is not identity on a frame with columns \
               [%s]"
              (names frame)
          else if not drop_preserves_survivor_order then
            QCheck.Test.fail_reportf
              "dropping %S from [%s] did not leave the survivors in order"
              first_name (names extended)
          else true)

(* All three column operations hand back the input's [Index.t] physically, not
   a rebuilt equal one. Pins frame.mli's "the returned frame shares [frame]'s
   index" on [add_column], [drop], and [select]; the engine relies on it to
   avoid re-deriving timestamps per derived frame. Structural checks cannot see
   this — an equal-but-distinct index passes every one of them, which is why the
   assertion is [==] rather than a timestamp comparison. *)
let frame_column_ops_preserve_index =
  QCheck.Test.make ~count:200 ~name:"frame_column_ops_preserve_index"
    Qcheck_gen.daily_frame_finite_floats_with_nan_arb (fun frame ->
      let shared = Cairos.Frame.index frame in
      let extended = extend frame in
      let dropped_shares =
        (* [extended] has ≥ 2 columns, so this removes a real column and still
           leaves a frame. Its own index is asserted to be [shared] first, so a
           pass here is [drop] carrying the input's index through. *)
        match
          Cairos.Frame.drop
            (Cairos.Nonempty.hd (Cairos.Frame.columns frame))
            extended
        with
        | None -> false
        | Some f -> Cairos.Frame.index f == shared
      in
      let selected_shares =
        match Cairos.Frame.select (Cairos.Frame.columns frame) frame with
        | None -> false
        | Some f -> Cairos.Frame.index f == shared
      in
      Cairos.Frame.index extended == shared && dropped_shares && selected_shares)

let () =
  Qcheck_gen.pin_seed_from_env ();
  let tests =
    List.map QCheck_alcotest.to_alcotest
      [
        single_column_round_trip;
        columns_in_insertion_order;
        two_column_construction_succeeds_on_identical_indices;
        frame_add_then_drop_round_trips;
        frame_column_ops_preserve_index;
      ]
  in
  Alcotest.run "Frame.props" [ ("property", tests) ]
