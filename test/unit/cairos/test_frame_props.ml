(* Property suite for [Cairos.Frame]. RFC 0046 TP-Frame-1..3.

   Each property runs at [~count:200] per RFC 0046 Test Plan; CI/local
   reproducibility is provided by [Qcheck_gen.pin_seed_from_env] (FR-4).

   Frame retrieval round-trips through {!Series.make_unsafe} (frame.ml:55),
   pairing the frame's shared index with the stored values tensor. The
   property suite asserts on the public Frame surface only — column names,
   the [Ok]/[Error] outcome of [of_series], and the values arrays exposed
   via [get] composed with [Series.values]. *)

(* TP-Frame-1 — round-tripping a singleton column through [Frame.of_series]
   and [Frame.get] returns the original values bitwise. The singleton path
   has no other column to mismatch against and no name to duplicate, so
   [of_series] cannot fail for any input from [daily_finite_float_series_arb];
   the [Error] branch is structurally unreachable and terminated with
   [failwith] per ~/.claude/solutions/ocaml/qcheck-generator-failwith.md
   (RFC 0030 §R4). The retrieval branch is similarly unreachable: the column
   "col" was just inserted, so [Frame.get "col"] cannot return [None]. *)
let single_column_round_trip =
  QCheck.Test.make ~count:200 ~name:"single_column_round_trip"
    Qcheck_gen.daily_finite_float_series_arb (fun s ->
      match Cairos.Frame.of_series (Cairos.Nonempty.singleton ("col", s)) with
      | Error _ ->
          (* Unreachable: a singleton has no sibling columns to mismatch
             indices against and no other names to duplicate. RFC 0030 §R4. *)
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

(* TP-Frame-2 — column names appear in insertion order. The pair generator
   builds two daily series with structurally identical indices (same length
   and timestamp arrays — qcheck_gen.ml:153-168), and the column names
   "a" and "b" are distinct, so [of_series] cannot fail; the [Error] branch
   is unreachable per RFC 0030 §R4. Catches a regression that sorts column
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
             the column names "a" and "b" are distinct. RFC 0030 §R4. *)
          failwith
            "unreachable: paired_aligned_daily_arb guarantees identical \
             indices and distinct column names"
      | Ok frame -> Cairos.Frame.columns frame = [ "a"; "b" ])

(* TP-Frame-3 — two-column construction succeeds whenever both series share
   the same index by construction. The negative case (mismatched indices →
   [Error]) is pinned by [of_series_timestamp_mismatch] / [of_series_length_mismatch]
   in test_frame.ml; this property pins the positive case across all input
   shapes [paired_aligned_daily_arb] produces. *)
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

let () =
  Qcheck_gen.pin_seed_from_env ();
  let tests =
    List.map QCheck_alcotest.to_alcotest
      [
        single_column_round_trip;
        columns_in_insertion_order;
        two_column_construction_succeeds_on_identical_indices;
      ]
  in
  Alcotest.run "Frame.props" [ ("property", tests) ]
