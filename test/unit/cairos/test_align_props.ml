(* Property suite for [Cairos.Align]. RFC 0046 TP-Align-1..4.

   Each property runs at [~count:200] per RFC 0046 Test Plan; CI/local
   reproducibility is provided by [Qcheck_gen.pin_seed_from_env] (FR-4).

   The [aligned] type is abstract (CONTRIBUTING.md §IV / align.mli:6); every
   property below inspects results only through the public accessors
   [Align.index], [Align.left], [Align.right]. *)

(* TP-Align-1 — Inner alignment's index length is bounded above by
   [min len_a len_b]. The intersection cannot exceed either side's cardinality.

   [paired_overlapping_daily_arb] guarantees overlap >= 1 by construction, so
   [Inner] always returns [Ok] for inputs from this generator; the [Error]
   branch is structurally unreachable. Per
   ~/.claude/solutions/ocaml/qcheck-generator-failwith.md (RFC 0030 §R4), an
   unreachable test branch is terminated with [failwith] — propagating the
   property to [false] would mis-classify a generator-internal failure as a
   library bug, and the QCheck shrinker would then mis-report the
   counter-example. *)
let inner_length_bounded_by_min =
  QCheck.Test.make ~count:200 ~name:"inner_length_bounded_by_min"
    Qcheck_gen.paired_overlapping_daily_arb (fun (a, b) ->
      match Cairos.Align.align ~strategy:`Inner a b with
      | Error _ ->
          (* Unreachable: paired_overlapping_daily_arb guarantees overlap >= 1.
             RFC 0030 §R4. *)
          failwith
            "unreachable: paired_overlapping_daily_arb guarantees non-empty \
             intersection"
      | Ok aligned ->
          Cairos.Index.length (Cairos.Align.index aligned)
          <= min (Cairos.Series.length a) (Cairos.Series.length b))

(* TP-Align-2 — Left alignment preserves the left input's index length.
   Per align.mli:32-33, [Left] always returns [Ok] for non-empty left input
   and the resulting index *is* the left input's index. The
   [paired_overlapping_daily_arb] generator never produces empty inputs, so
   [Error] is unreachable and terminated per RFC 0030 §R4. *)
let left_length_equals_left_input =
  QCheck.Test.make ~count:200 ~name:"left_length_equals_left_input"
    Qcheck_gen.paired_overlapping_daily_arb (fun (a, b) ->
      match Cairos.Align.align ~strategy:`Left a b with
      | Error _ ->
          (* Unreachable: Left always returns Ok for non-empty left input.
             RFC 0030 §R4. *)
          failwith
            "unreachable: Left strategy returns Ok for non-empty left input"
      | Ok aligned ->
          Cairos.Index.length (Cairos.Align.index aligned)
          = Cairos.Series.length a)

(* TP-Align-3 — when both series share a structurally identical index, the
   three live strategies (Inner, Left, Asof Backward) produce results whose
   index has the same length as both inputs and whose [left] / [right]
   accessors expose the input values arrays bitwise. Catches an asymmetry
   between strategies on the trivial input.

   [Asof Forward] is omitted from the collapse check because the RFC §Test
   Plan for TP-Align-3 names exactly Inner, Left, Asof Backward.

   [paired_aligned_daily_arb] makes both indices identical, so [Inner] has
   the full overlap, [Left] returns [Ok] for the non-empty left input, and
   [Asof Backward] matches every left timestamp to itself on the right.
   [Error] is therefore unreachable for all three strategies — terminated
   with [failwith] per RFC 0030 §R4. *)
let identical_indices_collapse_strategies =
  QCheck.Test.make ~count:200 ~name:"identical_indices_collapse_strategies"
    Qcheck_gen.paired_aligned_daily_arb (fun (a, b) ->
      let len_a = Cairos.Series.length a in
      let len_b = Cairos.Series.length b in
      let a_values = Nx.to_array (Cairos.Series.values a) in
      let b_values = Nx.to_array (Cairos.Series.values b) in
      let check strategy =
        match Cairos.Align.align ~strategy a b with
        | Error _ ->
            (* Unreachable: identical indices guarantee Ok across the three
               strategies under test. RFC 0030 §R4. *)
            failwith
              "unreachable: identical indices guarantee Ok for Inner/Left/Asof"
        | Ok aligned ->
            let idx_len = Cairos.Index.length (Cairos.Align.index aligned) in
            let lhs = Nx.to_array (Cairos.Align.left aligned) in
            let rhs = Nx.to_array (Cairos.Align.right aligned) in
            idx_len = len_a
            && idx_len = len_b
            && Qcheck_gen.float_arrays_bitwise_equal lhs a_values
            && Qcheck_gen.float_arrays_bitwise_equal rhs b_values
      in
      check `Inner && check `Left && check (`Asof `Backward))

(* TP-Align-4 — the index of an Ok Inner alignment is strictly monotonically
   increasing. Catches a regression that admits duplicate or out-of-order
   timestamps in the intersection (e.g. a sorted-merge bug that emits a
   matched timestamp twice or fails to advance one pointer). [Error] from
   Inner on [paired_overlapping_daily_arb] is unreachable per TP-Align-1's
   reasoning; terminated with [failwith] per RFC 0030 §R4. *)
let inner_index_strictly_monotonic =
  QCheck.Test.make ~count:200 ~name:"inner_index_strictly_monotonic"
    Qcheck_gen.paired_overlapping_daily_arb (fun (a, b) ->
      match Cairos.Align.align ~strategy:`Inner a b with
      | Error _ ->
          (* Unreachable: paired_overlapping_daily_arb guarantees overlap >= 1.
             RFC 0030 §R4. *)
          failwith
            "unreachable: paired_overlapping_daily_arb guarantees non-empty \
             intersection"
      | Ok aligned ->
          let ts = Cairos.Index.timestamps (Cairos.Align.index aligned) in
          ts
          |> Array.to_seqi
          |> Seq.for_all (fun (i, t) -> i = 0 || Ptime.compare ts.(i - 1) t < 0))

let () =
  Qcheck_gen.pin_seed_from_env ();
  let tests =
    List.map QCheck_alcotest.to_alcotest
      [
        inner_length_bounded_by_min;
        left_length_equals_left_input;
        identical_indices_collapse_strategies;
        inner_index_strictly_monotonic;
      ]
  in
  Alcotest.run "Align.props" [ ("property", tests) ]
