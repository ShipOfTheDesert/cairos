(* Property suite for [Cairos.Align].

   Each property runs at [~count:200]; CI/local
   reproducibility is provided by [Qcheck_gen.pin_seed_from_env].

   The [aligned] type is abstract (CONTRIBUTING.md §IV / align.mli:6); every
   property below inspects results only through the public accessors
   [Align.index], [Align.left], [Align.right]. *)

(* Inner alignment's index length is bounded above by
   [min len_a len_b]. The intersection cannot exceed either side's cardinality.

   [paired_overlapping_daily_arb] guarantees overlap >= 1 by construction, so
   [Inner] always returns [Ok] for inputs from this generator; the [Error]
   branch is structurally unreachable. It is terminated with [failwith] rather
   than [false] because reaching it means either [align] or the generator's
   overlap guarantee is broken — an invariant failure, not the contract
   violation a property counterexample describes. Shrinking is unaffected by
   the choice: QCheck shrinks a raised exception in a property body exactly as
   it shrinks a [false] return. See ocaml/qcheck-generator-failwith.md. *)
let inner_length_bounded_by_min =
  QCheck.Test.make ~count:200 ~name:"inner_length_bounded_by_min"
    Qcheck_gen.paired_overlapping_daily_arb (fun (a, b) ->
      match Cairos.Align.align ~strategy:`Inner a b with
      | Error _ ->
          (* Unreachable: paired_overlapping_daily_arb guarantees overlap >= 1. *)
          failwith
            "unreachable: paired_overlapping_daily_arb guarantees non-empty \
             intersection"
      | Ok aligned ->
          Cairos.Index.length (Cairos.Align.index aligned)
          <= min (Cairos.Series.length a) (Cairos.Series.length b))

(* Left alignment preserves the left input's index length.
   Per align.mli:32-33, [Left] always returns [Ok] for non-empty left input
   and the resulting index *is* the left input's index. The
   [paired_overlapping_daily_arb] generator never produces empty inputs, so
   [Error] is unreachable and terminated. *)
let left_length_equals_left_input =
  QCheck.Test.make ~count:200 ~name:"left_length_equals_left_input"
    Qcheck_gen.paired_overlapping_daily_arb (fun (a, b) ->
      match Cairos.Align.align ~strategy:`Left a b with
      | Error _ ->
          (* Unreachable: Left always returns Ok for non-empty left input. *)
          failwith
            "unreachable: Left strategy returns Ok for non-empty left input"
      | Ok aligned ->
          Cairos.Index.length (Cairos.Align.index aligned)
          = Cairos.Series.length a)

(* When both series share a structurally identical index, the
   three live strategies (Inner, Left, Asof Backward) produce results whose
   index has the same length as both inputs and whose [left] / [right]
   accessors expose the input values arrays bitwise. Catches an asymmetry
   between strategies on the trivial input.

   [Asof Forward] is omitted from the collapse check because the collapse
   check covers exactly Inner, Left, Asof Backward.

   [paired_aligned_daily_arb] makes both indices identical, so [Inner] has
   the full overlap, [Left] returns [Ok] for the non-empty left input, and
   [Asof Backward] matches every left timestamp to itself on the right.
   [Error] is therefore unreachable for all three strategies — terminated
   with [failwith]. *)
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
               strategies under test. *)
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

(* The index of an Ok Inner alignment is strictly monotonically
   increasing. Catches a regression that admits duplicate or out-of-order
   timestamps in the intersection (e.g. a sorted-merge bug that emits a
   matched timestamp twice or fails to advance one pointer). [Error] from
   Inner on [paired_overlapping_daily_arb] is unreachable by the
   length-bound property's reasoning; terminated with [failwith]. *)
let inner_index_strictly_monotonic =
  QCheck.Test.make ~count:200 ~name:"inner_index_strictly_monotonic"
    Qcheck_gen.paired_overlapping_daily_arb (fun (a, b) ->
      match Cairos.Align.align ~strategy:`Inner a b with
      | Error _ ->
          (* Unreachable: paired_overlapping_daily_arb guarantees overlap >= 1. *)
          failwith
            "unreachable: paired_overlapping_daily_arb guarantees non-empty \
             intersection"
      | Ok aligned ->
          let ts = Cairos.Index.timestamps (Cairos.Align.index aligned) in
          ts
          |> Array.to_seqi
          |> Seq.for_all (fun (i, t) -> i = 0 || Ptime.compare ts.(i - 1) t < 0))

(* [map2_nan] yields NaN exactly at the union of the input-NaN positions and
   agrees with [map2] everywhere else (align.mli).

   "Exactly" is only well-defined because [f] is drawn from functions that are
   NaN-free on finite inputs — gating is on inputs only, so an [f] that could
   emit NaN from a clean pair would put a NaN at an ungated position and make
   the property false as stated. Division is excluded for that reason
   ([0.0 /. 0.0] is NaN). *)
let nan_free_on_finite_fns =
  [|
    ("add", ( +. ));
    ("sub", ( -. ));
    ("mul", ( *. ));
    ("gt_indicator", fun a b -> if a > b then 1.0 else 0.0);
    ("max", Float.max);
    ("min", Float.min);
  |]

(* Cells are NaN with probability ~5% (one roll in twenty), mirroring
   [Qcheck_gen.daily_frame_finite_floats_with_nan_arb]'s injection rate; the
   rest are finite draws in [-10, 10]. Composed here rather than added to
   [qcheck_gen.mli] — it has one consumer. *)
let nan_injected_pair_arb =
  let open QCheck in
  let cell =
    let open Gen in
    let* roll = int_range 0 19 in
    if roll = 0 then return Float.nan else float_range (-10.0) 10.0
  in
  let gen =
    let open Gen in
    let* n = int_range 1 64 in
    let* xs_a = array_size (return n) cell in
    let* xs_b = array_size (return n) cell in
    let* f_idx = int_range 0 (Array.length nan_free_on_finite_fns - 1) in
    return (xs_a, xs_b, f_idx)
  in
  let count_nan xs =
    Array.fold_left (fun n x -> if Float.is_nan x then n + 1 else n) 0 xs
  in
  make
    ~print:(fun (xs_a, xs_b, f_idx) ->
      Printf.sprintf "<len=%d nan_left=%d nan_right=%d f=%s>"
        (Array.length xs_a) (count_nan xs_a) (count_nan xs_b)
        (fst nan_free_on_finite_fns.(f_idx)))
    gen

let map2_nan_nan_exactly_at_union_of_input_nans =
  QCheck.Test.make ~count:200
    ~name:"map2_nan_nan_exactly_at_union_of_input_nans" nan_injected_pair_arb
    (fun (xs_a, xs_b, f_idx) ->
      let f = snd nan_free_on_finite_fns.(f_idx) in
      let a = Qcheck_gen.make_series_from_floats ~freq:Cairos.Freq.Day xs_a in
      let b = Qcheck_gen.make_series_from_floats ~freq:Cairos.Freq.Day xs_b in
      match Cairos.Align.align ~strategy:`Inner a b with
      | Error _ ->
          (* Unreachable: both series share an identical index by construction,
             so Inner has the full overlap and never yields an empty result. *)
          failwith
            "unreachable: identical indices guarantee a non-empty intersection"
      | Ok aligned ->
          let actual =
            Nx.to_array
              (Cairos.Series.values (Cairos.Align.map2_nan aligned ~f))
          in
          let plain =
            Nx.to_array (Cairos.Series.values (Cairos.Align.map2 f aligned))
          in
          (* The oracle is derived from the contract, not from [map2]: at a
             gated position the contract says NaN, elsewhere it says [f] applied
             to the drawn inputs. Reusing [plain] here would let a shared defect
             in the align/to_array plumbing agree with itself. *)
          let expected =
            Array.init (Array.length xs_a) (fun i ->
                if Float.is_nan xs_a.(i) || Float.is_nan xs_b.(i) then Float.nan
                else f xs_a.(i) xs_b.(i))
          in
          (* The comparator's NaN branches carry these assertions: both-NaN must
             compare equal for the gated positions, one-sided-NaN must compare
             unequal so a wrongly-gated or wrongly-passed position fails. Both
             branches are pinned in test_align.ml. *)
          let matches_contract =
            Array.length actual = Array.length expected
            && Array.for_all2
                 (Qcheck_gen.float_approx_equal ~tol:1e-12)
                 expected actual
          in
          (* The second clause: [map2_nan] agrees with [map2] at the ungated
             positions, where the two must be indistinguishable. Compared
             [actual] against [plain] directly — comparing the oracle against
             [plain] would assert a fact about the oracle instead. Only the
             gated positions are skipped ([Float.is_nan x] where [x] is
             [actual]); a NaN [plain] produces at an ungated position must
             still fail, so it is not skipped. *)
          let agrees_with_map2_off_gate =
            Array.length actual = Array.length plain
            && Array.for_all2
                 (fun x y ->
                   Float.is_nan x
                   || Qcheck_gen.float_approx_equal ~tol:1e-12 x y)
                 actual plain
          in
          matches_contract && agrees_with_map2_off_gate)

let () =
  Qcheck_gen.pin_seed_from_env ();
  let tests =
    List.map QCheck_alcotest.to_alcotest
      [
        inner_length_bounded_by_min;
        left_length_equals_left_input;
        identical_indices_collapse_strategies;
        inner_index_strictly_monotonic;
        map2_nan_nan_exactly_at_union_of_input_nans;
      ]
  in
  Alcotest.run "Align.props" [ ("property", tests) ]
