(* Property suite for [Cairos.Series] (excluding scan/dropna which have their
   own files) and [Series.ffill]. RFC 0046 TP-Series-1..5.

   Each property runs at [~count:200] per RFC 0046 Test Plan; CI/local
   reproducibility is provided by [Qcheck_gen.pin_seed_from_env] (FR-4). *)

(* TP-Series-1 — [Series.map] with the Nx-tensor identity preserves the values
   array bitwise. The closure is [(fun t -> t)] at the Nx level (not a float
   identity) per series.mli:54: [Series.map] operates on the values payload as
   a whole. See ~/.claude/solutions/ocaml/cairos-series-map-is-tensor-level.md.
   Catches a regression that allocates a non-identical tensor on the identity
   path (e.g. drops NaN bit patterns under spurious arithmetic). *)
let map_identity_is_identity =
  QCheck.Test.make ~count:200 ~name:"map_identity_is_identity"
    Qcheck_gen.daily_float_series_arb (fun s ->
      let mapped = Cairos.Series.map (fun t -> t) s in
      Qcheck_gen.float_arrays_bitwise_equal
        (Nx.to_array (Cairos.Series.values mapped))
        (Nx.to_array (Cairos.Series.values s)))

(* TP-Series-2 — [Series.map] preserves length. Distinct from TP-Series-1
   because length is read from the index, not the values tensor. Catches a
   regression that decouples the index from a mapped output (e.g. swapping in
   a mismatched index by accident). *)
let map_preserves_length =
  QCheck.Test.make ~count:200 ~name:"map_preserves_length"
    Qcheck_gen.daily_float_series_arb (fun s ->
      let mapped = Cairos.Series.map (fun t -> t) s in
      Cairos.Series.length mapped = Cairos.Series.length s)

(* TP-Series-3 — [shift n (shift -n s)] preserves the trailing window starting
   at position [n]: positions [0, n) become NaN by construction (vacated
   leading positions per series.mli:64-68), and positions [n, length) recover
   the original values [old.(n), ..., old.(length - 1)] bitwise. Skipped at
   [n = 0] (trivial identity).

   The RFC §Test Plan code listing for TP-Series-3 has a typo: the right-side
   slice index is [0; length-n], which would be the *leading* window of the
   original. With [shift n (shift -n s)] the round-trip preserves the
   *trailing* window, not the leading one (verified by tracing the in-library
   implementation at lib/series.ml:33-47). The task plan note ("trailing
   window of length length - n to avoid the boundary NaNs shift introduces")
   describes the trailing-window form; this implementation honours the task
   plan and slices [R (n, length)] on both sides. *)
let series_with_shift_arb =
  let open QCheck in
  let gen =
    let open Gen in
    let* s = get_gen Qcheck_gen.daily_finite_float_series_arb in
    let len = Cairos.Series.length s in
    let* n = int_range 0 (max 0 (len - 1)) in
    return (s, n)
  in
  make
    ~print:(fun (s, n) ->
      Printf.sprintf "<series len=%d shift n=%d>" (Cairos.Series.length s) n)
    gen

let shift_round_trip_overlap_is_identity =
  QCheck.Test.make ~count:200 ~name:"shift_round_trip_overlap_is_identity"
    series_with_shift_arb (fun (s, n) ->
      QCheck.assume (n > 0);
      let len = Cairos.Series.length s in
      let round_tripped = Cairos.Series.shift n (Cairos.Series.shift (-n) s) in
      let lhs =
        Nx.to_array
          (Nx.slice [ R (n, len) ] (Cairos.Series.values round_tripped))
      in
      let rhs =
        Nx.to_array (Nx.slice [ R (n, len) ] (Cairos.Series.values s))
      in
      Qcheck_gen.float_arrays_bitwise_equal lhs rhs)

(* TP-Series-4 — [Series.slice]'s clamped-length contract. For arbitrary
   [start] and [stop] drawn from [[-len/2, 3 * len/2]] (a length-aware range
   that scales the bounds with the series's own length), the output length
   equals [max 0 (min stop len - max start 0)].

   This formula is equivalent to the in-library
   [max 0 (clamped_stop - clamped_start)] computation (verified by case
   analysis on whether [start]/[stop] fall above [len], inside [0..len], or
   below 0 — both produce identical answers). Catches a regression in the
   bounds-clamping logic (e.g. forgetting to floor at 0 or to cap at len).

   The length-aware bounds replace the original fixed [(-32, 96)] range so
   the in-range inverted-bound cell ([start, stop ∈ [0, len]] with
   [start > stop]) is hit at ~12% of cases instead of ~3%. The deterministic
   [slice_empty_range_produces_empty_series] case in [test_series.ml] still
   pins one specific tuple in that cell; the property adds shape coverage
   on top. *)
let series_with_slice_bounds_arb =
  let open QCheck in
  let gen =
    let open Gen in
    let* s = get_gen Qcheck_gen.daily_float_series_arb in
    let len = Cairos.Series.length s in
    let lo = -len / 2 in
    let hi = (3 * len / 2) + 1 in
    let* start = int_range lo hi in
    let* stop = int_range lo hi in
    return (s, start, stop)
  in
  make
    ~print:(fun (s, start, stop) ->
      Printf.sprintf "<series len=%d slice start=%d stop=%d>"
        (Cairos.Series.length s) start stop)
    gen

let slice_clamped_length_formula =
  QCheck.Test.make ~count:200 ~name:"slice_clamped_length_formula"
    series_with_slice_bounds_arb (fun (s, start, stop) ->
      let len = Cairos.Series.length s in
      let result = Cairos.Series.slice ~start ~stop s in
      let expected = max 0 (min stop len - max start 0) in
      Cairos.Series.length result = expected)

(* TP-Series-5 — after [Series.ffill], every position at or after the first
   non-NaN index of the input is non-NaN in the output. Empty input or
   all-NaN input returns [None] from [first_valid] and the property holds
   vacuously. Length is unchanged. Catches a regression where ffill drops or
   skips an element. *)
let ffill_no_nan_after_first_valid =
  QCheck.Test.make ~count:200 ~name:"ffill_no_nan_after_first_valid"
    Qcheck_gen.daily_float_series_arb (fun s ->
      let filled = Cairos.Series.ffill s in
      let len_unchanged =
        Cairos.Series.length filled = Cairos.Series.length s
      in
      match Cairos.Series.first_valid s with
      | None -> len_unchanged
      | Some (i0, _) ->
          let arr = Nx.to_array (Cairos.Series.values filled) in
          let no_nan_after_i0 =
            arr
            |> Array.to_seqi
            |> Seq.for_all (fun (j, x) -> j < i0 || not (Float.is_nan x))
          in
          len_unchanged && no_nan_after_i0)

let () =
  Qcheck_gen.pin_seed_from_env ();
  let tests =
    List.map QCheck_alcotest.to_alcotest
      [
        map_identity_is_identity;
        map_preserves_length;
        shift_round_trip_overlap_is_identity;
        slice_clamped_length_formula;
        ffill_no_nan_after_first_valid;
      ]
  in
  Alcotest.run "Series.props" [ ("property", tests) ]
