(* Property suite for [Cairos.Series] (excluding scan/dropna which have their
   own files) and [Series.ffill].

   Each property runs at [~count:200]; CI/local
   reproducibility is provided by [Qcheck_gen.pin_seed_from_env]. *)

(* [Series.map] with the Nx-tensor identity preserves the values
   array bitwise. The closure is [(fun t -> t)] at the Nx level (not a float
   identity) per series.mli:54: [Series.map] operates on the values payload as
   a whole.
   Catches a regression that allocates a non-identical tensor on the identity
   path (e.g. drops NaN bit patterns under spurious arithmetic). *)
let map_identity_is_identity =
  QCheck.Test.make ~count:200 ~name:"map_identity_is_identity"
    Qcheck_gen.daily_float_series_arb (fun s ->
      let mapped = Cairos.Series.map (fun t -> t) s in
      Qcheck_gen.float_arrays_bitwise_equal
        (Nx.to_array (Cairos.Series.values mapped))
        (Nx.to_array (Cairos.Series.values s)))

(* [Series.map] preserves length. Distinct from the bitwise-identity property
   because length is read from the index, not the values tensor. Catches a
   regression that decouples the index from a mapped output (e.g. swapping in
   a mismatched index by accident). *)
let map_preserves_length =
  QCheck.Test.make ~count:200 ~name:"map_preserves_length"
    Qcheck_gen.daily_float_series_arb (fun s ->
      let mapped = Cairos.Series.map (fun t -> t) s in
      Cairos.Series.length mapped = Cairos.Series.length s)

(* [shift n (shift -n s)] preserves the trailing window starting
   at position [n]: positions [0, n) become NaN by construction (vacated
   leading positions per series.mli:64-68), and positions [n, length) recover
   the original values [old.(n), ..., old.(length - 1)] bitwise. Skipped at
   [n = 0] (trivial identity).

   With [shift n (shift -n s)] the round-trip preserves the
   *trailing* window, not the leading one (verified by tracing the in-library
   implementation of [shift] in lib/series.ml): the trailing
   window has length length - n, avoiding the boundary NaNs shift introduces.
   This implementation slices [R (n, length)] on both sides. *)
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

(* [Series.slice]'s clamped-length contract. For arbitrary
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

(* After [Series.ffill], every position at or after the first
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

(* [Series.make] rejects a 0-dimensional values tensor at every dtype and
   every index length, and reaches that rejection without raising.

   "Never raises" is the property, not a formality: before the guard,
   [(Nx.shape values).(0)] raised [Invalid_argument] on a 0-d tensor, which is
   the only place in [lib/] where a library function could raise. An
   escaped exception fails the QCheck test, so the two halves of the claim are
   pinned by the same run.

   Every draw discriminates the guard from the length-mismatch check, since the
   assertion names [Zero_dimensional_values] rather than merely [Error]. Index
   length 0 is still drawn deliberately: at that length an implementation that
   read a 0-d tensor as "length 0" returns [Ok], which is the coarsest form of
   the same failure and is worth covering directly. The four dtypes span
   both dtype value representations ([float] and boxed [int32]/[int64]), since
   the shape read the guard protects is dtype-agnostic and a guard written
   against one representation would be caught by the others. *)
type zero_dim_values =
  | F64 of float
  | F32 of float
  | I32 of int32
  | I64 of int64

(* Daily index of [n] synthetic timestamps stepping from the shared epoch;
   [n = 0] yields the empty index. The [Error] branch is unreachable (strictly
   increasing finite POSIX seconds) and terminates with [failwith] per the
   generator carve-out: a generator must hand back an [Index.t], not a
   [result]. *)
let daily_index_of_length n =
  let ts =
    Array.init n (fun i ->
        Qcheck_gen.epoch_2024_01_01_utc +. (float_of_int i *. 86_400.0))
  in
  match Cairos.Index.of_unix_floats Cairos.Freq.Day ts with
  | Ok idx -> idx
  | Error e ->
      failwith
        ("test_series_props: of_unix_floats: " ^ Cairos.Index.err_to_string e)

let zero_dim_case_arb =
  let open QCheck in
  let gen =
    let open Gen in
    let* n = int_range 0 8 in
    let* values =
      oneof
        [
          (float_range (-1e6) 1e6 >|= fun x -> F64 x);
          (float_range (-1e6) 1e6 >|= fun x -> F32 x);
          (int_range (-1000) 1000 >|= fun x -> I32 (Int32.of_int x));
          (int_range (-1000) 1000 >|= fun x -> I64 (Int64.of_int x));
        ]
    in
    return (n, values)
  in
  let dtype_label = function
    | F64 _ -> "float64"
    | F32 _ -> "float32"
    | I32 _ -> "int32"
    | I64 _ -> "int64"
  in
  make
    ~print:(fun (n, values) ->
      Printf.sprintf "<0-d %s values, index len=%d>" (dtype_label values) n)
    gen

let rejected_as_zero_dim = function
  | Ok _ -> false
  | Error Cairos.Series.Zero_dimensional_values -> true
  | Error (Cairos.Series.Length_mismatch _) -> false

let prop_series_make_zero_dim_never_raises =
  QCheck.Test.make ~count:200 ~name:"prop_series_make_zero_dim_never_raises"
    zero_dim_case_arb (fun (n, values) ->
      let index = daily_index_of_length n in
      match values with
      | F64 x ->
          rejected_as_zero_dim
            (Cairos.Series.make index (Nx.scalar Nx.float64 x))
      | F32 x ->
          rejected_as_zero_dim
            (Cairos.Series.make index (Nx.scalar Nx.float32 x))
      | I32 x ->
          rejected_as_zero_dim (Cairos.Series.make index (Nx.scalar Nx.int32 x))
      | I64 x ->
          rejected_as_zero_dim (Cairos.Series.make index (Nx.scalar Nx.int64 x)))

(* [Series.at] agrees bitwise with the values tensor at every in-range position,
   is [None] at every out-of-range one, and reading through it leaves the tensor
   untouched.

   The sweep runs [-length - 2 .. length + 2] exhaustively per draw rather than
   drawing one index, so each draw covers the three regions that behave
   differently underneath the accessor: in range; the wrap region [[-length,
   -1]], where the underlying [Nx.item] reads a real element Python-style and an
   unguarded [at] is a silent wrong answer; and below [-length], where it raises.
   The deterministic cases in test_series.ml pin those boundaries at one length;
   what random input adds is that the guard tracks the length rather than
   happening to agree with it, over values drawn from the full IEEE range
   including NaN. The sweep is read ascending, so an accessor with no lower
   bound at all is reported as the raise from below [-length] rather than as a
   wrap-region disagreement; the wrap region is what surfaces once the guard
   rejects the raising range but not the wrapping one.

   The empty-disagreement pass is not vacuous: the in-range arm demands [Some],
   so an accessor answering [None] everywhere is reported rather than silently
   satisfying an assertion of absence.

   Comparison is bitwise, not [=]: a NaN element must be reported as the same
   NaN rather than passing or failing on IEEE inequality, which is the whole
   content of the accessor being NaN-blind.

   The tensor-unchanged check runs *before* the value comparison, and both run
   against the same pre-read snapshot. [Series.values] hands back the underlying
   tensor by reference, so an accessor that wrote through it is the failure this
   half exists to catch; reported second it would be dead for any write the
   sweep reads back afterwards, described as a wrong value rather than as a
   mutation. *)
let series_at_agrees_with_values =
  QCheck.Test.make ~count:200 ~name:"series_at_agrees_with_values"
    Qcheck_gen.daily_float_series_arb (fun s ->
      let len = Cairos.Series.length s in
      let before = Nx.to_array (Cairos.Series.values s) in
      let sweep = List.init ((2 * len) + 5) (fun k -> k - len - 2) in
      let observed = List.map (fun i -> (i, Cairos.Series.at i s)) sweep in
      let after = Nx.to_array (Cairos.Series.values s) in
      let disagreement (i, got) =
        match (got, i >= 0 && i < len) with
        | Some v, true ->
            if Qcheck_gen.float_arrays_bitwise_equal [| v |] [| before.(i) |]
            then None
            else
              Some (Printf.sprintf "at %d = %h, tensor holds %h" i v before.(i))
        | Some v, false -> Some (Printf.sprintf "at %d = %h, expected None" i v)
        | None, true ->
            Some (Printf.sprintf "at %d = None, expected %h" i before.(i))
        | None, false -> None
      in
      if not (Qcheck_gen.float_arrays_bitwise_equal after before) then
        QCheck.Test.fail_reportf
          "length %d: reading through [at] changed the values tensor" len
      else
        match List.filter_map disagreement observed with
        | [] -> true
        | ds ->
            QCheck.Test.fail_reportf "length %d: %d position(s) wrong: %s" len
              (List.length ds) (String.concat "; " ds))

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
        series_at_agrees_with_values;
        prop_series_make_zero_dim_never_raises;
      ]
  in
  Alcotest.run "Series.props" [ ("property", tests) ]
