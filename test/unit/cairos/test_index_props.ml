(* Property suite for [Cairos.Index]. RFC 0046 TP-Index-1..3.

   Each property runs at [~count:200] per RFC 0046 Test Plan; CI/local
   reproducibility is provided by [Qcheck_gen.pin_seed_from_env] (FR-4). *)

(* TP-Index-1 — round-trip a synthetic daily index through [Ptime.to_float_s]
   and recover the original POSIX seconds within relative tol [1e-15]. At
   the 2024-era POSIX scale (~1.7e9), this translates to ~1.7 microseconds
   absolute — well above [Ptime]'s picosecond precision but tight enough to
   catch a sub-millisecond epoch drift. The generator constructs timestamps
   as [epoch + i * 86_400], so the expected float at position i is
   recomputed from that contract. Catches a regression that drops
   sub-second precision or shifts the epoch interpretation. *)
let index_round_trip_via_timestamps =
  QCheck.Test.make ~count:200 ~name:"index_round_trip_via_timestamps"
    Qcheck_gen.daily_finite_float_series_arb (fun s ->
      let idx = Cairos.Series.index s in
      let ts = Cairos.Index.timestamps idx in
      let n = Array.length ts in
      let expected =
        Array.init n (fun i ->
            Qcheck_gen.epoch_2024_01_01_utc +. (float_of_int i *. 86_400.0))
      in
      let actual = Array.map Ptime.to_float_s ts in
      Array.length expected = Array.length actual
      && Array.for_all2
           (fun e a -> Qcheck_gen.float_approx_equal ~tol:1e-15 e a)
           expected actual)

(* TP-Index-2 — [Index.length] tracks the underlying timestamp array. Catches
   a regression where the length accessor decouples from the array. *)
let index_length_equals_timestamp_array_length =
  QCheck.Test.make ~count:200 ~name:"index_length_equals_timestamp_array_length"
    Qcheck_gen.daily_float_series_arb (fun s ->
      let idx = Cairos.Series.index s in
      Cairos.Index.length idx = Array.length (Cairos.Index.timestamps idx))

(* TP-Index-3 — [of_unix_floats] rejects non-monotonic input. Generator picks
   a length [n ∈ [3, 10]] and two indices [i, j ∈ [0, n-1]], builds a
   strictly-increasing base array [epoch + k * 86_400] and swaps positions
   [i] and [j]. Distinct base values guarantee that any [i ≠ j] swap breaks
   monotonicity; [QCheck.assume (i <> j)] discards the no-op case.

   Custom shrinker preserves the joint constraint [i, j ∈ [0, n-1]]: the
   default integer shrinker would shrink each component independently, so
   reducing [n] without clamping [i] / [j] could yield an out-of-bounds
   tuple that crashes the array swap. The shrinker yields candidates that
   reduce [n] (clamping [i] / [j]), reduce [i] toward [0], and reduce [j]
   toward [0], in that order. [i = j] candidates are not filtered — the
   property's [QCheck.assume (i <> j)] discards them, and skipping in the
   shrinker would hide a path the search would otherwise explore. *)
let shrink_swap (n, i, j) =
  let open QCheck.Iter in
  (if n > 3 then
     let new_n = n - 1 in
     return (new_n, min i (new_n - 1), min j (new_n - 1))
   else empty)
  <+> (if i > 0 then return (n, i - 1, j) else empty)
  <+> if j > 0 then return (n, i, j - 1) else empty

let non_monotonic_swap_arb =
  let open QCheck in
  let gen =
    let open Gen in
    let* n = int_range 3 10 in
    let* i = int_range 0 (n - 1) in
    let* j = int_range 0 (n - 1) in
    return (n, i, j)
  in
  make
    ~print:(fun (n, i, j) -> Printf.sprintf "(n=%d, swap %d<->%d)" n i j)
    ~shrink:shrink_swap gen

let of_unix_floats_rejects_non_monotonic =
  QCheck.Test.make ~count:200 ~name:"of_unix_floats_rejects_non_monotonic"
    non_monotonic_swap_arb (fun (n, i, j) ->
      QCheck.assume (i <> j);
      let arr =
        Array.init n (fun k ->
            Qcheck_gen.epoch_2024_01_01_utc +. (float_of_int k *. 86_400.0))
      in
      let tmp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- tmp;
      match Cairos.Index.of_unix_floats Cairos.Freq.Day arr with
      | Error (Cairos.Index.Non_monotonic _) -> true
      | Ok _ -> false
      | Error
          ( Cairos.Index.Invalid_timestamp _
          | Cairos.Index.Invalid_unix_timestamp _ ) ->
          false)

let () =
  Qcheck_gen.pin_seed_from_env ();
  let tests =
    List.map QCheck_alcotest.to_alcotest
      [
        index_round_trip_via_timestamps;
        index_length_equals_timestamp_array_length;
        of_unix_floats_rejects_non_monotonic;
      ]
  in
  Alcotest.run "Index.props" [ ("property", tests) ]
