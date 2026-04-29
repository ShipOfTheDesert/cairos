(* Constants *)

let epoch_2024_01_01_utc = 1_704_067_200.0
let default_seed = 0xC41A05

(* Index/series factories — total, generator-safe.

   Locally-abstract [type freq] match on the [Freq.t] GADT per
   ~/.claude/solutions/ocaml/gadt-exhaustiveness-locally-abstract-type.md. *)
let make_series_from_floats : type freq.
    freq:freq Cairos.Freq.t ->
    float array ->
    (freq, (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t =
 fun ~freq xs ->
  let bucket_seconds =
    match freq with
    | Cairos.Freq.Minute -> 60.0
    | Cairos.Freq.Hour -> 3_600.0
    | Cairos.Freq.Day -> 86_400.0
    | Cairos.Freq.Week -> 604_800.0
  in
  let n = Array.length xs in
  let ts =
    Array.init n (fun i ->
        epoch_2024_01_01_utc +. (float_of_int i *. bucket_seconds))
  in
  (* Unreachable: synthetic strictly-increasing finite POSIX seconds. RFC 0030
     §R4 — generators must terminate unreachable [result] branches with
     [failwith], not propagate [result], or QCheck's shrinker mis-reports. *)
  let idx =
    match Cairos.Index.of_unix_floats freq ts with
    | Ok i -> i
    | Error e ->
        failwith ("qcheck_gen: of_unix_floats: " ^ Cairos.Index.err_to_string e)
  in
  let values = Nx.create Nx.float64 [| n |] xs in
  (* Unreachable: index length matches values length by construction. RFC 0030
     §R4 — see comment above. *)
  match Cairos.Series.make idx values with
  | Ok s -> s
  | Error e -> failwith ("qcheck_gen: Series.make: " ^ e)

(* Build a daily series whose first timestamp is at
   [epoch_2024_01_01_utc + start_day * 86_400.0]. Used by
   [paired_overlapping_daily_arb] (and its shrinker) to position the second
   series at an arbitrary offset. Same unreachable-result discipline as
   [make_series_from_floats] (RFC 0030 §R4). *)
let make_offset_daily_series ~start_day xs =
  let n = Array.length xs in
  let ts =
    Array.init n (fun i ->
        epoch_2024_01_01_utc +. (float_of_int (start_day + i) *. 86_400.0))
  in
  let idx =
    match Cairos.Index.of_unix_floats Cairos.Freq.Day ts with
    | Ok i -> i
    | Error e -> failwith ("qcheck_gen.offset: " ^ Cairos.Index.err_to_string e)
  in
  let values = Nx.create Nx.float64 [| n |] xs in
  match Cairos.Series.make idx values with
  | Ok s -> s
  | Error e -> failwith ("qcheck_gen.offset: " ^ e)

(* Recover the [start_day] offset of a daily series whose timestamps were
   built via [epoch + k * 86_400] for some [k]. Used by the
   [paired_overlapping_daily_arb] shrinker to recover the structural
   [start_b_offset] parameter from the output pair. Returns [0] for an
   empty series (the arb never produces empty inputs, but guard the
   division-by-undefined for completeness). *)
let recover_start_day s =
  let ts = Cairos.Index.timestamps (Cairos.Series.index s) in
  if Array.length ts = 0 then 0
  else
    let t0 = Ptime.to_float_s ts.(0) in
    int_of_float ((t0 -. epoch_2024_01_01_utc) /. 86_400.0)

(* Single-series arbitraries *)

(* Prefix-truncation shrinker producing length 1 / n/2 / n-1 candidates.

   The locally-abstract [type freq] keeps the shrinker polymorphic so the same
   helper can shrink daily, minute, hourly, and weekly series without forcing
   the input freq witness to be erased. *)
let shrink_daily_series : type freq.
    (freq, (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t ->
    (freq, (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t QCheck.Iter.t =
 fun s ->
  let arr = Nx.to_array (Cairos.Series.values s) in
  let freq = Cairos.Index.freq (Cairos.Series.index s) in
  let n = Array.length arr in
  let open QCheck.Iter in
  if n <= 1 then empty
  else
    let candidates =
      List.sort_uniq compare [ 1; n / 2; n - 1 ]
      |> List.filter (fun k -> k >= 1 && k < n)
    in
    of_list candidates >|= fun k ->
    make_series_from_floats ~freq (Array.sub arr 0 k)

let daily_float_series_arb =
  let open QCheck in
  let gen =
    Gen.map
      (make_series_from_floats ~freq:Cairos.Freq.Day)
      (Gen.array_size (Gen.int_range 1 64) Gen.float)
  in
  make ~shrink:shrink_daily_series
    ~print:(fun s ->
      Printf.sprintf "<daily series len=%d>" (Cairos.Series.length s))
    gen

let daily_finite_float_series_arb =
  let open QCheck in
  let gen =
    Gen.map
      (make_series_from_floats ~freq:Cairos.Freq.Day)
      (Gen.array_size (Gen.int_range 1 64) (Gen.float_range (-1e6) 1e6))
  in
  make ~shrink:shrink_daily_series
    ~print:(fun s ->
      Printf.sprintf "<daily finite series len=%d>" (Cairos.Series.length s))
    gen

let daily_returns_series_arb =
  let open QCheck in
  let gen =
    Gen.map
      (make_series_from_floats ~freq:Cairos.Freq.Day)
      (Gen.array_size (Gen.int_range 2 64) (Gen.float_range (-0.5) 0.5))
  in
  make ~shrink:shrink_daily_series
    ~print:(fun s ->
      Printf.sprintf "<daily returns series len=%d>" (Cairos.Series.length s))
    gen

let daily_non_negative_series_arb =
  let open QCheck in
  let gen =
    Gen.map
      (make_series_from_floats ~freq:Cairos.Freq.Day)
      (Gen.array_size (Gen.int_range 1 64) (Gen.float_bound_inclusive 1e6))
  in
  make ~shrink:shrink_daily_series
    ~print:(fun s ->
      Printf.sprintf "<daily non-neg series len=%d>" (Cairos.Series.length s))
    gen

let daily_strict_positive_series_arb =
  let open QCheck in
  let gen =
    Gen.map
      (make_series_from_floats ~freq:Cairos.Freq.Day)
      (Gen.array_size (Gen.int_range 1 64) (Gen.float_range 1e-6 1e6))
  in
  make ~shrink:shrink_daily_series
    ~print:(fun s ->
      Printf.sprintf "<daily pos series len=%d>" (Cairos.Series.length s))
    gen

let minute_finite_float_series_arb =
  let open QCheck in
  let gen =
    Gen.map
      (make_series_from_floats ~freq:Cairos.Freq.Minute)
      (Gen.array_size (Gen.int_range 2 256) (Gen.float_range (-1e6) 1e6))
  in
  make
    ~print:(fun s ->
      Printf.sprintf "<minute finite series len=%d>" (Cairos.Series.length s))
    gen

let hourly_finite_float_series_arb =
  let open QCheck in
  let gen =
    Gen.map
      (make_series_from_floats ~freq:Cairos.Freq.Hour)
      (Gen.array_size (Gen.int_range 2 96) (Gen.float_range (-1e6) 1e6))
  in
  make
    ~print:(fun s ->
      Printf.sprintf "<hourly finite series len=%d>" (Cairos.Series.length s))
    gen

(* Paired-series arbitraries *)

let paired_aligned_daily_arb =
  let open QCheck in
  let gen =
    let open Gen in
    let* n = int_range 1 64 in
    let* xs_a = array_size (return n) (float_range (-1e6) 1e6) in
    let* xs_b = array_size (return n) (float_range (-1e6) 1e6) in
    return
      ( make_series_from_floats ~freq:Cairos.Freq.Day xs_a,
        make_series_from_floats ~freq:Cairos.Freq.Day xs_b )
  in
  make
    ~print:(fun (a, b) ->
      Printf.sprintf "<aligned daily pair len_a=%d len_b=%d>"
        (Cairos.Series.length a) (Cairos.Series.length b))
    gen

(* Pair of daily series sharing a strictly non-empty intersection.

   Re-parameterised from the original [(total, overlap, len_a_extra,
   len_b_extra)] four-tuple to [(len_a, len_b, start_b_offset)] with
   [start_b_offset ∈ [0, len_a - 1]]. The latter constraint guarantees the
   second series begins on a day inside the first's range, which pins
   overlap ≥ 1 by construction without any [total]-level constraint. The
   simpler parameterisation removes the latent default-shrinker hazard
   flagged in the original RFC §R2: the four-tuple's joint constraint
   (overlap ≤ total ∧ len_x_extra ≤ total - overlap) is not preserved by
   independent integer shrinks, while the three-tuple's only constraint is
   [start_b_offset < len_a], which the custom shrinker below preserves
   explicitly (clamps [start_b_offset] when shrinking [len_a]).

   The shrinker recovers [(len_a, len_b, start_b_offset, xs_a, xs_b)] from
   the output pair via [Cairos.Series.length] / [Cairos.Series.values] and
   the [recover_start_day] helper, then yields candidates that:

   - reduce [len_a] by one, clamping [start_b_offset] to [≤ len_a - 2]
   - reduce [len_b] by one
   - reduce [start_b_offset] by one (towards 0)

   Truncation reuses the leading prefix of [xs_a] / [xs_b], so values are
   not shrunk independently — the structural shape is what the consuming
   properties (TP-Align-1, 2, 4 + TP-Frame-2, 3) actually depend on. *)
let paired_overlapping_daily_arb =
  let open QCheck in
  let gen =
    let open Gen in
    let* len_a = int_range 1 64 in
    let* len_b = int_range 1 64 in
    let* start_b_offset = int_range 0 (len_a - 1) in
    let* xs_a = array_size (return len_a) (float_range (-1e6) 1e6) in
    let* xs_b = array_size (return len_b) (float_range (-1e6) 1e6) in
    return
      ( make_series_from_floats ~freq:Cairos.Freq.Day xs_a,
        make_offset_daily_series ~start_day:start_b_offset xs_b )
  in
  let shrink (a, b) =
    let open QCheck.Iter in
    let xs_a = Nx.to_array (Cairos.Series.values a) in
    let xs_b = Nx.to_array (Cairos.Series.values b) in
    let len_a = Array.length xs_a in
    let len_b = Array.length xs_b in
    let start_b_offset = recover_start_day b in
    let rebuild la lb sb =
      let xa = Array.sub xs_a 0 la in
      let xb = Array.sub xs_b 0 lb in
      ( make_series_from_floats ~freq:Cairos.Freq.Day xa,
        make_offset_daily_series ~start_day:sb xb )
    in
    (if len_a > 1 then
       let new_la = len_a - 1 in
       let new_sb = min start_b_offset (new_la - 1) in
       return (rebuild new_la len_b new_sb)
     else empty)
    <+> (if len_b > 1 then return (rebuild len_a (len_b - 1) start_b_offset)
         else empty)
    <+>
    if start_b_offset > 0 then return (rebuild len_a len_b (start_b_offset - 1))
    else empty
  in
  make ~shrink
    ~print:(fun (a, b) ->
      Printf.sprintf "<overlapping daily pair len_a=%d len_b=%d start_b=%d>"
        (Cairos.Series.length a) (Cairos.Series.length b) (recover_start_day b))
    gen

(* Comparators — lifted verbatim from test/unit/cairos/test_series_scan.ml *)

(* Three-branch NaN-aware tolerance comparator per
   ~/.claude/solutions/general/nan-aware-tolerance-comparator.md:
   branch on is_nan for both operands before subtraction.

   Infinity handling: after the nan branches, short-circuit on [a = b] so
   same-sign infinities compare equal (otherwise [inf -. inf = nan] and the
   relative-tolerance branch wrongly rejects them). If only one side is still
   infinite — or signs differ — return false explicitly instead of relying
   on the arithmetic, because [+inf <= +inf] is [true] in OCaml and would
   otherwise let [|+inf - (-inf)| <= tol *. +inf] slip through. *)
let float_approx_equal ~tol a b =
  match (Float.is_nan a, Float.is_nan b) with
  | true, true -> true
  | true, false
  | false, true ->
      false
  | false, false ->
      if a = b then true
      else if (not (Float.is_finite a)) || not (Float.is_finite b) then false
      else
        let diff = Float.abs (a -. b) in
        let scale = Float.max 1.0 (Float.max (Float.abs a) (Float.abs b)) in
        diff <= tol *. scale

(* OCaml's generic (=) returns false for nan = nan (IEEE 754). We need
   bitwise equality: two NaN values from the same code path are equal. *)
let float_bitwise_equal a b =
  Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b)

let float_arrays_bitwise_equal a b =
  Array.length a = Array.length b && Array.for_all2 float_bitwise_equal a b

(* Determinism

   [QCheck_alcotest] reads [QCHECK_SEED] from the process environment when
   forcing its lazy seed; if unset, it calls [Random.self_init ()] which
   draws a fresh seed from system entropy and ignores any prior
   [Random.init]. To pin the runner deterministically when no override is
   provided, we set [QCHECK_SEED] in the process env via [Unix.putenv]
   before [QCheck_alcotest.to_alcotest] is called for the first test (the
   lazy is forced inside [to_alcotest]'s default [?rand] argument). If the
   user has already set [QCHECK_SEED], honour their override. *)

let pin_seed_from_env ?(seed = default_seed) () =
  match Sys.getenv_opt "QCHECK_SEED" with
  | Some _ -> ()
  | None -> Unix.putenv "QCHECK_SEED" (string_of_int seed)
