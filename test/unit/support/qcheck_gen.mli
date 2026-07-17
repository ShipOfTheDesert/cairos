(** Shared QCheck generators, shrinkers, and float comparators for Cairos
    property-test suites.

    Lives under [test/unit/support/] and is consumed by the per-module
    [test_*_props.ml] files in [test/unit/cairos/] and
    [test/unit/cairos_finance/]. [failwith] is used inside generators on
    unreachable paths so the QCheck shrinker reports the right counter-example;
    [Alcotest.fail] would corrupt shrinker output. *)

(** {1 Constants} *)

val epoch_2024_01_01_utc : float
(** [1_704_067_200.0] — POSIX seconds at 2024-01-01T00:00:00Z. The fixed origin
    every synthetic index extends from. *)

val default_seed : int
(** [0xC41A05]. The CI default seed; [pin_seed_from_env] uses this when
    [QCHECK_SEED] is unset. *)

(** {1 Index/series factories — total, generator-safe}

    Each factory builds a series from a known-valid epoch + length-matched
    [Nx.t], so the internal [Index.of_unix_floats] and [Series.make] calls
    cannot fail by construction. Unreachable [failwith] sites are commented at
    their definitions. *)

val make_series_from_floats :
  freq:'freq Cairos.Freq.t ->
  float array ->
  ('freq, (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t
(** [make_series_from_floats ~freq xs] builds a synthetic series at frequency
    [freq] starting at {!epoch_2024_01_01_utc}. Bucket interval is selected by
    locally-abstract [type freq] match on the [Freq.t] GADT (Minute=60s,
    Hour=3600s, Daily=86400s, Weekly=604800s). *)

(** {1 Single-series arbitraries} *)

val daily_float_series_arb :
  ([ `Daily ], (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t
  QCheck.arbitrary
(** Random length [1..64], values from [QCheck.float] (full IEEE 754 incl.
    NaN/inf — verified bit-transparent through Nx). Shrinker truncates to length
    1 / n/2 / n-1. *)

val daily_finite_float_series_arb :
  ([ `Daily ], (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t
  QCheck.arbitrary
(** As above but values restricted to finite floats in [-1e6, 1e6] (rejection of
    nan/±inf via [Gen.float_range]). For finance-metric and cumulative-return
    invariants where NaN cases are pinned by deterministic Alcotest tests, not
    properties. *)

val daily_returns_series_arb :
  ([ `Daily ], (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t
  QCheck.arbitrary
(** Length [2..64], values in [-0.5, 0.5]. For finance metrics where
    [(1 +. r) ** n] should not overflow within the bounded length and the
    constant-r invariant is testable to numerical tolerance. *)

val daily_non_negative_series_arb :
  ([ `Daily ], (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t
  QCheck.arbitrary
(** Values in [0.0, 1e6]. Used by Window's expanding-monotone-reducer property
    and as a baseline for non-negativity-dependent invariants. Lifted verbatim
    from [test_series_scan.ml]'s inline definition. *)

val daily_strict_positive_series_arb :
  ([ `Daily ], (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t
  QCheck.arbitrary
(** Values in [1e-6, 1e6]. Used where strict positivity is required (e.g.
    constructing wealth series for drawdown). *)

val minute_finite_float_series_arb :
  ([ `Minute ], (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t
  QCheck.arbitrary
(** Higher-frequency variant for [Resample] downsampling properties. Length
    [2..256] so minute→daily downsampling produces ≥1 daily bucket reliably. *)

val hourly_finite_float_series_arb :
  ([ `Hour ], (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t
  QCheck.arbitrary
(** Higher-frequency variant for [Resample] downsampling properties. Length
    [2..96]. *)

(** {1 Paired-series arbitraries} *)

val paired_aligned_daily_arb :
  (([ `Daily ], (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t
  * ([ `Daily ], (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t)
  QCheck.arbitrary
(** Pair of daily series sharing a structurally identical index (same length,
    same timestamp array). Used by the Align "structurally identical → Inner =
    Left = Asof" invariant and by Frame's "two columns share the reference
    index" tautology-free invariants. *)

val paired_overlapping_daily_arb :
  (([ `Daily ], (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t
  * ([ `Daily ], (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t)
  QCheck.arbitrary
(** Pair of daily series whose indices share a strictly non-empty intersection.
    Parameterised internally as [(len_a, len_b, start_b_offset)] with
    [len_a, len_b ∈ [1, 64]] and [start_b_offset ∈ [0, len_a - 1]]; the
    [start_b_offset < len_a] constraint pins overlap ≥ 1 by construction. The
    first series starts at the epoch; the second starts at
    [epoch + start_b_offset * 86_400.0]. Used by Align's Inner-length-bound and
    inner-is-subset-of-both invariants.

    A custom shrinker recovers the structural parameters from the output pair
    via [Cairos.Series.length] and the timestamps' day offset, then yields
    shrunken candidates that respect [start_b_offset < len_a] (clamps when
    shrinking [len_a]). Default integer shrinking on the underlying generators
    would not preserve the joint constraint. *)

(** {1 Frame arbitraries}

    Each frame arb builds a [[`Daily]] frame whose columns share an index
    derived from {!epoch_2024_01_01_utc}. Internal [Frame.of_series] failures on
    duplicate names or index mismatch are unreachable by construction (each
    column is built from {!make_series_from_floats} at the same length, with
    distinct programmatic names [c0..c{C-1}]) and terminate with [failwith]. *)

val daily_frame_distinct_floats_arb : [ `Daily ] Cairos.Frame.t QCheck.arbitrary
(** Daily frame with [1..20] rows and [1..10] columns of finite, row-wise
    distinct floats. Distinctness within each row is enforced by sorting that
    row's draws and bumping any colliding cell by [Float.succ] before shuffling,
    so the post-shuffle multiset is guaranteed strict-distinct. Used by the rank
    no-tie permutation/order-preservation properties. *)

val daily_frame_finite_floats_with_nan_arb :
  [ `Daily ] Cairos.Frame.t QCheck.arbitrary
(** Daily frame with [1..20] rows and [1..10] columns. Each cell is [Float.nan]
    with probability ~5% (one roll in twenty); otherwise drawn from
    [Gen.float_range (-10.0) 10.0]. The narrow bucket lets ties arise naturally
    — used by rank's tie-aware [N(N+1)/2] sum invariant where the property must
    hold across arbitrary tie patterns. *)

val daily_frame_zscore_well_conditioned_arb :
  [ `Daily ] Cairos.Frame.t QCheck.arbitrary
(** Daily frame with [1..20] rows and [2..10] columns. Every row contains at
    least two distinct non-NaN cells, guaranteeing non-zero sample variance (the
    well-conditioned constraint at the [zscore] mean/std invariants).
    Distinctness is enforced by drawing two anchor values per row, bumping the
    second by [Float.succ] if it collides, then placing them at two random
    column positions; remaining cells may be NaN or any finite value in
    [[-100, 100]]. *)

(** {1 Shrinkers}

    Exposed because per-test files may want to compose shrinking with a further
    constraint (e.g. truncating to length ≥ 2 for finance metrics). *)

val shrink_daily_series :
  ('freq, (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t ->
  ('freq, (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t QCheck.Iter.t
(** Prefix-truncation shrinker producing length 1 / n/2 / n-1 candidates. *)

(** {1 Comparators} *)

val float_approx_equal : tol:float -> float -> float -> bool
(** Three-branch NaN-aware tolerance comparator: branch on [Float.is_nan] for
    both operands first (both-NaN ⇒ equal, one-NaN ⇒ unequal), then
    short-circuit on exact equality so same-sign infinities compare equal, then
    check [|a-b| <= tol *. max 1.0 (max |a| |b|)]. Lifted verbatim from
    [test_series_scan.ml]; both NaN branches and the infinity branches are
    pinned by the existing comparator unit tests in that file. *)

val float_arrays_bitwise_equal : float array -> float array -> bool
(** Same length and [Int64.bits_of_float]-equal at every position. Used for
    exact-bit equality where two code paths must agree (e.g.
    [scan (+.) 0.0 = cumsum]). Lifted verbatim from [test_series_scan.ml]. *)

(** {1 Determinism} *)

val pin_seed_from_env : ?seed:int -> unit -> unit
(** Pin [QCheck_alcotest]'s lazy seed by setting [QCHECK_SEED] in the process
    environment via [Unix.putenv] when the env var is not already set. Call once
    at the top of [let () = Alcotest.run ...] in every property test file,
    before [QCheck_alcotest.to_alcotest] is invoked (the lazy is forced inside
    [to_alcotest]'s default [?rand] argument). When [QCHECK_SEED] is already
    set, the user-supplied value wins — locally a contributor reproduces a
    failing run via [QCHECK_SEED=12345 just test]; CI inherits the [?seed]
    fallback (defaults to {!default_seed}) automatically.

    [Random.init] is intentionally NOT used here: [QCheck_alcotest]'s lazy
    [seed_] calls [Random.self_init ()] when [QCHECK_SEED] is unset, drawing
    from system entropy and ignoring any prior [Random.init]. The putenv-based
    mechanism routes through the env-var path [QCheck_alcotest] actually reads.
*)
