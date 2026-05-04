(** Shared fixture construction for the bench harnesses.

    The 2024-01-01 UTC epoch and the daily-step formula are pinned here so a
    future calendar-axis change is one-site rather than seven-site.

    Failures surface via [failwith] — bench harness convention per
    CONTRIBUTING.md §VII. The library-shaped {!Bench_emit} module follows the
    result-everywhere rule; these helpers do not. *)

val epoch : float
(** [1_704_067_200.0] — 2024-01-01 UTC, the bench's pinned start instant. *)

val make_index : ?start:int -> length:int -> unit -> [ `Daily ] Cairos.Index.t
(** [make_index ?start ~length ()] builds a [length]-bar daily index whose first
    timestamp is [epoch + start * 86_400] seconds. Default [start] is [0]. The
    trailing [unit] is required for the optional argument to be erasable. *)

val make_values : length:int -> (float, Nx.float64_elt) Nx.t
(** [make_values ~length] builds a deterministic float64 tensor of length
    [length] using the formula [((i * 7) + 13) mod 1000 / 100], producing
    visible variance over a sliding window in [[0, 9.99]].

    Used by benches whose hot loop is not specialised on a narrower value range.
    The cumprod-safe [[0.99, 1.01]] generator and the NaN-injecting
    [bench_dropna] generator stay inline at their sites (single-occurrence
    shapes, under the abstraction threshold). *)

val make_series :
  'freq Cairos.Index.t ->
  (float, Nx.float64_elt) Nx.t ->
  ('freq, (float, Nx.float64_elt) Nx.t) Cairos.Series.t
(** [make_series idx vals] constructs a series, raising [Failure] on
    [Cairos.Series.make] error. *)

val make_frame : bars:int -> columns:int -> [ `Daily ] Cairos.Frame.t
(** [make_frame ~bars ~columns] builds a deterministic [`Daily]-tagged frame
    with [bars] rows and [columns] named columns ([c0], [c1], …,
    [c{columns-1}]). All cells are finite floats — no NaN, since the bench
    measures the happy path of the cross-sectional kernels.

    Raises [Failure] if [columns < 1] or if [Cairos.Frame.of_series] rejects the
    construction (would only happen on an internal mismatch — bench-level
    convention per CONTRIBUTING.md §VII). *)
