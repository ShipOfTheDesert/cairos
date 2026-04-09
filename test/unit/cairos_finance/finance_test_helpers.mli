(** Test-only helpers for [cairos_finance]: synthetic indices and series
    builders.

    These helpers fabricate well-formed indices at each frequency without going
    through Ptime. Indices start at 2020-01-01T00:00:00Z (POSIX seconds
    1577836800) and step by the natural interval for their frequency, going
    through {!Cairos.Index.of_unix_floats}. Errors from the underlying smart
    constructors fail the current test via [Alcotest.fail]. *)

val daily_index_of_length : int -> [ `Daily ] Cairos.Index.t
val hourly_index_of_length : int -> [ `Hour ] Cairos.Index.t
val minute_index_of_length : int -> [ `Minute ] Cairos.Index.t
val weekly_index_of_length : int -> [ `Weekly ] Cairos.Index.t

val make_series :
  'freq Cairos.Index.t ->
  float array ->
  ('freq, (float, Nx.float64_elt) Nx.t) Cairos.Series.t
(** [make_series idx values] wraps [values] into an [Nx.float64] tensor and
    pairs it with [idx] via {!Cairos.Series.make}. Fails the test on length
    mismatch. *)

val make_daily_series :
  float array -> ([ `Daily ], (float, Nx.float64_elt) Nx.t) Cairos.Series.t
(** [make_daily_series values] is shorthand for
    [make_series (daily_index_of_length (Array.length values)) values]. *)
