(** Frequency downsampling for time series.

    [resample] converts a series from a higher frequency to a strictly lower
    frequency by grouping source timestamps into calendar-aligned buckets and
    aggregating each bucket to a single value. The output series carries the
    target frequency's phantom type.

    Only downsampling is supported: Minute -> Hour, Minute -> Daily, Minute ->
    Weekly, Hour -> Daily, Hour -> Weekly, Daily -> Weekly. Same-frequency and
    upsampling attempts return [Error]. *)

val resample :
  agg:[ `First | `Last | `Sum | `Mean | `Min | `Max ] ->
  'target Freq.t ->
  ('src, (float, 'b) Nx.t) Series.t ->
  (('target, (float, Bigarray.float64_elt) Nx.t) Series.t, string) result
(** [resample ~agg target_freq series] groups [series] into buckets aligned to
    [target_freq]'s calendar boundaries, aggregates each bucket using [agg], and
    returns a new series at [target_freq].

    Bucket boundaries are calendar-aligned period starts:
    - Hour: start of the clock hour (minutes and seconds truncated to 0)
    - Daily: midnight UTC of the calendar day
    - Weekly: Monday 00:00 UTC of the ISO week

    Each non-empty bucket produces one output point whose timestamp is the
    bucket boundary. Empty buckets are omitted — the output length equals the
    number of non-empty buckets.

    Returns [Error msg] when [target_freq] is not strictly lower than the source
    frequency. The frequency total order is: Minute < Hour < Daily < Weekly. *)
