(** Frequency downsampling for time series.

    [resample] converts a series from a higher frequency to a strictly lower
    frequency by grouping source timestamps into calendar-aligned buckets and
    aggregating each bucket to a single value. The output series carries the
    target frequency's phantom type.

    Only downsampling is supported: Minute -> Hour, Minute -> Daily, Minute ->
    Weekly, Minute -> Monthly, Hour -> Daily, Hour -> Weekly, Hour -> Monthly,
    Daily -> Weekly, Daily -> Monthly, Weekly -> Monthly. Same-frequency and
    upsampling attempts return [Error]. *)

(** {1 Errors}

    {!resample} returns a structured error so callers can pattern-match on the
    failure mode and recover the offending values without scanning error
    strings. *)

type err =
  | Target_not_lower of { source : Freq.any; target : Freq.any }
      (** [target] was not strictly lower than [source]. Equal frequencies are
          rejected alongside upsamples: the total order is Minute < Hour < Day <
          Week < Month, and only a strictly greater target rank is accepted.
          This is the only variant a caller can observe. *)
  | Unrepresentable_week_start of { timestamp : Ptime.t }
      (** Internal: the Monday opening the ISO week containing [timestamp] could
          not be computed. Structurally unreachable — subtracting a 0-6 day
          offset from a valid [Ptime.t] cannot underflow [Ptime.min] — and
          propagated rather than unwrapped so that no library function raises.
          Only a [Week] target can produce it. *)
  | Unrepresentable_bucket_timestamp of {
      year : int;
      month : int;
      day : int;
      hour : int;
    }
      (** Internal: the bucket boundary [year]-[month]-[day] at [hour]:00:00 UTC
          could not be reconstructed as a [Ptime.t]. Structurally unreachable —
          bucket keys are derived from valid [Ptime.t] values with only the
          finer components zeroed — and propagated for the same reason as
          [Unrepresentable_week_start]. *)

val err_to_string : err -> string
(** Render [err] as a human-readable one-line message. *)

val resample :
  agg:[ `First | `Last | `Sum | `Mean | `Min | `Max | `Count ] ->
  'target Freq.t ->
  ('src, (float, 'b) Nx.t) Series.t ->
  (('target, (float, Bigarray.float64_elt) Nx.t) Series.t, err) result
(** [resample ~agg target_freq series] groups [series] into buckets aligned to
    [target_freq]'s calendar boundaries, aggregates each bucket using [agg], and
    returns a new series at [target_freq].

    Bucket boundaries are calendar-aligned period starts:
    - Hour: start of the clock hour (minutes and seconds truncated to 0)
    - Daily: midnight UTC of the calendar day
    - Weekly: Monday 00:00 UTC of the ISO week
    - Monthly: the first day of the calendar month at 00:00 UTC, synthesised —
      the anchor need not appear in the source index

    Each non-empty bucket produces one output point whose timestamp is the
    bucket boundary. Empty buckets are omitted — the output length equals the
    number of non-empty buckets.

    [`Count] is the number of non-NaN observations in the bucket, delivered as
    an integral float because the output element type is always float64.
    Infinities count as observations, matching {!Series.dropna}. It reaches
    [0.0] only for a non-empty all-NaN bucket, since an empty bucket is omitted
    entirely.

    Returns [Error (Target_not_lower _)] when [target_freq] is not strictly
    lower than the source frequency, carrying both frequency witnesses. The
    frequency total order is: Minute < Hour < Daily < Weekly < Monthly. *)
