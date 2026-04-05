(** Frequency-tagged timestamp index.

    An [Index.t] carries a {!Freq.t} witness at the type level and wraps an
    array of {!Ptime.t} timestamps. Smart constructors parse ISO 8601 strings;
    {!of_unix_floats} accepts raw POSIX seconds. *)

type 'freq t

(** {1 String smart constructors}

    Parse ISO 8601 timestamps via {!Ptime.of_rfc3339}. Date-only strings (e.g.
    ["2024-01-15"]) are accepted and interpreted as midnight UTC. Timestamps
    must be strictly monotonically increasing. Return [Error msg] on any
    malformed string or non-monotonic ordering, where [msg] identifies the
    failing element and its position. *)

val daily : string array -> ([ `Daily ] t, string) result
(** Construct a daily-frequency index. *)

val minute : string array -> ([ `Minute ] t, string) result
(** Construct a minute-frequency index. *)

val hourly : string array -> ([ `Hour ] t, string) result
(** Construct an hourly-frequency index. *)

val weekly : string array -> ([ `Weekly ] t, string) result
(** Construct a weekly-frequency index. *)

(** {1 Float constructor}

    Construct from Unix float timestamps. Timestamps must be strictly
    monotonically increasing. Returns [Error msg] if any float is not a valid
    POSIX timestamp (NaN, infinity, out of range) or if timestamps are not
    monotonic, where [msg] identifies the failing element and its position. *)

val of_unix_floats : 'freq Freq.t -> float array -> ('freq t, string) result
(** [of_unix_floats freq floats] constructs an index from POSIX seconds. *)

(** {1 Accessors} *)

val length : 'freq t -> int
(** Number of timestamps in the index. *)

val freq : 'freq t -> 'freq Freq.t
(** Frequency witness carried by this index. *)

val timestamps : 'freq t -> Ptime.t array
(** Raw timestamp array. Returned by copy — mutations do not affect the index.
*)

(** {1 Internal constructors}

    These are library-internal. Do not use outside the [cairos] library. *)

val of_ptime_array_unsafe : 'freq Freq.t -> Ptime.t array -> 'freq t
(** [of_ptime_array_unsafe freq timestamps] constructs an index without
    validating monotonicity. Only safe when timestamps are extracted from valid
    {!t} values and the extraction preserves sorted order. *)

(** {1 Slicing} *)

val slice : start:int -> stop:int -> 'freq t -> 'freq t
(** [slice ~start ~stop t] returns timestamps in half-open range
    [\[start, stop)]. Both [start] and [stop] are clamped to [[0, length t]]. *)
