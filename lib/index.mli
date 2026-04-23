(** Frequency-tagged timestamp index.

    An [Index.t] carries a {!Freq.t} witness at the type level and wraps an
    array of {!Ptime.t} timestamps. Smart constructors parse ISO 8601 strings;
    {!of_unix_floats} accepts raw POSIX seconds. *)

type 'freq t

(** {1 Errors}

    Smart constructors return a structured error so callers can pattern-match on
    the failure mode and recover the 0-indexed array position without scanning
    error strings. *)

type err =
  | Invalid_timestamp of { position : int; raw : string }
      (** [raw] at [position] did not parse as RFC 3339. *)
  | Non_monotonic of { position : int }
      (** Element at [position] was not strictly greater than the previous. *)
  | Invalid_unix_timestamp of { position : int; raw : float }
      (** [raw] at [position] was not a valid POSIX timestamp (NaN, infinity,
          out of range). *)

val err_to_string : err -> string
(** Render [err] as a human-readable one-line message. *)

(** {1 String smart constructors}

    Parse ISO 8601 timestamps via {!Ptime.of_rfc3339}. Date-only strings (e.g.
    ["2024-01-15"]) are accepted and interpreted as midnight UTC. Timestamps
    must be strictly monotonically increasing. Return [Error] on any malformed
    string or non-monotonic ordering, with the failing element's position. *)

val daily : string array -> ([ `Daily ] t, err) result
(** Construct a daily-frequency index. *)

val minute : string array -> ([ `Minute ] t, err) result
(** Construct a minute-frequency index. *)

val hourly : string array -> ([ `Hour ] t, err) result
(** Construct an hourly-frequency index. *)

val weekly : string array -> ([ `Weekly ] t, err) result
(** Construct a weekly-frequency index. *)

(** {1 Float constructor}

    Construct from Unix float timestamps. Timestamps must be strictly
    monotonically increasing. Returns [Error] if any float is not a valid POSIX
    timestamp (NaN, infinity, out of range) or if timestamps are not monotonic.
*)

val of_unix_floats : 'freq Freq.t -> float array -> ('freq t, err) result
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
