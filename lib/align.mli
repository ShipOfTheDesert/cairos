(** Alignment of two same-frequency series on a shared index.

    The abstract {!aligned} type can only be constructed by {!align} and can
    only be inspected through the accessor functions below. *)

type ('freq, 'a, 'b) aligned
(** A frequency-tagged pair of reindexed value arrays produced by {!align}.

    Abstract: values of this type are constructed only by {!align} and inspected
    only through {!index}, {!left}, {!right}, {!map2}, and {!map2_nan}. External
    callers cannot pattern-match or project fields — misaligned binary
    operations remain unrepresentable by construction. *)

(** {1 Errors}

    {!align} returns a structured error so callers can pattern-match on the
    failure mode and recover the offending lengths without scanning error
    strings. *)

type err =
  | Empty_index of { left_length : int; right_length : int }
      (** The aligned index would be empty. Only [`Inner] can produce this: the
          left series had [left_length] timestamps, the right had
          [right_length], and the two share none. *)

val err_to_string : err -> string
(** Render [err] as a human-readable one-line message. *)

val align :
  strategy:[ `Inner | `Left | `Asof of [ `Forward | `Backward ] ] ->
  ('freq, ('a, 'b) Nx.t) Series.t ->
  ('freq, (float, 'c) Nx.t) Series.t ->
  (('freq, ('a, 'b) Nx.t, (float, 'c) Nx.t) aligned, err) result
(** [align ~strategy left right] pairs two same-frequency series on a shared
    index according to [strategy].

    - [`Inner] — index is the intersection of both indexes. Returns [Error] if
      the intersection is empty.
    - [`Left] — index matches the left series. Right values at timestamps not
      present in the right series are filled with [Float.nan].
    - [`Asof `Backward] — for each left timestamp, matches the nearest right
      timestamp at or before it. Unmatched positions are [Float.nan].
    - [`Asof `Forward] — for each left timestamp, matches the nearest right
      timestamp at or after it. Unmatched positions are [Float.nan].

    Returns [Error (Empty_index _)] when the resulting index would be empty
    (Inner with disjoint series). Left and Asof always return [Ok] for non-empty
    left input. *)

val map2 :
  (float -> float -> float) ->
  ('freq, (float, 'b) Nx.t, (float, 'c) Nx.t) aligned ->
  ('freq, (float, Bigarray.float64_elt) Nx.t) Series.t
(** [map2 f aligned] applies [f] element-wise over the left and right arrays,
    producing a new series with the aligned index. The function [f] receives
    corresponding elements from the left and right arrays.

    NaN inputs are handed to [f] unchanged, so what happens to them is [f]'s
    behaviour, not this function's. Arithmetic propagates them per IEEE 754, but
    a comparison on NaN is [false] — so a predicate such as
    [fun a b -> if a > b then 1.0 else 0.0] takes its [else] branch and emits a
    definite value where the inputs were undefined. Use {!map2_nan} when [f] is
    a predicate and NaN should propagate. *)

val map2_nan :
  ('freq, (float, 'b) Nx.t, (float, 'c) Nx.t) aligned ->
  f:(float -> float -> float) ->
  ('freq, (float, Bigarray.float64_elt) Nx.t) Series.t
(** [map2_nan aligned ~f] applies [f] element-wise over the left and right
    arrays, producing a new series with the aligned index, and yields
    [Float.nan] at every position where either input is NaN. [f] is applied only
    to pairs in which neither element is NaN.

    Gating is on the {e inputs} only. An [f] that itself returns [Float.nan]
    from a NaN-free pair has that NaN passed through to the output — this
    function does not inspect [f]'s result, so a NaN in the output does not by
    itself indicate a gated position.

    Use this over {!map2} when [f] is a predicate.
    [fun fast slow -> if fast > slow then 1.0 else 0.0] over a rolling warmup
    reads as [0.0] under {!map2} — a confident "flat" — where the honest answer
    is undefined.

    Note the argument order differs from {!map2}, which takes [f] first and
    positionally. The two are otherwise substitutable at a call site, so
    migrating between them requires reordering. *)

val index : ('freq, 'a, 'b) aligned -> 'freq Index.t
(** The shared timestamp index. *)

val left : ('freq, 'a, 'b) aligned -> 'a
(** The left values array, reindexed to the shared axis. *)

val right : ('freq, 'a, 'b) aligned -> 'b
(** The right values array, reindexed to the shared axis. *)
