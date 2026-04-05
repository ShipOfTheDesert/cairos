(** Alignment of two same-frequency series on a shared index.

    The private {!aligned} type can only be constructed by {!align}, making
    misalignment unrepresentable at the call site. *)

type ('freq, 'a, 'b) aligned = private {
  index : 'freq Index.t;
  left : 'a;
  right : 'b;
}

val align :
  strategy:[ `Inner | `Left | `Asof of [ `Forward | `Backward ] ] ->
  ('freq, ('a, 'b) Nx.t) Series.t ->
  ('freq, (float, 'c) Nx.t) Series.t ->
  (('freq, ('a, 'b) Nx.t, (float, 'c) Nx.t) aligned, string) result
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

    Returns [Error msg] when the resulting index would be empty (Inner with
    disjoint series). Left and Asof always return [Ok] for non-empty left input.
*)

val map2 :
  (float -> float -> float) ->
  ('freq, (float, 'b) Nx.t, (float, 'c) Nx.t) aligned ->
  ('freq, (float, Bigarray.float64_elt) Nx.t) Series.t
(** [map2 f aligned] applies [f] element-wise over the left and right arrays,
    producing a new series with the aligned index. The function [f] receives
    corresponding elements from the left and right arrays. NaN values from fill
    operations propagate through [f] per IEEE 754. *)

val index : ('freq, 'a, 'b) aligned -> 'freq Index.t
(** The shared timestamp index. *)

val left : ('freq, 'a, 'b) aligned -> 'a
(** The left values array, reindexed to the shared axis. *)

val right : ('freq, 'a, 'b) aligned -> 'b
(** The right values array, reindexed to the shared axis. *)
