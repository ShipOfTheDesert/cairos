(** Indexed time series over Nx tensors.

    A [Series.t] pairs a frequency-tagged {!Index.t} with an {!Nx.t} tensor of
    values. The frequency phantom type is preserved across all operations. *)

type ('freq, 'v) t

val make :
  'freq Index.t -> ('v, 'b) Nx.t -> (('freq, ('v, 'b) Nx.t) t, string) result
(** Construct a series from index and values. Returns [Error msg] if
    [Index.length index <> (Nx.shape values).(0)]. *)

(** {1 Accessors} *)

val index : ('freq, 'v) t -> 'freq Index.t
(** The timestamp index. *)

val values : ('freq, 'v) t -> 'v
(** The values tensor. Returns a direct reference — mutations to the returned
    tensor affect the series. This is intentional: Nx tensors may be large and
    copying on every access would be prohibitively expensive. Contrast with
    {!Index.timestamps}, which returns a defensive copy of the (small) timestamp
    array. *)

val length : ('freq, 'v) t -> int
(** Number of elements in the series. *)

(** {1 Slicing} *)

val slice :
  start:int -> stop:int -> ('freq, ('v, 'b) Nx.t) t -> ('freq, ('v, 'b) Nx.t) t
(** [slice ~start ~stop s] returns sub-series for half-open range
    [\[start, stop)]. Bounds are clamped to [[0, length s]]. *)

(** {1 Transformation} *)

val map : ('v -> 'w) -> ('freq, 'v) t -> ('freq, 'w) t
(** [map f s] applies [f] to the values, preserving the index. The frequency
    phantom type is preserved. *)
