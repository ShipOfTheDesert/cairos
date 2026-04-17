(** Indexed time series over Nx tensors.

    A [Series.t] pairs a frequency-tagged {!Index.t} with an {!Nx.t} tensor of
    values. The frequency phantom type is preserved across all operations. *)

type ('freq, 'v) t

val make :
  'freq Index.t -> ('v, 'b) Nx.t -> (('freq, ('v, 'b) Nx.t) t, string) result
(** Construct a series from index and values. Returns [Error msg] if
    [Index.length index <> (Nx.shape values).(0)]. *)

(** {1 Internal constructors}

    These are library-internal. Do not use outside the [cairos] library. *)

val make_unsafe : 'freq Index.t -> ('v, 'b) Nx.t -> ('freq, ('v, 'b) Nx.t) t
(** [make_unsafe index values] constructs a series without checking length
    equality. Only safe when the length invariant is guaranteed by the call site
    (e.g. {!Align.map2} where aligned arrays match by construction). *)

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

val head : int -> ('freq, ('v, 'b) Nx.t) t -> ('freq, ('v, 'b) Nx.t) t
(** [head n s] returns the first [n] entries of [s]. Clamps to [length s] if [n]
    exceeds length. Returns an empty series when [n <= 0]. *)

val tail : int -> ('freq, ('v, 'b) Nx.t) t -> ('freq, ('v, 'b) Nx.t) t
(** [tail n s] returns the last [n] entries of [s]. Clamps to [length s] if [n]
    exceeds length. Returns an empty series when [n <= 0]. *)

(** {1 Transformation} *)

val map : ('v -> 'w) -> ('freq, 'v) t -> ('freq, 'w) t
(** [map f s] applies [f] to the values, preserving the index. The frequency
    phantom type is preserved. *)

(** {1 Derived operations}

    Total, length-preserving operations on float series. All produce a new
    series with the same index. Vacated or undefined positions are filled with
    [Float.nan]. *)

val shift : int -> ('freq, (float, 'b) Nx.t) t -> ('freq, (float, 'b) Nx.t) t
(** [shift n s] lags values by [n] positions (positive [n] shifts forward,
    vacated leading positions are [Float.nan]; negative [n] shifts backward,
    vacated trailing positions are [Float.nan]). [shift 0] returns an identical
    series. If [abs n >= length s], all values are [Float.nan]. *)

val pct_change : ('freq, (float, 'b) Nx.t) t -> ('freq, (float, 'b) Nx.t) t
(** [pct_change s] computes [(v[i] - v[i-1]) / v[i-1]] for each element. The
    first element is [Float.nan]. Division follows IEEE 754: [x / 0.0] yields
    [infinity] or [neg_infinity]; [0.0 / 0.0] yields [nan]. *)

val first_valid : ('freq, (float, 'b) Nx.t) t -> (int * float) option
(** [first_valid s] returns [Some (i, v)] where [i] is the position of the first
    non-NaN value [v], or [None] if the series is empty or all NaN. *)

val ffill : ('freq, (float, 'b) Nx.t) t -> ('freq, (float, 'b) Nx.t) t
(** [ffill s] replaces each [Float.nan] with the most recent preceding non-NaN
    value (by position). Leading NaNs with no preceding non-NaN remain
    [Float.nan]. *)

val bfill : ('freq, (float, 'b) Nx.t) t -> ('freq, (float, 'b) Nx.t) t
(** [bfill s] replaces each [Float.nan] with the nearest subsequent non-NaN
    value (by position). Trailing NaNs with no subsequent non-NaN remain
    [Float.nan]. *)

(** {1 Accumulation}

    Prefix-accumulation (scan / running fold) operations over float series. Each
    produces a new series of the same length and index where element [i] is the
    left-fold of the first [i+1] input elements. Single-pass O(n). NaN
    propagation follows IEEE 754 defaults. *)

val scan :
  (float -> float -> float) ->
  float ->
  ('freq, (float, 'b) Nx.t) t ->
  ('freq, (float, Bigarray.float64_elt) Nx.t) t
(** [scan f init s] computes a prefix accumulation (left-scan) over the series
    values. Element [i] of the result is
    [fold_left f init (values.(0) ... values.(i))]. The series index and
    frequency are preserved. The output element type is always
    [Bigarray.float64_elt] regardless of the input element type.

    [scan] is total: an empty series produces an empty series.

    {b NaN propagation.} A NaN at position [i] poisons the accumulator: all
    elements from [i] onward are NaN (IEEE 754 arithmetic). *)

val cumsum :
  ('freq, (float, 'b) Nx.t) t -> ('freq, (float, Bigarray.float64_elt) Nx.t) t
(** [cumsum s] computes the running sum. Equivalent to [scan ( +. ) 0.0 s].

    A NaN at position [i] causes all subsequent elements to be NaN. *)

val cumprod :
  ('freq, (float, 'b) Nx.t) t -> ('freq, (float, Bigarray.float64_elt) Nx.t) t
(** [cumprod s] computes the running product. Equivalent to [scan ( *. ) 1.0 s].

    A NaN at position [i] causes all subsequent elements to be NaN. *)
