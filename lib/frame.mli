(** Multi-series frame with shared index.

    A [Frame.t] groups named float series that share an identical
    frequency-tagged index. Construction validates index identity across all
    series. Individual series are reconstructed on access by pairing the shared
    index with stored values. *)

type 'freq t

val of_series :
  (string * ('freq, (float, Bigarray.float64_elt) Nx.t) Series.t) list ->
  ('freq t, string) result
(** [of_series pairs] constructs a frame from named series. All series must have
    structurally identical indices (same length, same timestamps in the same
    order). The index of the first series is the reference.

    Returns [Error msg] when:
    - the input list is empty
    - any series has an index that differs from the first (different length or
      different timestamps)

    Column names are preserved in insertion order. *)

val get :
  string ->
  'freq t ->
  ('freq, (float, Bigarray.float64_elt) Nx.t) Series.t option
(** [get name frame] retrieves the series named [name]. Returns [None] if [name]
    is not a column in [frame]. The returned series pairs the frame's shared
    index with the stored values via {!Series.make_unsafe}. *)

val columns : 'freq t -> string list
(** Column names in insertion order. *)

val head : int -> 'freq t -> 'freq t
(** [head n frame] returns a frame with the first [n] rows. Clamps to the frame
    length when [n] exceeds it. Returns an empty frame when [n <= 0]. *)

val tail : int -> 'freq t -> 'freq t
(** [tail n frame] returns a frame with the last [n] rows. Clamps to the frame
    length when [n] exceeds it. Returns an empty frame when [n <= 0]. *)

type column_stats = {
  count : int;
  mean : float;
  std : float;
  min : float;
  p25 : float;
  median : float;
  p75 : float;
  max : float;
}
(** Per-column summary statistics produced by {!describe}. *)

val describe : 'freq t -> (string * column_stats) list
(** [describe f] computes per-column summary statistics. NaN values are excluded
    from all computations. Percentiles use linear interpolation. Returns one
    entry per column in insertion order. If a column is all-NaN, [count] is [0]
    and all float fields are [Float.nan]. Uses population standard deviation
    (ddof=0). *)
