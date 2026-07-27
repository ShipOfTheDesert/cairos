(** Multi-series frame with shared index.

    A [Frame.t] groups named float series that share an identical
    frequency-tagged index. Construction validates index identity across all
    series. Individual series are reconstructed on access by pairing the shared
    index with stored values. *)

type 'freq t

(** {1 Errors}

    {!of_series} returns a structured error so callers can pattern-match on the
    failure mode and recover the offending column without scanning error
    strings. *)

type err =
  | Duplicate_column of { name : string }
      (** Two of the supplied pairs carried the column name [name]. The
          positions are not carried: the scan reports the first repeat it
          reaches and holds no column index. *)
  | Index_mismatch of {
      column : string;
      expected_length : int;
      found_length : int;
    }
      (** The index of column [column] is not structurally identical to the
          reference index, which is the first pair's. [expected_length] is the
          reference index's length and [found_length] is [column]'s. Equal
          lengths mean the two agree in length and differ in their timestamps or
          the order of them. *)

val err_to_string : err -> string
(** Render [err] as a human-readable one-line message. *)

val of_series :
  (string * ('freq, (float, Bigarray.float64_elt) Nx.t) Series.t) Nonempty.t ->
  ('freq t, err) result
(** [of_series pairs] constructs a frame from named series. All series must have
    structurally identical indices (same length, same timestamps in the same
    order). The index of the first series is the reference.

    Non-emptiness is a structural precondition — the [Nonempty.t] parameter
    makes a zero-column Frame unrepresentable at compile time. Remaining runtime
    errors: [Error (Duplicate_column _)] for a repeated column name, checked
    first, and [Error (Index_mismatch _)] for a series whose index differs from
    the first series'.

    Column names are preserved in insertion order. *)

val get :
  string ->
  'freq t ->
  ('freq, (float, Bigarray.float64_elt) Nx.t) Series.t option
(** [get name frame] retrieves the series named [name]. Returns [None] if [name]
    is not a column in [frame]. A [Frame.t] always has at least one column; this
    returning [None] means the name is absent, not that the frame is empty. The
    returned series pairs the frame's shared index with the stored values via
    {!Series.make_unsafe}. *)

val columns : 'freq t -> string Nonempty.t
(** Column names in insertion order. The [Nonempty.t] carries the frame's
    one-column-minimum invariant: a [Frame.t] is built from a [Nonempty.t] of
    pairs by {!of_series}, so callers destructure the result without an empty
    case. *)

val index : 'freq t -> 'freq Index.t
(** [index frame] returns the index shared by every column. Total: a [Frame.t]
    owns exactly one index, validated at construction against every member
    series. Callers do not need to reach for an arbitrary column via {!get} to
    recover it. *)

val to_series :
  'freq t ->
  (string * ('freq, (float, Bigarray.float64_elt) Nx.t) Series.t) Nonempty.t
(** [to_series frame] returns every column as a named series, in insertion
    order. Total inverse of {!of_series}: feeding the result back into
    {!of_series} yields [Ok] with an equal frame, since neither the
    duplicate-name nor the index-mismatch error can arise from a frame's own
    columns. Each series pairs {!index} with the stored values, as {!get} does.
*)

val mapi_cells :
  f:(col:int -> name:string -> row:int -> float -> float) -> 'freq t -> 'freq t
(** [mapi_cells ~f frame] returns a frame with the same shape as [frame] — the
    same column names in the same order and the same [Index.t] — where the cell
    at column position [col] and row position [row] holds
    [f ~col ~name ~row cell]. [col] is the 0-based position of the column in
    {!columns}, [name] is that column's name, [row] is the 0-based position in
    {!index}, and [cell] is the input value there.

    Total: the output shape is taken from [frame], so neither the duplicate-name
    nor the index-mismatch error of {!of_series} can arise. Callers deriving a
    frame over an existing frame's shape use this rather than round-tripping
    through {!of_series} and handling a [result] that cannot be [Error].

    [f] is applied exactly once per cell; the traversal order is unspecified.
    NaN cells reach [f] as [Float.nan]; [f] is responsible for its own NaN
    handling. *)

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
    (ddof=0).

    The ddof difference from {!zscore} (which uses ddof=1) is deliberate: this
    function summarises a column as the whole population it is, while {!zscore}
    treats each row's cross-section as a sample. *)

(** {1 Cross-sectional operations}

    All three operations traverse the input frame row-by-row. At each timestamp
    the per-column values are gathered into a length-[C] buffer and the
    operation reduces, ranks, or normalises across that buffer. NaN cells are
    excluded from the per-row aggregate but preserved as NaN in the output.

    All three are total: no [result], no exceptions. Edge cases (single-column
    frame, all-NaN row, constant row, ≤1-non-NaN-cell row for [zscore]) emit
    [Float.nan] at the appropriate cells.

    Output frequency tag and index are structurally identical to the input
    frame's: same length, same [Ptime.t] array, same order.

    Performance: the column-major [Frame.t] layout is optimised for up to ~500
    instruments. Cross-sectional operations above ~1000 instruments require a
    matrix-backed [Frame] (post-MVP, not yet landed). The per-row scratch buffer
    is allocated once outside the row loop and reused. *)

val column_map :
  f:(float array -> float) ->
  'freq t ->
  ('freq, (float, Bigarray.float64_elt) Nx.t) Series.t
(** [column_map ~f frame] reduces the cross-section of [frame] at each timestamp
    to a single float via [f].

    For each timestamp [t] in the input frame, [f] is invoked exactly once with
    a [float array] holding the per-column values at [t] in [columns frame]
    order. The return value becomes the corresponding cell in the output series.
    NaN cells reach [f] as [Float.nan]; [f] is responsible for its own NaN
    handling.

    The output series carries the input frame's ['freq] phantom tag and the same
    [Index.t]. [Series.length output = Index.length (index frame)].

    The buffer passed to [f] is reused across timestamps; [f] must not retain a
    reference to it across calls. ([f] may copy out values it needs to keep.) *)

val rank : 'freq t -> 'freq t
(** [rank frame] returns a frame of the same shape where each cell holds the
    cross-sectional rank of the corresponding input cell among the non-NaN cells
    in its row.

    Ranks are 1-based: the smallest non-NaN value at a timestamp gets rank
    [1.0]; the largest gets rank [N], where [N] is the count of non-NaN cells at
    that timestamp. Ties resolve to the average of the rank positions they would
    occupy (matching Pandas [DataFrame.rank(axis=1, method='average')]).

    NaN cells in the input remain [Float.nan] in the output and are excluded
    from [N]. A row of all-NaN cells produces an all-NaN output row. A
    single-column frame produces an output where every non-NaN cell is [1.0]. A
    constant row of [N] non-NaN cells produces [N] cells all holding
    [(N + 1) / 2].

    The output frame carries the input's ['freq] phantom tag, the same
    [Index.t], and the same column names in the same order.

    Performance: see {!column_map}'s note on the ~500-instrument boundary. *)

val zscore : 'freq t -> 'freq t
(** [zscore frame] returns a frame of the same shape where each cell holds the
    cross-sectional z-score of the corresponding input cell relative to the
    non-NaN cells in its row.

    For each timestamp [t]: let [N] be the count of non-NaN cells at [t], [m]
    their mean, [s] their sample standard deviation ([ddof=1], matching
    [Cairos_finance.annualised_vol]). The output at column [c] and timestamp [t]
    is [(input[c, t] -. m) /. s] when [N >= 2] and [s <> 0]; [Float.nan]
    otherwise. NaN cells in the input remain [Float.nan].

    The ddof difference from {!describe} (which uses ddof=0) is deliberate: this
    function treats each row's cross-section as a sample drawn from a wider
    universe of instruments, while {!describe} summarises a column as the whole
    population it is.

    Edge cases:
    - All-NaN row → all-NaN output row.
    - Constant row ([s = 0]) → all-NaN output row (NaN cells stay NaN).
    - Row with [≤ 1] non-NaN cell → all-NaN output row.
    - Single-column frame → every output cell is [Float.nan].

    The output frame carries the input's ['freq] phantom tag, the same
    [Index.t], and the same column names in the same order.

    Performance: see {!column_map}'s note on the ~500-instrument boundary. *)
