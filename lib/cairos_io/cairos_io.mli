(** CSV loading for Cairos.

    Four constructors: two per loading path (single-series and
    multi-instrument), each path offering a "standard shape" constructor with no
    configuration and an explicit constructor that forces every shape parameter.
    No optional arguments. No behavioural defaults. *)

open Cairos

(** {1 Single-series} *)

val of_csv :
  freq:'freq Freq.t ->
  string ->
  (('freq, (float, Bigarray.float64_elt) Nx.t) Series.t, string) result
(** [of_csv ~freq path] loads a two-column CSV file in the standard shape:
    header row present, timestamp in column 0, price in column 1, comma
    separator, ASCII. Errors include the 1-indexed line number.

    Non-finite values in the price column (NaN, positive or negative infinity,
    or any string that fails [Float.of_string_opt]) are errors. This is
    deliberately stricter than {!frame_of_csv}, which accepts [inf] as a
    sentinel: callers with multiple instruments should load via {!frame_of_csv}
    and compose {!Cairos.Align.align} rather than stitching several {!of_csv}
    results together. *)

val of_csv_with :
  freq:'freq Freq.t ->
  header:bool ->
  timestamp_col:int ->
  price_col:int ->
  string ->
  (('freq, (float, Bigarray.float64_elt) Nx.t) Series.t, string) result
(** [of_csv_with ~freq ~header ~timestamp_col ~price_col path] loads a
    single-series CSV with explicit layout. [~timestamp_col] and [~price_col]
    are zero-indexed; must be non-negative and distinct. *)

(** {1 Multi-instrument frame} *)

val frame_of_csv : freq:'freq Freq.t -> string -> ('freq Frame.t, string) result
(** [frame_of_csv ~freq path] loads a wide-format multi-instrument CSV in the
    standard shape: header row present, timestamp in column 0, every subsequent
    column an instrument whose header value becomes its column name in the
    resulting {!Cairos.Frame.t}. Missing cells (short rows or empty values
    between commas) are filled with [Float.nan], consistent with
    [Align.align ~strategy:`Left]. Callers who need strict alignment should
    compose {!Cairos.Align.align} after loading. *)

val frame_of_csv_with :
  freq:'freq Freq.t ->
  header:bool ->
  timestamp_col:int ->
  string ->
  ('freq Frame.t, string) result
(** [frame_of_csv_with ~freq ~header ~timestamp_col path] loads a wide-format
    multi-instrument CSV with explicit layout. Every non-timestamp column is
    loaded as an instrument column. When [~header:false], columns are named
    positionally [col_1], [col_2], … starting from the first non-timestamp
    column (PRD FR-8). *)
