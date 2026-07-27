(** CSV loading for Cairos.

    Four constructors: two per loading path (single-series and
    multi-instrument), each path offering a "standard shape" constructor with no
    configuration and an explicit constructor that forces every shape parameter.
    No optional arguments. No behavioural defaults.

    {2 Supported file format}

    The parser is hand-rolled for unquoted ASCII CSV. Format constants are not
    caller-configurable — they describe the file shape the package accepts, not
    a behavioural choice:

    - Delimiter: comma. Semicolon or tab files are out of scope at MVP.
    - Quoting: none. Files containing quoted fields, embedded commas inside
      fields, or escaped quotes are not supported and will parse as malformed
      rows (typically surfacing as {!Too_few_columns} or a silently split
      value).
    - Line endings: [LF] or [CRLF] (the trailing [\r] is stripped).
    - Byte-order mark: a single UTF-8 BOM on line 1 is stripped; other BOMs are
      not handled.

    Files that violate these assumptions should be pre-processed by the caller
    or loaded through a dedicated CSV library. If production data requires
    quoted-field support, surface the tradeoff before adding it. *)

open Cairos

(** {1 Errors}

    All four constructors return a structured error so callers can pattern-match
    on the failure mode and recover the offending line, column, or argument
    without scanning error strings.

    Line numbers are 1-indexed positions in the source file, counting the header
    row when there is one — the number a user can act on. This is why the parse
    failures below are CSV-domain variants rather than a wrapped
    [Cairos.Index.err], whose positions index the timestamp array. *)

type column_arg =
  | Timestamp_col  (** The [~timestamp_col] argument. *)
  | Price_col  (** The [~price_col] argument. *)

type err =
  | File_not_found of { path : string; cause : string }
      (** [path] could not be opened; [cause] is the operating system's message.
          Also covers a path that exists but cannot be read. *)
  | Empty_file of { path : string }
      (** [path] holds no lines, or only whitespace-only lines. *)
  | Header_only of { path : string }
      (** [path] holds a header row and no data rows. Only reachable under
          [~header:true]. *)
  | Too_few_columns of { line_no : int; expected : int; found : int }
      (** The row at [line_no] has [found] fields where the requested column
          layout needs at least [expected]. *)
  | Unparseable_timestamp of { line_no : int; raw : string }
      (** [raw], the timestamp field of the row at [line_no], is not a valid
          timestamp for the requested frequency. *)
  | Non_monotonic_timestamps of { line_no : int }
      (** The timestamp at [line_no] does not come strictly after the one before
          it. *)
  | Non_finite_price of { line_no : int; raw : string }
      (** [raw], the price field of the row at [line_no], is NaN, an infinity,
          or not a float at all. Single-series path only: [frame_of_csv] accepts
          infinities. *)
  | Unparseable_float_in_cell of { line_no : int; col : int; raw : string }
      (** [raw], at zero-indexed column [col] of the row at [line_no], is
          present and is not a float. An absent cell is [Float.nan], not an
          error. *)
  | Duplicate_header of { col_a : int; col_b : int; name : string }
      (** The header names two instrument columns [name], at zero-indexed
          columns [col_a] and [col_b]. *)
  | Invalid_column_arg of { arg : column_arg; value : int }
      (** The caller passed [value] for the [arg] column argument, which is
          negative. *)
  | Duplicate_column_arg of { value : int }
      (** The caller passed [value] for both [~timestamp_col] and [~price_col];
          the two must differ. *)
  | Empty_frame_columns of { path : string }
      (** [path] has no columns left once the timestamp column is removed, so
          there is no instrument to load. *)
  | Series_error of Series.err
      (** A parsed column was rejected by [Cairos.Series.make]. Unreachable
          through the constructors below — both paths build values and index
          from the same row count — and carried so that no call is unwrapped
          rather than because a caller can observe it. *)
  | Frame_error of Frame.err
      (** The parsed columns were rejected by [Cairos.Frame.of_series].
          Unreachable for the same reason: every column carries the same index
          and duplicate names are rejected as [Duplicate_header] first. *)

val err_to_string : err -> string
(** Render [err] as a human-readable one-line message. [Series_error] and
    [Frame_error] render the wrapped error too, so one call yields the complete
    message, and both carry an ["internal: "] marker — as
    [Cairos.Resample.err_to_string] does for its two unreachable arms — so a
    reader can tell a broken library invariant from bad input. Message prose is
    not part of the contract; match on the variant instead. *)

(** {1 Single-series} *)

val of_csv :
  freq:'freq Freq.t ->
  string ->
  (('freq, (float, Bigarray.float64_elt) Nx.t) Series.t, err) result
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
  (('freq, (float, Bigarray.float64_elt) Nx.t) Series.t, err) result
(** [of_csv_with ~freq ~header ~timestamp_col ~price_col path] loads a
    single-series CSV with explicit layout. [~timestamp_col] and [~price_col]
    are zero-indexed; must be non-negative and distinct. *)

(** {1 Multi-instrument frame} *)

val frame_of_csv : freq:'freq Freq.t -> string -> ('freq Frame.t, err) result
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
  ('freq Frame.t, err) result
(** [frame_of_csv_with ~freq ~header ~timestamp_col path] loads a wide-format
    multi-instrument CSV with explicit layout. Every non-timestamp column is
    loaded as an instrument column. When [~header:false], columns are named
    positionally [col_1], [col_2], … starting from the first non-timestamp
    column. *)
