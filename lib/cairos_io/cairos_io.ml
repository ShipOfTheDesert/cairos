type row = { line_no : int; fields : string array }

type err =
  | File_not_found of { path : string; cause : string }
  | Empty_file of { path : string }
  | Header_only of { path : string }
  | Too_few_columns of { line_no : int; expected : int; found : int }
  | Unparseable_timestamp of { line_no : int; raw : string }
  | Non_monotonic_timestamps of { line_no : int }
  | Non_finite_price of { line_no : int; raw : string }
  | Unparseable_float_in_cell of { line_no : int; col : int; raw : string }
  | Duplicate_header of { col_a : int; col_b : int; name : string }
  | Invalid_col_arg of string
  | Empty_frame_columns of { path : string }

let format_err = function
  | File_not_found { path; cause } ->
      Printf.sprintf "cairos_io: file not found: %s (%s)" path cause
  | Empty_file { path } -> Printf.sprintf "cairos_io: empty file: %s" path
  | Header_only { path } ->
      Printf.sprintf "cairos_io: header-only file: %s" path
  | Too_few_columns { line_no; expected; found } ->
      Printf.sprintf
        "cairos_io: line %d: expected at least %d columns, found %d" line_no
        expected found
  | Unparseable_timestamp { line_no; raw } ->
      Printf.sprintf "cairos_io: line %d: invalid timestamp %S" line_no raw
  | Non_monotonic_timestamps { line_no } ->
      Printf.sprintf "cairos_io: line %d: timestamps not strictly monotonic"
        line_no
  | Non_finite_price { line_no; raw } ->
      Printf.sprintf "cairos_io: line %d: non-finite price %S" line_no raw
  | Unparseable_float_in_cell { line_no; col; raw } ->
      Printf.sprintf "cairos_io: line %d, col %d: unparseable float %S" line_no
        col raw
  | Duplicate_header { col_a; col_b; name } ->
      Printf.sprintf
        "cairos_io: line 1: duplicate header column %S at cols %d and %d" name
        col_a col_b
  | Invalid_col_arg detail ->
      Printf.sprintf "cairos_io: invalid argument: %s" detail
  | Empty_frame_columns { path } ->
      Printf.sprintf "cairos_io: no instrument columns in %s" path

let strip_trailing_cr s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s

let strip_leading_bom s =
  let bom = "\xEF\xBB\xBF" in
  if String.length s >= 3 && String.sub s 0 3 = bom then
    String.sub s 3 (String.length s - 3)
  else s

(* Sys_error is the one place an exception crosses a stdlib boundary in
   this module; it is caught here and converted to File_not_found, then
   callers see only [result]. *)
let read_lines path : (string list, err) result =
  try
    In_channel.with_open_text path (fun ic ->
        let rec loop acc first_line =
          match In_channel.input_line ic with
          | None -> List.rev acc
          | Some raw ->
              let line = strip_trailing_cr raw in
              let line = if first_line then strip_leading_bom line else line in
              loop (line :: acc) false
        in
        Ok (loop [] true))
  with
  | Sys_error cause -> Error (File_not_found { path; cause })

let split_line s = Array.of_list (String.split_on_char ',' s)
let data_line_offset ~header = if header then 2 else 1

type parse_result = {
  header_fields : string array option;
  rows : row Cairos.Nonempty.t;
}

(* Single validation point for empty / whitespace-only / header-only files.
   Returns a non-empty rows collection by construction — no [assert false]
   downstream. The header line (when [header=true]) is surfaced separately
   so callers that need column shape can read it without re-parsing. *)
let parse_rows ~header ~path lines : (parse_result, err) result =
  let row_of line_no line = { line_no; fields = split_line line } in
  match lines with
  | [] -> Error (Empty_file { path })
  | _ when List.for_all (fun s -> String.trim s = "") lines ->
      Error (Empty_file { path })
  | [ _ ] when header -> Error (Header_only { path })
  | hdr :: first :: rest when header ->
      let first_row = row_of 2 first in
      let rest_rows = List.mapi (fun i l -> row_of (i + 3) l) rest in
      Ok
        {
          header_fields = Some (split_line hdr);
          rows = Cairos.Nonempty.make first_row rest_rows;
        }
  | first :: rest ->
      let first_row = row_of 1 first in
      let rest_rows = List.mapi (fun i l -> row_of (i + 2) l) rest in
      Ok
        {
          header_fields = None;
          rows = Cairos.Nonempty.make first_row rest_rows;
        }

(* Collect timestamp strings and prices in CSV-line order; fail fast on the
   first per-row shape/parse error. Monotonicity and timestamp parsing are
   delegated to [dispatch_index] below — per-row work here is only what we
   can validate without the whole array. *)
let collect ~timestamp_col ~price_col rows =
  let n = List.length rows in
  let timestamps = Array.make n "" in
  let prices = Array.make n 0.0 in
  let needed = max timestamp_col price_col + 1 in
  let rec loop i = function
    | [] -> Ok ()
    | { line_no; fields } :: rest -> (
        let ncol = Array.length fields in
        if ncol < needed then
          Error (Too_few_columns { line_no; expected = needed; found = ncol })
        else
          let raw = fields.(price_col) in
          match Float.of_string_opt raw with
          | Some f when Float.is_finite f ->
              timestamps.(i) <- fields.(timestamp_col);
              prices.(i) <- f;
              loop (i + 1) rest
          | _ -> Error (Non_finite_price { line_no; raw }))
  in
  match loop 0 rows with
  | Error _ as e -> e
  | Ok () -> Ok (timestamps, prices)

(* Locally abstract type refines the index result constructor-by-constructor.
   See ~/.claude/solutions/ocaml/gadt-exhaustiveness-locally-abstract-type.md
   — the (type freq) annotation lives on this private helper so the public
   [of_csv_with] keeps a plain ['freq] signature. *)
let dispatch_index (type freq) (f : freq Cairos.Freq.t) (ts : string array) :
    (freq Cairos.Index.t, Cairos.Index.err) result =
  match f with
  | Cairos.Freq.Day -> Cairos.Index.daily ts
  | Cairos.Freq.Minute -> Cairos.Index.minute ts
  | Cairos.Freq.Hour -> Cairos.Index.hourly ts
  | Cairos.Freq.Week -> Cairos.Index.weekly ts

(* Translate an [Index.err] to the cairos_io [err] carrying a CSV line
   number. CSV line = 0-indexed position + (1 if no header else 2). *)
let index_err_to_cairos_io ~offset (index_err : Cairos.Index.err) : err =
  match index_err with
  | Cairos.Index.Invalid_timestamp { position; raw } ->
      Unparseable_timestamp { line_no = position + offset; raw }
  | Cairos.Index.Non_monotonic { position } ->
      Non_monotonic_timestamps { line_no = position + offset }
  | Cairos.Index.Invalid_unix_timestamp { position; raw } ->
      (* Reachable only if a future caller swaps in [of_unix_floats]; current
         path uses the string smart constructors exclusively. *)
      Unparseable_timestamp
        { line_no = position + offset; raw = Float.to_string raw }

let of_csv_with (type freq) ~(freq : freq Cairos.Freq.t) ~header ~timestamp_col
    ~price_col path :
    ((freq, (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t, string) result
    =
  let ( let* ) = Result.bind in
  let wrap r = Result.map_error format_err r in
  let invalid detail = Error (format_err (Invalid_col_arg detail)) in
  if timestamp_col < 0 then
    invalid (Printf.sprintf "timestamp_col %d < 0" timestamp_col)
  else if price_col < 0 then
    invalid (Printf.sprintf "price_col %d < 0" price_col)
  else if timestamp_col = price_col then
    invalid
      (Printf.sprintf "timestamp_col and price_col must differ (both = %d)"
         timestamp_col)
  else
    let* lines = wrap (read_lines path) in
    let* parsed = wrap (parse_rows ~header ~path lines) in
    let rows = Cairos.Nonempty.to_list parsed.rows in
    let* timestamps, prices = wrap (collect ~timestamp_col ~price_col rows) in
    let offset = data_line_offset ~header in
    let* index =
      dispatch_index freq timestamps
      |> Result.map_error (fun e ->
          format_err (index_err_to_cairos_io ~offset e))
    in
    let n = Array.length prices in
    let values = Nx.create Nx.float64 [| n |] prices in
    Cairos.Series.make index values

let of_csv ~freq path =
  of_csv_with ~freq ~header:true ~timestamp_col:0 ~price_col:1 path

(* Instrument columns for a wide-format frame file, in source-file order. For
   [~header:false] the name is positional [col_1], [col_2], ... where the
   index is a 1-based count among non-timestamp columns (FR-8). *)
let collect_frame_columns ~header ~timestamp_col reference_fields =
  let n = Array.length reference_fields in
  List.init n Fun.id
  |> List.filter_map (fun i ->
      if i = timestamp_col then None
      else
        let name =
          if header then reference_fields.(i)
          else
            let p = i - if i > timestamp_col then 1 else 0 in
            Printf.sprintf "col_%d" (p + 1)
        in
        Some (i, name))

(* Scan instrument columns (already in source-file order) for the first
   header-name collision. Caller gates on ~header:true — positional names
   (col_1, col_2, ...) are unique by construction.

   Frame.of_series (lib/frame.ml:18-24) also rejects duplicate column names,
   but its message anchors to the name only. We check earlier so the error
   reports both source-file column positions and uses the "duplicate header"
   vocabulary the RFC fixes in Step 5. *)
let find_duplicate_header columns =
  let rec check = function
    | []
    | [ _ ] ->
        None
    | (col_a, name) :: rest -> (
        match List.find_opt (fun (_, n) -> n = name) rest with
        | Some (col_b, _) -> Some (col_a, col_b, name)
        | None -> check rest)
  in
  check columns

(* Per-row collection for the frame path. A missing cell — short row OR
   empty string between commas — is treated as [Float.nan] per RFC Step 4
   and PRD FR-7. A present cell that fails [Float.of_string_opt] is an
   [Unparseable_float_in_cell] error. No [Float.is_finite] check here: [inf]
   is accepted on the frame path (PRD Decision 3). *)
let frame_collect ~timestamp_col ~instrument_cols rows =
  let n = List.length rows in
  let ncols = List.length instrument_cols in
  let timestamps = Array.make n "" in
  let column_values = Array.init ncols (fun _ -> Array.make n Float.nan) in
  let rec loop i = function
    | [] -> Ok ()
    | { line_no; fields } :: rest ->
        let flen = Array.length fields in
        if flen <= timestamp_col then
          Error
            (Too_few_columns
               { line_no; expected = timestamp_col + 1; found = flen })
        else begin
          timestamps.(i) <- fields.(timestamp_col);
          let rec parse_cols j = function
            | [] -> Ok ()
            | (col_idx, _name) :: rest_cols -> (
                let v =
                  if col_idx >= flen then Ok Float.nan
                  else
                    let raw = fields.(col_idx) in
                    if raw = "" then Ok Float.nan
                    else
                      match Float.of_string_opt raw with
                      | Some f -> Ok f
                      | None ->
                          Error
                            (Unparseable_float_in_cell
                               { line_no; col = col_idx; raw })
                in
                match v with
                | Error _ as e -> e
                | Ok f ->
                    column_values.(j).(i) <- f;
                    parse_cols (j + 1) rest_cols)
          in
          match parse_cols 0 instrument_cols with
          | Error _ as e -> e
          | Ok () -> loop (i + 1) rest
        end
  in
  match loop 0 rows with
  | Error _ as e -> e
  | Ok () -> Ok (timestamps, column_values)

let frame_of_csv_with (type freq) ~(freq : freq Cairos.Freq.t) ~header
    ~timestamp_col path : (freq Cairos.Frame.t, string) result =
  let ( let* ) = Result.bind in
  let wrap r = Result.map_error format_err r in
  let invalid detail = Error (format_err (Invalid_col_arg detail)) in
  if timestamp_col < 0 then
    invalid (Printf.sprintf "timestamp_col %d < 0" timestamp_col)
  else
    let* lines = wrap (read_lines path) in
    let* parsed = wrap (parse_rows ~header ~path lines) in
    let reference_fields =
      match parsed.header_fields with
      | Some h -> h
      | None -> (Cairos.Nonempty.hd parsed.rows).fields
    in
    let ncols = Array.length reference_fields in
    if ncols <= timestamp_col then
      Error
        (format_err
           (Too_few_columns
              { line_no = 1; expected = timestamp_col + 1; found = ncols }))
    else
      let instrument_cols =
        collect_frame_columns ~header ~timestamp_col reference_fields
      in
      let* () =
        if header then
          match find_duplicate_header instrument_cols with
          | Some (col_a, col_b, name) ->
              Error (format_err (Duplicate_header { col_a; col_b; name }))
          | None -> Ok ()
        else Ok ()
      in
      let rows = Cairos.Nonempty.to_list parsed.rows in
      let* timestamps, col_values =
        wrap (frame_collect ~timestamp_col ~instrument_cols rows)
      in
      let offset = data_line_offset ~header in
      let* index =
        dispatch_index freq timestamps
        |> Result.map_error (fun e ->
            format_err (index_err_to_cairos_io ~offset e))
      in
      let n = Array.length timestamps in
      let* named =
        instrument_cols
        |> List.mapi (fun j (_, name) -> (name, col_values.(j)))
        |> List.fold_left
             (fun acc (name, arr) ->
               let* xs = acc in
               let values = Nx.create Nx.float64 [| n |] arr in
               let* s = Cairos.Series.make index values in
               Ok ((name, s) :: xs))
             (Ok [])
      in
      let* named_ne =
        Cairos.Nonempty.of_list (List.rev named)
        |> Option.to_result ~none:(format_err (Empty_frame_columns { path }))
      in
      Cairos.Frame.of_series named_ne

let frame_of_csv ~freq path =
  frame_of_csv_with ~freq ~header:true ~timestamp_col:0 path
