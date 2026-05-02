let schema_version = "cairos-bench-baseline-v1"
let monotonic_clock_label = "monotonic-clock"

type cell = {
  bench : string;
  name : string;
  instance : string;
  estimate : float;
  r_square : float;
}

let output_mode () =
  match Sys.getenv_opt "CAIROS_BENCH_OUTPUT" with
  | Some "json" -> `Json
  | _ -> `Notty

(* Round to %.6g (~6 significant digits) so the emitted JSON byte output is
   stable across rebaselines: a 0.1% wall-clock drift produces a one-character
   diff, not a re-formatted line.  Non-finite values pass through unchanged
   (defensive — a well-formed bench run never produces them). *)
let round_6g f =
  if Float.is_finite f then float_of_string (Printf.sprintf "%.6g" f) else f

let cell_to_json c =
  `Assoc
    [
      ("estimate", `Float (round_6g c.estimate));
      ("instance", `String c.instance);
      ("name", `String c.name);
      ("r_square", `Float (round_6g c.r_square));
    ]

let compare_by_name_instance a b =
  match String.compare a.name b.name with
  | 0 -> String.compare a.instance b.instance
  | c -> c

let cells_of_results ~bench results instances =
  List.fold_left
    (fun acc w ->
      let instance = Bechamel.Measure.label w in
      match Hashtbl.find_opt results instance with
      | None -> acc
      | Some inner ->
          Hashtbl.fold
            (fun name ols acc ->
              let estimate =
                match Bechamel.Analyze.OLS.estimates ols with
                | Some (e :: _) -> e
                | _ -> Float.nan
              in
              let r_square =
                match Bechamel.Analyze.OLS.r_square ols with
                | Some r -> r
                | None -> Float.nan
              in
              { bench; name; instance; estimate; r_square } :: acc)
            inner acc)
    [] instances

let bench_doc_of_cells ~bench cells =
  let sorted = List.sort compare_by_name_instance cells in
  `Assoc
    [
      ("$schema", `String schema_version);
      ("bench", `String bench);
      ("cells", `List (List.map cell_to_json sorted));
    ]

let to_channel oc ~bench results instances =
  let cells = cells_of_results ~bench results instances in
  Yojson.Basic.to_channel oc (bench_doc_of_cells ~bench cells)

let ( let* ) = Result.bind

let as_string : Yojson.Basic.t -> (string, string) result = function
  | `String s -> Ok s
  | _ -> Error "expected string"

let as_float : Yojson.Basic.t -> (float, string) result = function
  | `Float f -> Ok f
  | `Int i -> Ok (float_of_int i)
  | _ -> Error "expected number"

let getf fields k =
  match List.assoc_opt k fields with
  | Some v -> Ok v
  | None -> Error (Printf.sprintf "missing field %s" k)

let parse_cell ~bench (json : Yojson.Basic.t) =
  match json with
  | `Assoc fields ->
      let* nv = getf fields "name" in
      let* name = as_string nv in
      let* iv = getf fields "instance" in
      let* instance = as_string iv in
      let* ev = getf fields "estimate" in
      let* estimate = as_float ev in
      let* rv = getf fields "r_square" in
      let* r_square = as_float rv in
      Ok { bench; name; instance; estimate; r_square }
  | _ -> Error "expected cell object"

let parse_bench_doc (json : Yojson.Basic.t) =
  match json with
  | `Assoc fields -> (
      let* sv = getf fields "$schema" in
      let* schema = as_string sv in
      if schema <> schema_version then
        Error
          (Printf.sprintf "unknown bench schema version: %s (expected %s)"
             schema schema_version)
      else
        let* bv = getf fields "bench" in
        let* bench = as_string bv in
        let* cv = getf fields "cells" in
        match cv with
        | `List items ->
            let rec collect acc = function
              | [] -> Ok (List.rev acc)
              | item :: rest ->
                  let* c = parse_cell ~bench item in
                  collect (c :: acc) rest
            in
            collect [] items
        | _ -> Error "cells not an array")
  | _ -> Error "expected bench object"

let parse_consolidated (json : Yojson.Basic.t) =
  match json with
  | `Assoc top -> (
      let* sv = getf top "$schema" in
      let* schema = as_string sv in
      if schema <> schema_version then
        Error
          (Printf.sprintf "unknown schema version: %s (expected %s)" schema
             schema_version)
      else
        let* bv = getf top "benches" in
        match bv with
        | `List [] ->
            Error
              "consolidated document has empty benches array (run just \
               bench-record)"
        | `List items ->
            let rec collect acc = function
              | [] -> Ok (List.rev acc)
              | item :: rest ->
                  let* cells = parse_bench_doc item in
                  collect (List.rev_append cells acc) rest
            in
            collect [] items
        | _ -> Error "benches not an array")
  | _ -> Error "expected top-level object"

let load_baseline ~path =
  match Yojson.Basic.from_file path with
  | exception Sys_error msg ->
      Error (Printf.sprintf "cannot read %s: %s" path msg)
  | exception Yojson.Json_error msg ->
      Error (Printf.sprintf "json parse error in %s: %s" path msg)
  | json -> parse_consolidated json

let read_bench_dir ~path =
  let entries =
    match Sys.readdir path with
    | exception Sys_error msg ->
        Error (Printf.sprintf "cannot read directory %s: %s" path msg)
    | xs -> Ok xs
  in
  let* entries = entries in
  let json_paths =
    Array.to_list entries
    |> List.filter (fun name -> Filename.check_suffix name ".json")
    |> List.sort String.compare
    |> List.map (fun name -> Filename.concat path name)
  in
  match json_paths with
  | [] ->
      Error
        (Printf.sprintf
           "no per-bench JSON files in %s (expected one *.json per bench)" path)
  | _ ->
      let rec collect acc = function
        | [] -> Ok (List.rev acc)
        | p :: rest ->
            let parsed =
              match Yojson.Basic.from_file p with
              | exception Sys_error msg ->
                  Error (Printf.sprintf "cannot read %s: %s" p msg)
              | exception Yojson.Json_error msg ->
                  Error (Printf.sprintf "json parse error in %s: %s" p msg)
              | json -> parse_bench_doc json
            in
            let* cells = parsed in
            collect (List.rev_append cells acc) rest
      in
      collect [] json_paths

let write_consolidated ~path cells =
  let by_bench = Hashtbl.create 16 in
  List.iter
    (fun c ->
      let prev =
        match Hashtbl.find_opt by_bench c.bench with
        | Some xs -> xs
        | None -> []
      in
      Hashtbl.replace by_bench c.bench (c :: prev))
    cells;
  let groups = Hashtbl.fold (fun b cs acc -> (b, cs) :: acc) by_bench [] in
  let sorted_groups =
    List.sort (fun (a, _) (b, _) -> String.compare a b) groups
  in
  let bench_to_json (bench_name, cs) =
    let sorted = List.sort compare_by_name_instance cs in
    `Assoc
      [
        ("$schema", `String schema_version);
        ("bench", `String bench_name);
        ("cells", `List (List.map cell_to_json sorted));
      ]
  in
  let json =
    `Assoc
      [
        ("$schema", `String schema_version);
        ("benches", `List (List.map bench_to_json sorted_groups));
      ]
  in
  match open_out path with
  | exception Sys_error msg ->
      Error (Printf.sprintf "cannot write %s: %s" path msg)
  | oc ->
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () ->
          Yojson.Basic.pretty_to_channel oc json;
          output_char oc '\n');
      Ok ()

type regression = { cell : cell; baseline : cell; ratio : float }

type validated_pair = {
  baseline_monotonic : cell list;
  current_monotonic : cell list;
}

let sort_pairs =
  List.sort (fun (b1, n1) (b2, n2) ->
      match String.compare b1 b2 with
      | 0 -> String.compare n1 n2
      | k -> k)

let validate_coverage ~baseline ~current =
  let monotonic_only =
    List.filter (fun c -> c.instance = monotonic_clock_label)
  in
  let b = monotonic_only baseline in
  let c = monotonic_only current in
  let key cell = (cell.bench, cell.name) in
  let current_keys = List.map key c in
  let missing =
    List.filter_map
      (fun bl ->
        if List.mem (key bl) current_keys then None else Some (bl.bench, bl.name))
      b
  in
  match missing with
  | [] -> Ok { baseline_monotonic = b; current_monotonic = c }
  | _ -> Error (sort_pairs missing)

type diff_outcome = Ok_no_regression | Regressions of regression list

let regress ~threshold pair =
  let key c = (c.bench, c.name) in
  let baseline_map =
    List.map (fun cell -> (key cell, cell)) pair.baseline_monotonic
  in
  let regs =
    List.filter_map
      (fun cur ->
        match List.assoc_opt (key cur) baseline_map with
        | None -> None
        | Some bl ->
            let ratio = cur.estimate /. bl.estimate in
            if ratio > 1.0 +. threshold then
              Some { cell = cur; baseline = bl; ratio }
            else None)
      pair.current_monotonic
  in
  let sort_regs =
    List.sort (fun r1 r2 ->
        match String.compare r1.cell.bench r2.cell.bench with
        | 0 -> String.compare r1.cell.name r2.cell.name
        | k -> k)
  in
  match regs with
  | [] -> Ok_no_regression
  | _ -> Regressions (sort_regs regs)
