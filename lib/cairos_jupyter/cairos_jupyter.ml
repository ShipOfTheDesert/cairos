open Cairos

(* The ocaml-jupyter kernel captures stdout through a pipe, which makes OCaml
   default to full buffering — Printf output never flushes to the cell without
   an explicit [flush stdout]. Disabling buffering here fixes this globally so
   every write to stdout (from Printf, print_endline, the pp_* helpers below,
   and user notebook code) reaches the kernel immediately. See the .mli for
   how to undo this in a specific notebook if needed. *)
let () = Out_channel.set_buffered stdout false
let display svg = ignore (Jupyter_notebook.display "image/svg+xml" svg)

let pp_series ?(n = 3) name s =
  let len = Series.length s in
  Printf.printf "%s: length=%d\n" name len;
  let show = min n len in
  if show > 0 then begin
    let hd = Series.head show s in
    let tl = Series.tail show s in
    let hv = Nx.to_array (Series.values hd) in
    let tv = Nx.to_array (Series.values tl) in
    Printf.printf "  first %d:" show;
    Array.iter (Printf.printf " %.1f") hv;
    Printf.printf "\n";
    Printf.printf "  last %d:" show;
    Array.iter (Printf.printf " %.1f") tv;
    Printf.printf "\n"
  end

let pp_first_valid name s =
  match Series.first_valid s with
  | None -> Printf.printf "%s: all NaN\n" name
  | Some (i, v) -> Printf.printf "%s first non-NaN at [%d]: %.2f\n" name i v

let pp_frame ?(n = 3) frame =
  let cols = Frame.columns frame in
  let tail_f = Frame.tail n frame in
  Printf.printf "Frame columns: %s\n" (String.concat ", " cols);
  List.iter
    (fun name ->
      match Frame.get name frame with
      | None -> Printf.printf "  %s: not found\n" name
      | Some s ->
          let len = Series.length s in
          let show = min n len in
          let tv =
            match Frame.get name tail_f with
            | Some ts -> Nx.to_array (Series.values ts)
            | None -> [||]
          in
          Printf.printf "  %s: length=%d, last %d:" name len show;
          Array.iter (Printf.printf " %.2f") tv;
          Printf.printf "\n")
    cols

let pp_describe frame =
  let stats = Frame.describe frame in
  Printf.printf "%-12s %5s %8s %8s %8s %8s %8s %8s %8s\n" "column" "count"
    "mean" "std" "min" "p25" "median" "p75" "max";
  Printf.printf "%s\n" (String.make 77 '-');
  List.iter
    (fun (name, (s : Frame.column_stats)) ->
      Printf.printf "%-12s %5d %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f\n" name
        s.count s.mean s.std s.min s.p25 s.median s.p75 s.max)
    stats
