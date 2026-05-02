(* Run with:
     opam exec -- dune exec bench/bench_compare.exe -- \
       --baseline bench/baseline.json --bench-dir /tmp/bench-cur

   Loads the committed baseline and a directory of per-bench JSON tempfiles
   produced by the just bench-compare shell loop, validates that every
   baseline (bench, name) is covered by current, then diffs wall-clock cells.
   Exits non-zero on any baseline cell missing from current OR any >20%
   wall-clock regression.  Threshold is hardcoded per PRD Decision 3 / RFC
   §Options Considered E.  Allocation cells are ignored per PRD Decision 2. *)

let print_regression (r : Bench_emit.regression) =
  Printf.printf "regression: %s/%s  baseline=%g  current=%g  ratio=%.2fx\n"
    r.cell.bench r.cell.name r.baseline.estimate r.cell.estimate r.ratio

let print_missing (bench, name) =
  Printf.printf "missing-in-current: %s/%s\n" bench name

let () =
  let baseline_path = ref "" in
  let bench_dir = ref "" in
  Arg.parse
    [
      ("--baseline", Arg.Set_string baseline_path, "path");
      ("--bench-dir", Arg.Set_string bench_dir, "path");
    ]
    (fun _ -> ())
    "bench_compare --baseline <path> --bench-dir <path>";
  let baseline =
    match Bench_emit.load_baseline ~path:!baseline_path with
    | Ok cells -> cells
    | Error msg ->
        prerr_endline ("bench_compare: " ^ msg);
        exit 2
  in
  let current =
    match Bench_emit.read_bench_dir ~path:!bench_dir with
    | Ok cells -> cells
    | Error msg ->
        prerr_endline ("bench_compare: " ^ msg);
        exit 2
  in
  match Bench_emit.validate_coverage ~baseline ~current with
  | Error missing ->
      List.iter print_missing missing;
      exit 1
  | Ok pair -> (
      match Bench_emit.regress ~threshold:0.20 pair with
      | Ok_no_regression ->
          print_endline "bench-compare: no regressions";
          exit 0
      | Regressions rs ->
          List.iter print_regression rs;
          exit 1)
