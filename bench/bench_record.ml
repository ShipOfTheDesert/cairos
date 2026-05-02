(* Run with:
     opam exec -- dune exec bench/bench_record.exe -- --bench-dir /tmp/bench-cur

   Reads a directory of per-bench JSON tempfiles (produced by the
   just bench-record shell loop) and rewrites bench/baseline.json via
   Bench_emit.write_consolidated, normalising ordering and float formatting so
   the committed baseline is diff-stable. *)

let baseline_path = "bench/baseline.json"

let () =
  let bench_dir = ref "" in
  Arg.parse
    [ ("--bench-dir", Arg.Set_string bench_dir, "path") ]
    (fun _ -> ())
    "bench_record --bench-dir <path>";
  match Bench_emit.read_bench_dir ~path:!bench_dir with
  | Error msg ->
      prerr_endline ("bench_record: " ^ msg);
      exit 2
  | Ok cells -> (
      match Bench_emit.write_consolidated ~path:baseline_path cells with
      | Error msg ->
          prerr_endline ("bench_record: " ^ msg);
          exit 2
      | Ok () -> print_endline ("bench-record: wrote " ^ baseline_path))
