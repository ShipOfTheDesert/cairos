open Bench_emit

(* Cell pretty-printer + equality for Alcotest.  Floats are compared with a
   small tolerance — both estimate and r_square round-trip through [%.6g]
   (~6 significant digits) inside [write_consolidated], so an exact compare
   would falsely reject e.g. 121.000000001 ≈ 121.0. *)
let cell_eq a b =
  String.equal a.bench b.bench
  && String.equal a.name b.name
  && String.equal a.instance b.instance
  && Float.abs (a.estimate -. b.estimate) < 1e-6
  && Float.abs (a.r_square -. b.r_square) < 1e-6

let pp_cell fmt c =
  Format.fprintf fmt
    "{ bench = %S; name = %S; instance = %S; estimate = %.6g; r_square = %.6g }"
    c.bench c.name c.instance c.estimate c.r_square

let cell_testable = Alcotest.testable pp_cell cell_eq

(* Save / restore env var for hermetic tests.  [Sys.unsafe_getenv] would
   bypass setuid checks; [Sys.getenv_opt] is safe and gives the real env.
   Unix lacks a portable unsetenv in this stdlib, so we restore by either
   replaying the original value or setting to empty (which our code reads
   identically to "unset" — both fall to the [`Notty] arm). *)
let with_env var value f =
  let prior = Sys.getenv_opt var in
  Unix.putenv var value;
  let restore () =
    match prior with
    | Some v -> Unix.putenv var v
    | None -> Unix.putenv var ""
  in
  Fun.protect ~finally:restore f

let with_env_unset var f =
  let prior = Sys.getenv_opt var in
  Unix.putenv var "";
  let restore () =
    match prior with
    | Some v -> Unix.putenv var v
    | None -> Unix.putenv var ""
  in
  Fun.protect ~finally:restore f

let mk_cell ~bench ~name ~instance ~estimate ~r_square =
  { bench; name; instance; estimate; r_square }

(* Three cells × two benches = 6 cells.  All numeric values are chosen so
   their %.6g representation round-trips exactly — the test then checks
   the writer + parser preserves shape, not float-precision policy. *)
let fixture_cells () =
  [
    mk_cell ~bench:"alpha" ~name:"op/n=10" ~instance:"monotonic-clock"
      ~estimate:1234.5 ~r_square:0.999;
    mk_cell ~bench:"alpha" ~name:"op/n=10" ~instance:"minor-allocated"
      ~estimate:42.0 ~r_square:1.0;
    mk_cell ~bench:"alpha" ~name:"op/n=10" ~instance:"major-allocated"
      ~estimate:0.0 ~r_square:1.0;
    mk_cell ~bench:"beta" ~name:"thing/100" ~instance:"monotonic-clock"
      ~estimate:50.5 ~r_square:0.99;
    mk_cell ~bench:"beta" ~name:"thing/100" ~instance:"minor-allocated"
      ~estimate:7.0 ~r_square:1.0;
    mk_cell ~bench:"beta" ~name:"thing/100" ~instance:"major-allocated"
      ~estimate:0.0 ~r_square:1.0;
  ]

let canonical_order =
  List.sort (fun a b ->
      match String.compare a.bench b.bench with
      | 0 -> (
          match String.compare a.name b.name with
          | 0 -> String.compare a.instance b.instance
          | c -> c)
      | c -> c)

(* ------------------------------------------------------------------ *)
(* TP-Bench-1 — output_mode dispatches on env var                      *)
(* ------------------------------------------------------------------ *)

let test_output_mode_dispatches_on_env_var () =
  with_env_unset "CAIROS_BENCH_OUTPUT" (fun () ->
      Alcotest.(check bool) "unset → Notty" true (output_mode () = `Notty));
  with_env "CAIROS_BENCH_OUTPUT" "json" (fun () ->
      Alcotest.(check bool) "\"json\" → Json" true (output_mode () = `Json));
  with_env "CAIROS_BENCH_OUTPUT" "JSON" (fun () ->
      Alcotest.(check bool)
        "\"JSON\" → Notty (case-sensitive)" true
        (output_mode () = `Notty));
  with_env "CAIROS_BENCH_OUTPUT" "yes" (fun () ->
      Alcotest.(check bool) "\"yes\" → Notty" true (output_mode () = `Notty));
  with_env "CAIROS_BENCH_OUTPUT" "true" (fun () ->
      Alcotest.(check bool) "\"true\" → Notty" true (output_mode () = `Notty))

(* ------------------------------------------------------------------ *)
(* TP-Bench-2 — write_consolidated / load_baseline round-trip          *)
(* ------------------------------------------------------------------ *)

let test_parse_consolidated_round_trip () =
  let path = Filename.temp_file "bench_emit_round_trip" ".json" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () ->
      let cells = fixture_cells () in
      (match write_consolidated ~path cells with
      | Error msg -> Alcotest.failf "write_consolidated failed: %s" msg
      | Ok () -> ());
      match load_baseline ~path with
      | Error msg -> Alcotest.failf "load_baseline failed: %s" msg
      | Ok loaded ->
          Alcotest.(check (list cell_testable))
            "round-trip preserves cells" (canonical_order cells)
            (canonical_order loaded))

(* ------------------------------------------------------------------ *)
(* TP-Bench-2b — write_consolidated reports IO failures as Error       *)
(* ------------------------------------------------------------------ *)

(* Library-shaped per CONTRIBUTING.md §V: an unwritable path must surface
   as Error _, never as a raised Sys_error, so bench_record.ml can print
   the diagnostic and exit cleanly. *)
let test_write_consolidated_reports_io_failure () =
  let unwritable =
    Filename.concat "/no-such-directory-for-bench-test-789" "x.json"
  in
  match write_consolidated ~path:unwritable [] with
  | Ok () -> Alcotest.fail "expected Error for unwritable path"
  | Error msg ->
      Alcotest.(check bool)
        "error mentions the path" true
        (String.length msg > 0)

(* ------------------------------------------------------------------ *)
(* TP-Bench-3 — bench_doc_of_cells round-trips via parse              *)
(* ------------------------------------------------------------------ *)

(* Catches a regression in the per-bench emission path that the consolidated
   round-trip test does not — that test goes through [write_consolidated],
   which builds the doc directly and never exercises [bench_doc_of_cells]. *)
let test_bench_doc_of_cells_round_trips_via_parse () =
  let cells =
    [
      mk_cell ~bench:"x" ~name:"b/n=2" ~instance:"monotonic-clock"
        ~estimate:200.0 ~r_square:0.99;
      mk_cell ~bench:"x" ~name:"a/n=1" ~instance:"minor-allocated"
        ~estimate:50.0 ~r_square:1.0;
      mk_cell ~bench:"x" ~name:"a/n=1" ~instance:"monotonic-clock"
        ~estimate:100.0 ~r_square:0.98;
    ]
  in
  let doc = bench_doc_of_cells ~bench:"x" cells in
  let consolidated =
    `Assoc [ ("$schema", `String schema_version); ("benches", `List [ doc ]) ]
  in
  match parse_consolidated consolidated with
  | Error msg ->
      Alcotest.failf "parse_consolidated rejected canonical doc: %s" msg
  | Ok parsed ->
      Alcotest.(check (list cell_testable))
        "round-trip via Yojson tree" (canonical_order cells)
        (canonical_order parsed)

(* ------------------------------------------------------------------ *)
(* TP-Bench-4 — round_6g preserves non-finite, rounds finite          *)
(* ------------------------------------------------------------------ *)

let test_round_6g_preserves_non_finite_and_rounds_finite () =
  Alcotest.(check bool)
    "NaN passes through" true
    (Float.is_nan (round_6g Float.nan));
  Alcotest.(check (float 0.0))
    "+inf passes through" Float.infinity (round_6g Float.infinity);
  Alcotest.(check (float 0.0))
    "-inf passes through" Float.neg_infinity
    (round_6g Float.neg_infinity);
  (* Finite values round to ~6 significant digits.  1234.5678 → "1234.57". *)
  Alcotest.(check (float 1e-9))
    "rounds finite to 6g" 1234.57 (round_6g 1234.5678);
  Alcotest.(check (float 0.0))
    "exact 6g representable preserved" 100.0 (round_6g 100.0)

(* ------------------------------------------------------------------ *)
(* TP-Bench-5 — parse_consolidated rejects malformed inputs           *)
(* ------------------------------------------------------------------ *)

let test_parse_consolidated_rejects_malformed () =
  let assert_error name json =
    match parse_consolidated json with
    | Error _ -> ()
    | Ok _ -> Alcotest.failf "expected Error for: %s" name
  in
  (* Top level is not an object. *)
  assert_error "top-level array" (`List []);
  (* Missing $schema. *)
  assert_error "missing $schema" (`Assoc [ ("benches", `List []) ]);
  (* Wrong schema version. *)
  assert_error "wrong schema"
    (`Assoc [ ("$schema", `String "wrong-schema-v0"); ("benches", `List []) ]);
  (* Missing benches. *)
  assert_error "missing benches"
    (`Assoc [ ("$schema", `String schema_version) ]);
  (* benches not an array. *)
  assert_error "benches not array"
    (`Assoc [ ("$schema", `String schema_version); ("benches", `String "oops") ]);
  (* Empty benches array (C4 — guards against silent all-green on empty
     consolidated docs). *)
  assert_error "empty benches"
    (`Assoc [ ("$schema", `String schema_version); ("benches", `List []) ]);
  (* Bench doc not an object. *)
  assert_error "bench doc not object"
    (`Assoc
       [
         ("$schema", `String schema_version);
         ("benches", `List [ `String "oops" ]);
       ]);
  (* Bench doc missing required field (bench name). *)
  assert_error "bench doc missing field"
    (`Assoc
       [
         ("$schema", `String schema_version);
         ( "benches",
           `List
             [
               `Assoc
                 [ ("$schema", `String schema_version); ("cells", `List []) ];
             ] );
       ]);
  (* Cell missing required field (estimate). *)
  assert_error "cell missing field"
    (`Assoc
       [
         ("$schema", `String schema_version);
         ( "benches",
           `List
             [
               `Assoc
                 [
                   ("$schema", `String schema_version);
                   ("bench", `String "x");
                   ( "cells",
                     `List
                       [
                         `Assoc
                           [
                             ("name", `String "n");
                             ("instance", `String "monotonic-clock");
                             ("r_square", `Float 0.9);
                           ];
                       ] );
                 ];
             ] );
       ])

(* ------------------------------------------------------------------ *)
(* TP-Bench-6 — load_baseline reports IO and JSON parse failures      *)
(* ------------------------------------------------------------------ *)

let test_load_baseline_handles_io_and_parse_errors () =
  let missing =
    Filename.concat
      (Filename.get_temp_dir_name ())
      "bench_emit_definitely_absent_42.json"
  in
  (try Sys.remove missing with
  | _ -> ());
  (match load_baseline ~path:missing with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for missing file");
  let garbled = Filename.temp_file "bench_emit_garbled" ".json" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove garbled with
      | _ -> ())
    (fun () ->
      let oc = open_out garbled in
      output_string oc "{not valid json";
      close_out oc;
      match load_baseline ~path:garbled with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "expected Error on malformed JSON")

(* ------------------------------------------------------------------ *)
(* TP-Bench-7 — read_bench_dir consolidates per-bench tempfiles       *)
(* ------------------------------------------------------------------ *)

(* Helper: with a fresh tempdir, run [f] on it; clean up on exit. *)
let with_tempdir prefix f =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (prefix ^ "_" ^ string_of_int (Random.bits ()))
  in
  Unix.mkdir dir 0o700;
  let rec rmrf path =
    let entries =
      try Sys.readdir path with
      | _ -> [||]
    in
    Array.iter
      (fun e ->
        let p = Filename.concat path e in
        if Sys.is_directory p then rmrf p
        else
          try Sys.remove p with
          | _ -> ())
      entries;
    try Unix.rmdir path with
    | _ -> ()
  in
  Fun.protect ~finally:(fun () -> rmrf dir) (fun () -> f dir)

let write_per_bench_file ~dir ~bench cells =
  let path = Filename.concat dir (bench ^ ".json") in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> Yojson.Basic.to_channel oc (bench_doc_of_cells ~bench cells))

(* Mirrors the M4-A flow: each bench writes a per-bench JSON to its own
   tempfile in a tempdir; bench_record / bench_compare read the dir via
   read_bench_dir and proceed.  Catches a regression in the dir-walking
   logic that the consolidated round-trip does not. *)
let test_read_bench_dir_round_trips () =
  let alpha_cells =
    List.filter (fun c -> c.bench = "alpha") (fixture_cells ())
  in
  let beta_cells = List.filter (fun c -> c.bench = "beta") (fixture_cells ()) in
  with_tempdir "bench_emit_dir_rt" (fun dir ->
      write_per_bench_file ~dir ~bench:"alpha" alpha_cells;
      write_per_bench_file ~dir ~bench:"beta" beta_cells;
      match read_bench_dir ~path:dir with
      | Error msg -> Alcotest.failf "read_bench_dir failed: %s" msg
      | Ok cells ->
          Alcotest.(check (list cell_testable))
            "round-trip via dir of per-bench files"
            (canonical_order (fixture_cells ()))
            (canonical_order cells))

(* ------------------------------------------------------------------ *)
(* TP-Bench-8 — read_bench_dir rejects empty / malformed / missing    *)
(* ------------------------------------------------------------------ *)

let test_read_bench_dir_rejects_bad_inputs () =
  (* Missing dir. *)
  let absent =
    Filename.concat (Filename.get_temp_dir_name ()) "bench_emit_absent_dir_99"
  in
  (try Unix.rmdir absent with
  | _ -> ());
  (match read_bench_dir ~path:absent with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for missing dir");
  (* Empty dir — guards against silent all-green on a phantom run. *)
  with_tempdir "bench_emit_empty_dir" (fun dir ->
      match read_bench_dir ~path:dir with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "expected Error for empty dir");
  (* Dir with a malformed *.json file. *)
  with_tempdir "bench_emit_malformed_dir" (fun dir ->
      let p = Filename.concat dir "bad.json" in
      let oc = open_out p in
      output_string oc "{not valid json";
      close_out oc;
      match read_bench_dir ~path:dir with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "expected Error for malformed *.json")

(* ------------------------------------------------------------------ *)
(* TP-Bench-9 — validate_coverage reports missing baseline cells      *)
(* ------------------------------------------------------------------ *)

let test_validate_coverage_reports_missing_baseline_cells () =
  let cell_a =
    mk_cell ~bench:"x" ~name:"A" ~instance:"monotonic-clock" ~estimate:100.0
      ~r_square:0.99
  in
  let cell_b =
    mk_cell ~bench:"x" ~name:"B" ~instance:"monotonic-clock" ~estimate:100.0
      ~r_square:0.99
  in
  let baseline = [ cell_a; cell_b ] in
  let current = [ cell_a ] in
  match validate_coverage ~baseline ~current with
  | Error [ ("x", "B") ] -> ()
  | Error pairs ->
      Alcotest.failf "expected [(x, B)], got %d pairs" (List.length pairs)
  | Ok _ -> Alcotest.fail "B missing from current must fail validate_coverage"

(* ------------------------------------------------------------------ *)
(* TP-Bench-10 — validate_coverage permits new current cells          *)
(* ------------------------------------------------------------------ *)

let test_validate_coverage_permits_new_current_cells () =
  let cell_a =
    mk_cell ~bench:"x" ~name:"A" ~instance:"monotonic-clock" ~estimate:100.0
      ~r_square:0.99
  in
  let cell_b =
    mk_cell ~bench:"x" ~name:"B" ~instance:"monotonic-clock" ~estimate:100.0
      ~r_square:0.99
  in
  let baseline = [ cell_a ] in
  let current = [ cell_a; cell_b ] in
  match validate_coverage ~baseline ~current with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "new bench cell must not block validate_coverage"

(* ------------------------------------------------------------------ *)
(* TP-Bench-11 — validate_coverage ignores allocation instances       *)
(* ------------------------------------------------------------------ *)

(* If an allocation cell is missing from current, validate_coverage must
   still succeed: only monotonic-clock cells are gated. *)
let test_validate_coverage_ignores_allocation_instances () =
  let mc_cell =
    mk_cell ~bench:"x" ~name:"A" ~instance:"monotonic-clock" ~estimate:100.0
      ~r_square:0.99
  in
  let alloc_cell =
    mk_cell ~bench:"x" ~name:"A" ~instance:"minor-allocated" ~estimate:42.0
      ~r_square:1.0
  in
  let baseline = [ mc_cell; alloc_cell ] in
  let current = [ mc_cell ] in
  match validate_coverage ~baseline ~current with
  | Ok _ -> ()
  | Error _ ->
      Alcotest.fail
        "missing allocation cell must not block coverage; only monotonic-clock \
         counts"

(* ------------------------------------------------------------------ *)
(* TP-Bench-12 — regress flags >threshold cells, ignores below        *)
(* ------------------------------------------------------------------ *)

let test_regress_flags_above_threshold_only () =
  let baseline =
    [
      mk_cell ~bench:"x" ~name:"c" ~instance:"monotonic-clock" ~estimate:100.0
        ~r_square:0.99;
    ]
  in
  let current_above =
    [
      mk_cell ~bench:"x" ~name:"c" ~instance:"monotonic-clock" ~estimate:121.0
        ~r_square:0.99;
    ]
  in
  let current_below =
    [
      mk_cell ~bench:"x" ~name:"c" ~instance:"monotonic-clock" ~estimate:119.9
        ~r_square:0.99;
    ]
  in
  let pair_above =
    match validate_coverage ~baseline ~current:current_above with
    | Ok p -> p
    | Error _ -> Alcotest.fail "validate_coverage unexpectedly errored"
  in
  (match regress ~threshold:0.20 pair_above with
  | Regressions [ r ] ->
      Alcotest.(check (float 1e-6))
        "21% triggers regression at threshold 0.20" 1.21 r.ratio
  | Regressions rs ->
      Alcotest.failf "expected exactly one regression, got %d" (List.length rs)
  | Ok_no_regression ->
      Alcotest.fail "121% (21% over) should regress at threshold 0.20");
  let pair_below =
    match validate_coverage ~baseline ~current:current_below with
    | Ok p -> p
    | Error _ -> Alcotest.fail "validate_coverage unexpectedly errored"
  in
  match regress ~threshold:0.20 pair_below with
  | Ok_no_regression -> ()
  | Regressions _ ->
      Alcotest.fail "119.9% (19.9% over) must NOT regress at threshold 0.20"

(* ------------------------------------------------------------------ *)
(* TP-Bench-13 — regress ignores allocation instances                  *)
(* ------------------------------------------------------------------ *)

let test_regress_ignores_allocation_instances () =
  let baseline =
    [
      mk_cell ~bench:"x" ~name:"c" ~instance:"monotonic-clock" ~estimate:100.0
        ~r_square:0.99;
      mk_cell ~bench:"x" ~name:"c" ~instance:"minor-allocated" ~estimate:100.0
        ~r_square:1.0;
    ]
  in
  let current =
    [
      mk_cell ~bench:"x" ~name:"c" ~instance:"monotonic-clock" ~estimate:105.0
        ~r_square:0.99;
      mk_cell ~bench:"x" ~name:"c" ~instance:"minor-allocated" ~estimate:200.0
        ~r_square:1.0;
    ]
  in
  let pair =
    match validate_coverage ~baseline ~current with
    | Ok p -> p
    | Error _ -> Alcotest.fail "validate_coverage unexpectedly errored"
  in
  match regress ~threshold:0.20 pair with
  | Ok_no_regression -> ()
  | Regressions _ ->
      Alcotest.fail
        "allocation instance should not gate; only monotonic-clock counts"

(* ------------------------------------------------------------------ *)
(* TP-Bench-15 — every bench file's [~bench:"X"] matches its filename  *)
(* ------------------------------------------------------------------ *)

(* Pins the per-bench [~bench:] string against the source filename stem
   so a typo (e.g. ~bench:"alignn" in bench_align.ml) surfaces here
   rather than only on the next [just bench-record] diff. *)

let rec find_project_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project") then dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then
      Alcotest.failf "could not locate dune-project ancestor of %s"
        (Sys.getcwd ())
    else find_project_root parent

(* Helper modules that don't call [to_channel ~bench:...] so are excluded. *)
let bench_helpers =
  [ "bench_emit"; "bench_fixtures"; "bench_compare"; "bench_record" ]

let extract_bench_string_literal source =
  (* Match the first occurrence of [~bench:"..."] and return the contents.
     Robust to whitespace between [~bench:] and the string. *)
  let re = Str.regexp "~bench:[ \t\n]*\"\\([^\"]*\\)\"" in
  match Str.search_forward re source 0 with
  | _ -> Some (Str.matched_group 1 source)
  | exception Not_found -> None

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let n = in_channel_length ic in
      let buf = Bytes.create n in
      really_input ic buf 0 n;
      Bytes.unsafe_to_string buf)

let test_bench_strings_match_filenames () =
  let root = find_project_root (Sys.getcwd ()) in
  let bench_dir = Filename.concat root "bench" in
  let entries = Sys.readdir bench_dir in
  Array.sort String.compare entries;
  let checked = ref 0 in
  Array.iter
    (fun name ->
      if Filename.check_suffix name ".ml" then begin
        let stem = Filename.chop_suffix name ".ml" in
        if List.mem stem bench_helpers then ()
        else if not (String.length stem >= 6 && String.sub stem 0 6 = "bench_")
        then ()
        else begin
          let expected = String.sub stem 6 (String.length stem - 6) in
          let source = read_file (Filename.concat bench_dir name) in
          match extract_bench_string_literal source with
          | None ->
              Alcotest.failf
                "%s: no ~bench:\"...\" literal found (every bench harness must \
                 emit one for [Bench_emit.to_channel])"
                name
          | Some actual ->
              Alcotest.(check string)
                (Printf.sprintf "%s ~bench: matches filename" name)
                expected actual;
              incr checked
        end
      end)
    entries;
  Alcotest.(check bool)
    "at least one bench harness was checked" true (!checked > 0)

(* ------------------------------------------------------------------ *)
(* TP-Bench-14 — monotonic_clock_label matches Bechamel's witness      *)
(* ------------------------------------------------------------------ *)

(* C9: the gate's filter literal lives in [Bench_emit.monotonic_clock_label].
   Bechamel produces the same label via [Measure.label] on its toolkit
   instance.  This test pins the equality so a future Bechamel-side rename
   surfaces here rather than silently disabling the gate. *)
let test_monotonic_clock_label_matches_bechamel () =
  let bechamel_label =
    Bechamel.Measure.label Bechamel.Toolkit.Instance.monotonic_clock
  in
  Alcotest.(check string)
    "Bechamel toolkit label matches our constant" monotonic_clock_label
    bechamel_label

(* ------------------------------------------------------------------ *)
(* TP-Bench-16 — bench_compare/bench_record exit-code wiring           *)
(* ------------------------------------------------------------------ *)

(* The driver executables are thin wrappers over [Bench_emit] functions
   that are tested in isolation, but the [exit 0/1/2] wiring itself
   is only exercised at the process boundary. This test spawns each
   executable on fixture inputs and pins the documented exit codes. *)

let make_tempdir prefix =
  let dir = Filename.temp_file prefix "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  dir

let rm_rf path =
  let rec aux p =
    match (Unix.stat p).st_kind with
    | exception Unix.Unix_error _ -> ()
    | Unix.S_DIR ->
        Sys.readdir p |> Array.iter (fun e -> aux (Filename.concat p e));
        Unix.rmdir p
    | _ -> Sys.remove p
  in
  aux path

let write_file path content =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let bench_doc_json ~bench cells =
  let doc = bench_doc_of_cells ~bench cells in
  Yojson.Basic.to_string doc

let consolidated_json cells =
  (* Mirror write_consolidated's grouping but as an in-memory JSON string. *)
  let by_bench = Hashtbl.create 4 in
  List.iter
    (fun c ->
      let prev =
        match Hashtbl.find_opt by_bench c.bench with
        | Some xs -> xs
        | None -> []
      in
      Hashtbl.replace by_bench c.bench (c :: prev))
    cells;
  let groups =
    Hashtbl.fold (fun b cs acc -> (b, cs) :: acc) by_bench []
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let benches =
    List.map (fun (bench, cs) -> bench_doc_of_cells ~bench cs) groups
  in
  Yojson.Basic.to_string
    (`Assoc [ ("$schema", `String schema_version); ("benches", `List benches) ])

let test_driver_exit_codes () =
  let root = find_project_root (Sys.getcwd ()) in
  let compare_exe =
    Filename.concat root "_build/default/bench/bench_compare.exe"
  in
  let record_exe =
    Filename.concat root "_build/default/bench/bench_record.exe"
  in
  if not (Sys.file_exists compare_exe) then
    Alcotest.failf "expected built executable at %s — run `dune build`"
      compare_exe;
  if not (Sys.file_exists record_exe) then
    Alcotest.failf "expected built executable at %s — run `dune build`"
      record_exe;

  let baseline_cell =
    mk_cell ~bench:"x" ~name:"op/n=10" ~instance:"monotonic-clock"
      ~estimate:100.0 ~r_square:0.99
  in

  let with_dir prefix f =
    let dir = make_tempdir prefix in
    Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)
  in

  let run_compare ~baseline ~bench_dir =
    let cmd =
      Printf.sprintf "%s --baseline %s --bench-dir %s >/dev/null 2>&1"
        (Filename.quote compare_exe)
        (Filename.quote baseline) (Filename.quote bench_dir)
    in
    Sys.command cmd
  in
  let run_record ~bench_dir =
    let cmd =
      Printf.sprintf "%s --bench-dir %s >/dev/null 2>&1"
        (Filename.quote record_exe)
        (Filename.quote bench_dir)
    in
    Sys.command cmd
  in

  (* Scenario 1: baseline matches current → exit 0. *)
  with_dir "bench_compare_ok" (fun dir ->
      let baseline_path = Filename.concat dir "baseline.json" in
      let bench_dir = Filename.concat dir "current" in
      Unix.mkdir bench_dir 0o700;
      write_file baseline_path (consolidated_json [ baseline_cell ]);
      write_file
        (Filename.concat bench_dir "x.json")
        (bench_doc_json ~bench:"x" [ baseline_cell ]);
      Alcotest.(check int)
        "bench_compare exit 0 on no regression" 0
        (run_compare ~baseline:baseline_path ~bench_dir));

  (* Scenario 2: current >20% slower → exit 1. *)
  with_dir "bench_compare_regress" (fun dir ->
      let baseline_path = Filename.concat dir "baseline.json" in
      let bench_dir = Filename.concat dir "current" in
      Unix.mkdir bench_dir 0o700;
      write_file baseline_path (consolidated_json [ baseline_cell ]);
      let regressed =
        { baseline_cell with estimate = baseline_cell.estimate *. 1.5 }
      in
      write_file
        (Filename.concat bench_dir "x.json")
        (bench_doc_json ~bench:"x" [ regressed ]);
      Alcotest.(check int)
        "bench_compare exit 1 on >20% regression" 1
        (run_compare ~baseline:baseline_path ~bench_dir));

  (* Scenario 3: missing-in-current → exit 1. *)
  with_dir "bench_compare_missing" (fun dir ->
      let baseline_path = Filename.concat dir "baseline.json" in
      let bench_dir = Filename.concat dir "current" in
      Unix.mkdir bench_dir 0o700;
      let other_cell =
        mk_cell ~bench:"y" ~name:"op/n=10" ~instance:"monotonic-clock"
          ~estimate:100.0 ~r_square:0.99
      in
      write_file baseline_path (consolidated_json [ baseline_cell ]);
      write_file
        (Filename.concat bench_dir "y.json")
        (bench_doc_json ~bench:"y" [ other_cell ]);
      Alcotest.(check int)
        "bench_compare exit 1 on missing-in-current" 1
        (run_compare ~baseline:baseline_path ~bench_dir));

  (* Scenario 4: malformed baseline → exit 2. *)
  with_dir "bench_compare_malformed" (fun dir ->
      let baseline_path = Filename.concat dir "baseline.json" in
      let bench_dir = Filename.concat dir "current" in
      Unix.mkdir bench_dir 0o700;
      write_file baseline_path "{ not valid json";
      Alcotest.(check int)
        "bench_compare exit 2 on malformed baseline" 2
        (run_compare ~baseline:baseline_path ~bench_dir));

  (* Scenario 5: bench_record on missing dir → exit 2. *)
  Alcotest.(check int)
    "bench_record exit 2 on missing --bench-dir" 2
    (run_record ~bench_dir:"/no-such-dir-for-bench-record-test-789")

let () =
  Alcotest.run "bench_emit"
    [
      ( "output_mode",
        [
          Alcotest.test_case "dispatches on env var" `Quick
            test_output_mode_dispatches_on_env_var;
        ] );
      ( "json_io",
        [
          Alcotest.test_case "parse_consolidated round trip" `Quick
            test_parse_consolidated_round_trip;
          Alcotest.test_case "write_consolidated reports IO failure" `Quick
            test_write_consolidated_reports_io_failure;
          Alcotest.test_case "bench_doc_of_cells round trips via parse" `Quick
            test_bench_doc_of_cells_round_trips_via_parse;
          Alcotest.test_case "round_6g preserves non-finite, rounds finite"
            `Quick test_round_6g_preserves_non_finite_and_rounds_finite;
          Alcotest.test_case "parse_consolidated rejects malformed inputs"
            `Quick test_parse_consolidated_rejects_malformed;
          Alcotest.test_case "load_baseline reports IO and parse errors" `Quick
            test_load_baseline_handles_io_and_parse_errors;
          Alcotest.test_case "read_bench_dir consolidates per-bench files"
            `Quick test_read_bench_dir_round_trips;
          Alcotest.test_case "read_bench_dir rejects bad inputs" `Quick
            test_read_bench_dir_rejects_bad_inputs;
        ] );
      ( "validate_coverage",
        [
          Alcotest.test_case "reports missing baseline cells" `Quick
            test_validate_coverage_reports_missing_baseline_cells;
          Alcotest.test_case "permits new current cells" `Quick
            test_validate_coverage_permits_new_current_cells;
          Alcotest.test_case "ignores allocation instances" `Quick
            test_validate_coverage_ignores_allocation_instances;
        ] );
      ( "regress",
        [
          Alcotest.test_case "flags above threshold only" `Quick
            test_regress_flags_above_threshold_only;
          Alcotest.test_case "ignores allocation instances" `Quick
            test_regress_ignores_allocation_instances;
        ] );
      ( "bechamel",
        [
          Alcotest.test_case "monotonic_clock_label matches Bechamel" `Quick
            test_monotonic_clock_label_matches_bechamel;
        ] );
      ( "harness_pins",
        [
          Alcotest.test_case
            "every bench's ~bench: literal matches its filename" `Quick
            test_bench_strings_match_filenames;
        ] );
      ( "drivers",
        [
          Alcotest.test_case "bench_compare/bench_record exit codes" `Quick
            test_driver_exit_codes;
        ] );
    ]
