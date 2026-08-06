(* Run with: opam exec -- dune exec bench/bench_frame_columns.exe

   Benchmark: Cairos.Frame column operations [add_column], [drop], and [select]
   at C in {100, 500} on a 1000-bar daily frame — the same two fixture shapes
   bench_frame_xsec uses, so the two files' numbers are comparable and the
   ~500-instrument boundary frame.mli documents is the upper point of both.
   Six cells, one Bechamel instance.

   These three operations are row-independent: each rebuilds the column list
   and reuses the input's index and value tensors verbatim, so the cost is a
   function of C alone and the 1000 bars only fix the fixture. That is the
   thing worth watching — [add_column]'s duplicate-name scan is quadratic in C
   and [select]'s membership test is C * |request|, so a cell that starts
   tracking the bar count would mean an operation began copying rows.

   Separate from bench_frame_xsec rather than a fourth group inside it: that
   file runs at a 12s quota for the OLS-fit reason documented at its cfg, and
   six sub-millisecond cells do not need 72s of wall time to fit. 2s is the
   quota every other bench of this scale in this directory uses.

   Wall clock only — the two allocation instances the other ten benches in
   this directory also emit are deliberately absent. Measured on this workload
   at the 2s quota below, [monotonic-clock] fits every cell at r2 >= 0.98,
   while [minor-allocated] gives r2 = NaN at add_column/n=500 (zero estimate,
   no variance to fit) and 0.025 at add_column/n=100, and [major-allocated]
   gives 0.24 at select/n=500. Raising the quota to 8s clears the NaN but
   still leaves three allocation cells below 0.9 (0.89, 0.59, 0.58), so the
   shortfall is the metric and not the sample count: these operations rebuild
   a small list of pointers, and the allocation counters do not resolve that
   against the surrounding traffic. An r2 below ~0.9 is a measurement defect
   rather than a noisy-but-usable number and must not enter the committed
   baseline; a NaN r2 is worse, being unrepresentable in standard JSON, so
   emitting it would abort the whole just bench-compare run rather than only
   this bench. Wall clock is also the only instance the regression gate reads
   (bench_emit.mli, monotonic_clock_label).

   Prerequisite: this file is only built when cairos's :with-test deps are
   installed (bechamel + bechamel-notty). Run
     opam install --deps-only --with-test .
   after a fresh clone. *)

open Bechamel
open Toolkit

let bars = 1000
let column_sizes = [ 100; 500 ]
let instances = Instance.[ monotonic_clock ]

(* The added column carries the frame's own index rather than an equal rebuilt
   one, which is what a caller deriving a column from a frame has in hand.
   [add_column] compares indices structurally either way, so this measures the
   full timestamp walk, not a physical-equality shortcut it does not take. *)
let make_case n =
  let frame = Bench_fixtures.make_frame ~bars ~columns:n in
  let addition =
    Bench_fixtures.make_series (Cairos.Frame.index frame)
      (Bench_fixtures.make_values ~length:bars)
  in
  (* Half the columns, every other one, so [select] neither degenerates to
     selecting everything nor to a single-name lookup. Names follow
     [Bench_fixtures.make_frame]'s [c0..c{n-1}] scheme. *)
  let requested =
    List.filter_map
      (fun i -> if i mod 2 = 0 then Some (Printf.sprintf "c%d" i) else None)
      (List.init n Fun.id)
  in
  let names =
    match Cairos.Nonempty.of_list requested with
    | Some ne -> ne
    | None ->
        failwith (Printf.sprintf "bench input: empty select request for n=%d" n)
  in
  (frame, addition, names)

let case_table = List.map (fun n -> (n, make_case n)) column_sizes

let case_for n =
  match List.assoc_opt n case_table with
  | Some c -> c
  | None -> failwith (Printf.sprintf "bench input: no frame for n=%d" n)

(* Setup is hoisted out of [Staged.stage]; the staged closures capture the
   pre-built frame, series, and request so the measured loop runs only the
   column operation. *)
let test_add_column =
  Test.make_indexed ~name:"add_column/n" ~fmt:"%s=%d" ~args:column_sizes
    (fun n ->
      let frame, addition, _ = case_for n in
      Staged.stage (fun () ->
          ignore (Cairos.Frame.add_column "added" addition frame)))

(* [c0] is a name the frame actually carries, so this measures a real removal
   rather than the absent-name no-op, which rebuilds a full-length list instead
   of an [n-1] one. Which name is dropped does not change the cost: the filter
   traverses every column either way. *)
let test_drop =
  Test.make_indexed ~name:"drop/n" ~fmt:"%s=%d" ~args:column_sizes (fun n ->
      let frame, _, _ = case_for n in
      Staged.stage (fun () -> ignore (Cairos.Frame.drop "c0" frame)))

let test_select =
  Test.make_indexed ~name:"select/n" ~fmt:"%s=%d" ~args:column_sizes (fun n ->
      let frame, _, names = case_for n in
      Staged.stage (fun () -> ignore (Cairos.Frame.select names frame)))

let test_frame_columns =
  Test.make_grouped ~name:"frame_columns"
    [ test_add_column; test_drop; test_select ]

let benchmark () =
  let cfg =
    Benchmark.cfg ~limit:3000 ~quota:(Time.second 2.0) ~stabilize:true ()
  in
  Benchmark.all cfg instances test_frame_columns

let analyze raw_results =
  let ols =
    Analyze.ols ~r_square:true ~bootstrap:0 ~predictors:[| Measure.run |]
  in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  Analyze.merge ols instances results

let render_notty results =
  List.iter
    (fun instance -> Bechamel_notty.Unit.add instance (Measure.unit instance))
    instances;
  let window =
    match Notty_unix.winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w = 120; h = 1 }
  in
  let image =
    Bechamel_notty.Multiple.image_of_ols_results ~rect:window
      ~predictor:Measure.run results
  in
  Notty_unix.eol image |> Notty_unix.output_image

let () =
  let results = analyze (benchmark ()) in
  match Bench_emit.output_mode () with
  | `Notty -> render_notty results
  | `Json ->
      Bench_emit.to_channel stdout ~bench:"frame_columns" results instances
