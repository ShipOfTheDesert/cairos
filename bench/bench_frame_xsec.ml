(* Run with: opam exec -- dune exec bench/bench_frame_xsec.exe

   Benchmark: Cairos.Frame cross-sectional operations [column_map], [rank], and
   [zscore] at C in {100, 500} on a 1000-bar daily frame (PRD FR-8). Six cells
   total; 18 records across the three Bechamel instances.

   The 1000-bar frame is built once per indexed [n] via
   [Bench_fixtures.make_frame], hoisted outside [Staged.stage] per the
   bechamel-staged-setup-hoisting handbook. The reducer for [column_map] is a
   no-allocation closure ([first +. last]) so the harness measures the
   row-iteration kernel rather than the user-supplied reduction.

   Per RFC 0050 §Options Considered F: one bench file with three named tests
   sharing setup, grouped under "frame_xsec".

   Prerequisite: this file is only built when cairos's :with-test deps are
   installed (bechamel + bechamel-notty). Run
     opam install --deps-only --with-test .
   after a fresh clone. *)

open Bechamel
open Toolkit

let bars = 1000
let column_sizes = [ 100; 500 ]

let frame_table =
  List.map
    (fun n -> (n, Bench_fixtures.make_frame ~bars ~columns:n))
    column_sizes

let frame_for n =
  match List.assoc_opt n frame_table with
  | Some f -> f
  | None -> failwith (Printf.sprintf "bench input: no frame for n=%d" n)

(* Setup is hoisted out of [Staged.stage]; the staged closures capture the
   pre-built frame so the measured loop runs only the cross-sectional op. *)
let test_column_map =
  Test.make_indexed ~name:"column_map/n" ~fmt:"%s=%d" ~args:column_sizes
    (fun n ->
      let frame = frame_for n in
      Staged.stage (fun () ->
          ignore
            (Cairos.Frame.column_map
               ~f:(fun a -> a.(0) +. a.(Array.length a - 1))
               frame)))

let test_rank =
  Test.make_indexed ~name:"rank/n" ~fmt:"%s=%d" ~args:column_sizes (fun n ->
      let frame = frame_for n in
      Staged.stage (fun () -> ignore (Cairos.Frame.rank frame)))

let test_zscore =
  Test.make_indexed ~name:"zscore/n" ~fmt:"%s=%d" ~args:column_sizes (fun n ->
      let frame = frame_for n in
      Staged.stage (fun () -> ignore (Cairos.Frame.zscore frame)))

let test_frame_xsec =
  Test.make_grouped ~name:"frame_xsec"
    [ test_column_map; test_rank; test_zscore ]

let benchmark () =
  let instances =
    Instance.[ monotonic_clock; minor_allocated; major_allocated ]
  in
  (* Quota raised to 12s/cell (vs. the 2s precedent on smaller benches) because
     [rank]/[zscore] at [n=500] on a 1000-bar frame each run ~2.3s — a 2s
     quota collects a single sample, leaving Bechamel's OLS [r_square] as
     NaN, which trips [Yojson.Basic]'s NaN guard in the JSON tail. 12s lets
     OLS fit ≥ 3 samples per cell while keeping the six-cell wall-time
     comfortably under the bench-suite NFR-3 budget. *)
  let cfg =
    Benchmark.cfg ~limit:3000 ~quota:(Time.second 12.0) ~stabilize:true ()
  in
  Benchmark.all cfg instances test_frame_xsec

let analyze raw_results =
  let instances =
    Instance.[ monotonic_clock; minor_allocated; major_allocated ]
  in
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
    Instance.[ monotonic_clock; minor_allocated; major_allocated ];
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
      Bench_emit.to_channel stdout ~bench:"frame_xsec" results
        Instance.[ monotonic_clock; minor_allocated; major_allocated ]
