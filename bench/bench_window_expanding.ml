(* Run with: opam exec -- dune exec bench/bench_window_expanding.exe

   Benchmark: Cairos.Window.expanding on a 10k-element daily float64 series,
   reducer Nx.max — a representative O(n) expanding reducer.

   This bench is distinct from bench_cumprod.ml's "expanding product" cell,
   which exists to contrast Series.cumprod's O(n) scan with the O(n^2) naive
   expanding-product baseline. This file measures expanding's own per-call
   cost for a typical reducer at the PRD's pinned input size.

   Prerequisite: this file is only built when cairos's :with-test deps are
   installed (bechamel + bechamel-notty). Run
     opam install --deps-only --with-test .
   after a fresh clone. *)

open Bechamel
open Toolkit

let n = 10_000

let make_input () =
  let idx = Bench_fixtures.make_index ~length:n () in
  let values = Bench_fixtures.make_values ~length:n in
  Bench_fixtures.make_series idx values

(* Setup is hoisted out of [Staged.stage] so the hot loop only measures
   [Window.expanding]. The same 10k-point input is reused across iterations. *)
let test_expanding =
  let s = make_input () in
  Test.make ~name:"expanding/max/10k"
    (Staged.stage (fun () ->
         ignore (Cairos.Window.expanding (fun w -> Nx.item [] (Nx.max w)) s)))

let benchmark () =
  let instances =
    Instance.[ monotonic_clock; minor_allocated; major_allocated ]
  in
  let cfg =
    Benchmark.cfg ~limit:3000 ~quota:(Time.second 2.0) ~stabilize:true ()
  in
  Benchmark.all cfg instances test_expanding

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
      Bench_emit.to_channel stdout ~bench:"window_expanding" results
        Instance.[ monotonic_clock; minor_allocated; major_allocated ]
