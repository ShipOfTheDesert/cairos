(* Run with: opam exec -- dune exec bench/bench_dropna.exe

   Benchmark: Cairos.Series.dropna (two-pass mask-and-gather) on a
   10k-element daily float64 series with a 10% NaN fraction.

   Prerequisite: this file is only built when cairos's :with-test deps are
   installed (bechamel + bechamel-notty). Run
     opam install --deps-only --with-test .
   after a fresh clone. *)

open Bechamel
open Toolkit

let n = 10_000
let nan_fraction = 0.10
let seed = 0xCA1_5EED

let make_input () =
  let idx = Bench_fixtures.make_index ~length:n () in
  let rng = Random.State.make [| seed |] in
  let values =
    Nx.create Nx.float64 [| n |]
      (Array.init n (fun i ->
           if Random.State.float rng 1.0 < nan_fraction then Float.nan
           else Float.of_int (((i * 7) + 13) mod 1000) /. 100.0))
  in
  Bench_fixtures.make_series idx values

(* Setup is hoisted out of [Staged.stage] so the hot loop only measures
   the operation under test. Every measured iteration reuses the same input. *)
let test_dropna =
  let s = make_input () in
  Test.make ~name:"dropna / 10k daily float64, 10% NaN"
    (Staged.stage (fun () -> ignore (Cairos.Series.dropna s)))

let benchmark () =
  let instances =
    Instance.[ monotonic_clock; minor_allocated; major_allocated ]
  in
  let cfg =
    Benchmark.cfg ~limit:3000 ~quota:(Time.second 2.0) ~stabilize:true ()
  in
  Benchmark.all cfg instances test_dropna

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
      Bench_emit.to_channel stdout ~bench:"dropna" results
        Instance.[ monotonic_clock; minor_allocated; major_allocated ]
