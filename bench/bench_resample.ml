(* Run with: opam exec -- dune exec bench/bench_resample.exe

   Benchmark: Cairos.Resample.resample daily->weekly and daily->monthly, both
   with [~agg:`Last] on a ~2_500-bar (10-year) daily float64 series. The two
   cells share one input and differ only in the target frequency, isolating the
   calendar-bucketing cost of weekly vs monthly.

   Prerequisite: this file is only built when cairos's :with-test deps are
   installed (bechamel + bechamel-notty). Run
     opam install --deps-only --with-test .
   after a fresh clone. *)

open Bechamel
open Toolkit

let n = 2_500

let make_input () =
  let idx = Bench_fixtures.make_index ~length:n () in
  let values = Bench_fixtures.make_values ~length:n in
  Bench_fixtures.make_series idx values

(* Setup is hoisted out of [Staged.stage] so the hot loop only measures
   [Resample.resample]. The same ~2_500-bar input is shared across both cells
   and reused across iterations; [resample] does not mutate its input. The
   group name "resample" is prefixed onto each cell by [make_grouped], so the
   emitted names remain [resample/daily-to-weekly/2500] and
   [resample/daily-to-monthly/2500]. *)
let test_resample =
  let s = make_input () in
  Test.make_grouped ~name:"resample"
    [
      Test.make ~name:"daily-to-weekly/2500"
        (Staged.stage (fun () ->
             match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Week s with
             | Ok _ -> ()
             | Error _ -> failwith "bench input violates resample contract"));
      Test.make ~name:"daily-to-monthly/2500"
        (Staged.stage (fun () ->
             match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Month s with
             | Ok _ -> ()
             | Error _ -> failwith "bench input violates resample contract"));
    ]

let benchmark () =
  let instances =
    Instance.[ monotonic_clock; minor_allocated; major_allocated ]
  in
  let cfg =
    Benchmark.cfg ~limit:3000 ~quota:(Time.second 2.0) ~stabilize:true ()
  in
  Benchmark.all cfg instances test_resample

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
      Bench_emit.to_channel stdout ~bench:"resample" results
        Instance.[ monotonic_clock; minor_allocated; major_allocated ]
