(* Run with: opam exec -- dune exec bench/bench_series_map.exe

   Bootstrap benchmark: measures Cairos.Series.map applied to a 100k-element
   daily float64 series with a single-op Nx transform.

   Prerequisite: this file is only built when cairos's :with-test deps are
   installed (bechamel + bechamel-notty). Run
     opam install --deps-only --with-test .
   after a fresh clone. *)

open Bechamel
open Toolkit

let n = 100_000

let make_input () =
  let epoch = 1_704_067_200.0 in
  (* 2024-01-01 UTC *)
  let ts = Array.init n (fun i -> epoch +. (float_of_int i *. 86_400.0)) in
  let idx =
    match Cairos.Index.of_unix_floats Cairos.Freq.Day ts with
    | Ok i -> i
    | Error e -> failwith ("bench input index: " ^ e)
  in
  let values = Nx.create Nx.float64 [| n |] (Array.init n float_of_int) in
  match Cairos.Series.make idx values with
  | Ok s -> s
  | Error e -> failwith ("bench input series: " ^ e)

(* Setup is hoisted out of [Staged.stage] so the hot loop only measures
   [Series.map]. Every measured iteration reuses the same input. *)
let test =
  let s = make_input () in
  Test.make ~name:"Series.map (*2 + 1) / 100k daily float64"
    (Staged.stage (fun () ->
         Cairos.Series.map (fun v -> Nx.add_s (Nx.mul_s v 2.0) 1.0) s))

let benchmark () =
  let instances =
    Instance.[ monotonic_clock; minor_allocated; major_allocated ]
  in
  let cfg =
    Benchmark.cfg ~limit:2000 ~quota:(Time.second 1.0) ~stabilize:true ()
  in
  Benchmark.all cfg instances (Test.make_grouped ~name:"cairos" [ test ])

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

let () =
  List.iter
    (fun instance -> Bechamel_notty.Unit.add instance (Measure.unit instance))
    Instance.[ monotonic_clock; minor_allocated; major_allocated ];
  let results = analyze (benchmark ()) in
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
