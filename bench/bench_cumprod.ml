(* Run with: opam exec -- dune exec bench/bench_cumprod.exe

   Benchmark: Cairos.Series.cumprod (O(n) scan) vs Window.expanding product
   (O(n^2)) on a 10k-element daily float64 series.

   Prerequisite: this file is only built when cairos's :with-test deps are
   installed (bechamel + bechamel-notty). Run
     opam install --deps-only --with-test .
   after a fresh clone. *)

open Bechamel
open Toolkit

let n = 10_000

let make_input () =
  let epoch = 1_704_067_200.0 in
  (* 2024-01-01 UTC *)
  let ts = Array.init n (fun i -> epoch +. (float_of_int i *. 86_400.0)) in
  let idx =
    match Cairos.Index.of_unix_floats Cairos.Freq.Day ts with
    | Ok i -> i
    | Error e -> failwith ("bench input index: " ^ Cairos.Index.err_to_string e)
  in
  (* Synthetic (1 + returns) series: random positive values in [0.99, 1.01] *)
  let values =
    Nx.create Nx.float64 [| n |]
      (Array.init n (fun i ->
           let x = Float.of_int (((i * 7) + 13) mod 1000) /. 50_000.0 in
           1.0 -. 0.01 +. x))
  in
  match Cairos.Series.make idx values with
  | Ok s -> s
  | Error e -> failwith ("bench input series: " ^ e)

(* Setup is hoisted out of [Staged.stage] so the hot loop only measures
   the operation under test. Every measured iteration reuses the same input. *)
let test_cumprod =
  let s = make_input () in
  Test.make ~name:"cumprod / 10k daily float64"
    (Staged.stage (fun () -> ignore (Cairos.Series.cumprod s)))

let test_expanding_product =
  let s = make_input () in
  Test.make ~name:"expanding product / 10k daily float64"
    (Staged.stage (fun () ->
         ignore
           (Cairos.Window.expanding
              (fun w -> Array.fold_left ( *. ) 1.0 (Nx.to_array w))
              s)))

let benchmark () =
  let instances =
    Instance.[ monotonic_clock; minor_allocated; major_allocated ]
  in
  let cfg =
    Benchmark.cfg ~limit:3000 ~quota:(Time.second 2.0) ~stabilize:true ()
  in
  Benchmark.all cfg instances
    (Test.make_grouped ~name:"cumprod vs expanding"
       [ test_cumprod; test_expanding_product ])

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
