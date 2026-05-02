(* Run with: opam exec -- dune exec bench/bench_align.exe

   Benchmark: Cairos.Align.align for strategies Inner, Left, Asof Backward at
   n in {100, 1_000, 10_000} on daily float64 series (PRD FR-1 line 3).

   Two input shapes are pre-built once per n:
   - identical-index pair (Left strategy: every left timestamp matches a right
     timestamp, no NaN fill — this is the dominant production case for Left).
   - 1-bar offset pair (Inner and Asof Backward: intersection is non-empty but
     not trivial — n - 1 matches out of n, so neither strategy degenerates to
     "no work" or "full reindex").

   Prerequisite: this file is only built when cairos's :with-test deps are
   installed (bechamel + bechamel-notty). Run
     opam install --deps-only --with-test .
   after a fresh clone. *)

open Bechamel
open Toolkit

let sizes = [ 100; 1_000; 10_000 ]

let make_pair_identical ~n =
  let idx = Bench_fixtures.make_index ~length:n () in
  let vals = Bench_fixtures.make_values ~length:n in
  (Bench_fixtures.make_series idx vals, Bench_fixtures.make_series idx vals)

let make_pair_offset ~n =
  let idx_a = Bench_fixtures.make_index ~length:n () in
  let idx_b = Bench_fixtures.make_index ~start:1 ~length:n () in
  let vals = Bench_fixtures.make_values ~length:n in
  (Bench_fixtures.make_series idx_a vals, Bench_fixtures.make_series idx_b vals)

(* Pre-build inputs once per n; the staged closures capture the pair so the
   measured loop runs only [Cairos.Align.align]. *)
let identical_table = List.map (fun n -> (n, make_pair_identical ~n)) sizes
let offset_table = List.map (fun n -> (n, make_pair_offset ~n)) sizes

let pair_for table n =
  match List.assoc_opt n table with
  | Some p -> p
  | None -> failwith (Printf.sprintf "bench input: no pair for n=%d" n)

let test_inner =
  Test.make_indexed ~name:"inner/n" ~fmt:"%s=%d" ~args:sizes (fun n ->
      let a, b = pair_for offset_table n in
      Staged.stage (fun () ->
          match Cairos.Align.align ~strategy:`Inner a b with
          | Ok _ -> ()
          | Error _ -> failwith "bench input violates align contract"))

let test_left =
  Test.make_indexed ~name:"left/n" ~fmt:"%s=%d" ~args:sizes (fun n ->
      let a, b = pair_for identical_table n in
      Staged.stage (fun () ->
          match Cairos.Align.align ~strategy:`Left a b with
          | Ok _ -> ()
          | Error _ -> failwith "bench input violates align contract"))

let test_asof_backward =
  Test.make_indexed ~name:"asof-backward/n" ~fmt:"%s=%d" ~args:sizes (fun n ->
      let a, b = pair_for offset_table n in
      Staged.stage (fun () ->
          match Cairos.Align.align ~strategy:(`Asof `Backward) a b with
          | Ok _ -> ()
          | Error _ -> failwith "bench input violates align contract"))

let test_align =
  Test.make_grouped ~name:"align" [ test_inner; test_left; test_asof_backward ]

let benchmark () =
  let instances =
    Instance.[ monotonic_clock; minor_allocated; major_allocated ]
  in
  let cfg =
    Benchmark.cfg ~limit:3000 ~quota:(Time.second 2.0) ~stabilize:true ()
  in
  Benchmark.all cfg instances test_align

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
      Bench_emit.to_channel stdout ~bench:"align" results
        Instance.[ monotonic_clock; minor_allocated; major_allocated ]
