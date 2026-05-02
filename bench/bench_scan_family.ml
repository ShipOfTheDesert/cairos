(* Run with: opam exec -- dune exec bench/bench_scan_family.exe

   Benchmark: prefix-accumulation family on a 100k-element daily float64
   series — Series.cumsum, Series.scan ( +. ) 0.0, and Series.cumprod.
   Three named cells confirm O(n) wall-clock scaling at the PRD's pinned
   100k size (PRD FR-1 line 5).

   The same input series is reused across all three tests — they don't
   mutate input, and the scan family shares its hot-loop shape (single
   left-fold over the underlying float64 array). Sharing keeps setup
   cost paid once and the relative timing of the three reducers
   directly comparable.

   Quota tuning: kept at the precedent ~limit:3000 ~quota:2.0s per
   bench_cumprod.ml:56-57. At 100k each iteration is ~10x the 10k cell;
   smoke runs land well under 10s end-to-end (NFR-3). If a future
   maintainer sees a cell time out, bump quota and document here.

   Prerequisite: this file is only built when cairos's :with-test deps are
   installed (bechamel + bechamel-notty). Run
     opam install --deps-only --with-test .
   after a fresh clone. *)

open Bechamel
open Toolkit

let n = 100_000

let make_input () =
  let idx = Bench_fixtures.make_index ~length:n () in
  (* Synthetic (1 + returns) series: random positive values in [0.99, 1.01]
     so cumprod doesn't overflow over 100k bars. cumsum and scan ( +. )
     also accept the same shape. *)
  let values =
    Nx.create Nx.float64 [| n |]
      (Array.init n (fun i ->
           let x = Float.of_int (((i * 7) + 13) mod 1000) /. 50_000.0 in
           1.0 -. 0.01 +. x))
  in
  Bench_fixtures.make_series idx values

(* Setup is hoisted out of [Staged.stage] so the hot loop only measures
   the operation under test. The same 100k-point input is shared across
   all three tests — none of [cumsum], [scan], [cumprod] mutate input. *)
let test_scan_family =
  let s = make_input () in
  Test.make_grouped ~name:"scan"
    [
      Test.make ~name:"cumsum/100k"
        (Staged.stage (fun () -> ignore (Cairos.Series.cumsum s)));
      Test.make ~name:"scan-add/100k"
        (Staged.stage (fun () -> ignore (Cairos.Series.scan ( +. ) 0.0 s)));
      Test.make ~name:"cumprod/100k"
        (Staged.stage (fun () -> ignore (Cairos.Series.cumprod s)));
    ]

let benchmark () =
  let instances =
    Instance.[ monotonic_clock; minor_allocated; major_allocated ]
  in
  let cfg =
    Benchmark.cfg ~limit:3000 ~quota:(Time.second 2.0) ~stabilize:true ()
  in
  Benchmark.all cfg instances test_scan_family

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
      Bench_emit.to_channel stdout ~bench:"scan_family" results
        Instance.[ monotonic_clock; minor_allocated; major_allocated ]
