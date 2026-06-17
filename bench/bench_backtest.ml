(* Run with: opam exec -- dune exec bench/bench_backtest.exe

   Benchmark: Cairos_engine.Backtest.run at 100 instruments × n_bars daily
   bars × weekly rebalances, indexed at n_bars in {250, 500, 1000} (PRD 0055
   FR-6, RFC 0056 §Phase 5 / §Module Breakdown). One Bechamel benchmark cell
   per indexed n_bars; the inner closure does exactly one Backtest.run.

   Per the bechamel-staged-setup-hoisting handbook
   (~/.claude/solutions/ocaml/bechamel-staged-setup-hoisting.md), the price
   frame, signal frame, and rebalance index are built once per indexed n_bars
   above the [Staged.stage] boundary. The closure captures those values so
   only the loop body runs per iteration.

   Fixture shape (deterministic, no randomness — the bench does not need
   reproducibility across machines beyond the OCaml side):
   - n_cols = 100 (RFC 0056 §Phase 5 step 16, MVP scaling target).
   - prices: per-instrument multiplicative random walks anchored at 1.0
     with per-bar ratios driven by an integer pseudo-random sequence so the
     ratio stays in [0.999, 1.001]. Strictly positive across all bars.
   - rebalance bars: every 5 bars starting at bar 5, ending at bar
     [n_bars - 2] inclusive. Yields ~50 / ~100 / ~200 weekly rebalances at
     n_bars = 250 / 500 / 1000.
   - signals: small target weights in [-0.01, 0.01] driven by the same
     integer sequence, written at rebalance bars only (zeros elsewhere).
     Sum-of-weights across 100 instruments is bounded at ~0.5 in absolute
     value, so the per-bar MTM factor stays well above zero.
   - commission = 0.001, slippage = 0.0005 (matching the cross-validate
     harness — no need to invent a third constant).

   Prerequisite: this file is only built when cairos / cairos_engine's
   :with-test deps are installed (bechamel + bechamel-notty). Run
     opam install --deps-only --with-test .
   after a fresh clone. *)

open Bechamel
open Toolkit

let n_cols = 100
let n_bars_args = [ 250; 500; 1000 ]
let commission = 0.001
let slippage = 0.0005
let rebalance_step = 5
let first_rebalance_bar = 5

(* Deterministic price ratio in [0.999, 1.001]. The integer sequence is
   chosen so adjacent (j, t) cells differ — a large stride on j (101) and a
   small stride on t (7) decorrelate columns enough to give visible
   cross-instrument variance without introducing floating-point noise. *)
let price_ratio ~j ~t =
  let k = ((j * 101) + (t * 7) + 13) mod 4001 in
  1.0 +. (float_of_int (k - 2000) /. 2000.0 *. 0.001)

(* Deterministic target weight in [-0.01, 0.01]. Same integer-sequence
   shape; different multiplier and offset so signal targets do not align
   with price ratios. *)
let target_weight ~j ~k =
  let m = ((j * 53) + (k * 11) + 7) mod 2003 in
  float_of_int (m - 1001) /. 1001.0 *. 0.01

let make_index n =
  let ts =
    Array.init n (fun i -> Bench_fixtures.epoch +. (float_of_int i *. 86_400.0))
  in
  match Cairos.Index.of_unix_floats Cairos.Freq.Day ts with
  | Ok idx -> idx
  | Error e -> failwith ("bench_backtest index: " ^ Cairos.Index.err_to_string e)

let make_price_columns ~n_bars =
  Array.init n_cols (fun j ->
      let p = Array.make n_bars 0.0 in
      p.(0) <- 1.0;
      for t = 1 to n_bars - 1 do
        p.(t) <- p.(t - 1) *. price_ratio ~j ~t
      done;
      p)

let rebalance_bars_for ~n_bars =
  (* Weekly rebalances at bars 5, 10, …, up to and including [n_bars - 2]
     (the upper bound matches RFC 0052 entrypoint precondition 6: every
     rebalance must have a T+1 open available). *)
  let last = n_bars - 2 in
  let rec loop bar acc =
    if bar > last then List.rev acc else loop (bar + rebalance_step) (bar :: acc)
  in
  Array.of_list (loop first_rebalance_bar [])

let make_signal_columns ~n_bars ~rebalance_bars =
  Array.init n_cols (fun j ->
      let s = Array.make n_bars 0.0 in
      Array.iteri (fun k bar -> s.(bar) <- target_weight ~j ~k) rebalance_bars;
      s)

let frame_from_columns ~idx ~values =
  let pairs =
    List.init n_cols (fun j ->
        let nx =
          Nx.create Nx.float64 [| Array.length values.(j) |] values.(j)
        in
        let s = Cairos.Series.make_unsafe idx nx in
        ("c" ^ string_of_int j, s))
  in
  match pairs with
  | [] -> failwith "bench_backtest: empty columns (unreachable)"
  | first :: rest -> (
      let ne = Cairos.Nonempty.make first rest in
      match Cairos.Frame.of_series ne with
      | Ok f -> f
      | Error e -> failwith ("bench_backtest frame: " ^ e))

let rebalance_index_for ~price_idx ~rebalance_bars =
  let price_ts = Cairos.Index.timestamps price_idx in
  let floats =
    Array.map (fun bar -> Ptime.to_float_s price_ts.(bar)) rebalance_bars
  in
  match Cairos.Index.of_unix_floats Cairos.Freq.Day floats with
  | Ok idx -> idx
  | Error e ->
      failwith
        ("bench_backtest rebalance index: " ^ Cairos.Index.err_to_string e)

type fixture = {
  price_frame : [ `Daily ] Cairos.Frame.t;
  signal_frame : [ `Daily ] Cairos.Frame.t;
  rebalance_index : [ `Daily ] Cairos.Index.t;
}

let make_fixture ~n_bars =
  let idx = make_index n_bars in
  let prices = make_price_columns ~n_bars in
  let rebalance_bars = rebalance_bars_for ~n_bars in
  let signals = make_signal_columns ~n_bars ~rebalance_bars in
  let price_frame = frame_from_columns ~idx ~values:prices in
  let signal_frame = frame_from_columns ~idx ~values:signals in
  let rebalance_index = rebalance_index_for ~price_idx:idx ~rebalance_bars in
  { price_frame; signal_frame; rebalance_index }

(* Pre-build one fixture per indexed n_bars; the staged closure captures the
   fixture so the measured loop runs only [Cairos_engine.Backtest.run]. *)
let fixture_table =
  List.map (fun n_bars -> (n_bars, make_fixture ~n_bars)) n_bars_args

let fixture_for n_bars =
  match List.assoc_opt n_bars fixture_table with
  | Some f -> f
  | None ->
      failwith
        (Printf.sprintf "bench_backtest: no fixture for n_bars=%d" n_bars)

let test_run =
  Test.make_indexed ~name:"run/n_bars" ~fmt:"%s=%d" ~args:n_bars_args
    (fun n_bars ->
      let { price_frame; signal_frame; rebalance_index } = fixture_for n_bars in
      Staged.stage (fun () ->
          match
            Cairos_engine.Backtest.run ~price_frame ~signal_frame
              ~rebalance_index ~commission ~slippage
          with
          | Ok _ -> ()
          | Error _ -> failwith "bench input violates Backtest.run contract"))

let benchmark () =
  let instances =
    Instance.[ monotonic_clock; minor_allocated; major_allocated ]
  in
  (* Quota raised to 8s/cell because the n_bars=1000 cell runs ~4.4ms per
     iteration (200 weekly rebalances × 100 instruments × MTM at every bar).
     At the 2s default that yields ~450 samples — sufficient today, but a
     future regression that nudges per-iteration time higher could push
     sample count toward the OLS [r_square] noise floor. 8s preserves
     headroom (~1800 samples on the largest cell) while keeping the
     three-cell wall-time at ~24s — well inside the bench-suite NFR-3
     budget. *)
  let cfg =
    Benchmark.cfg ~limit:3000 ~quota:(Time.second 8.0) ~stabilize:true ()
  in
  Benchmark.all cfg instances test_run

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
      Bench_emit.to_channel stdout ~bench:"backtest" results
        Instance.[ monotonic_clock; minor_allocated; major_allocated ]
