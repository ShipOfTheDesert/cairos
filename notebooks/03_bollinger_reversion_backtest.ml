(* -*- coding: utf-8 -*-
(* --- *)
(* jupyter: *)
(*   jupytext: *)
(*     formats: ipynb,ml:percent *)
(*     text_representation: *)
(*       extension: .ml *)
(*       format_name: percent *)
(*       format_version: '1.3' *)
(*       jupytext_version: 1.19.1 *)
(*   kernelspec: *)
(*     display_name: OCaml /home/valdev/personal/shipofthedesert/cairos *)
(*     language: OCaml *)
(*     name: ocaml-jupyter *)
(* --- *)

(* %% [markdown] *)
(* # Bollinger Mean-Reversion Backtest *)
(* *)
(* A long/flat mean-reversion backtest on a synthetic daily price series: *)
(* go long when the price crosses below the lower Bollinger band (20-day *)
(* SMA − 2σ), go flat when the price crosses back above the 20-day SMA. *)
(* The signal is lagged one day to avoid look-ahead, then combined with *)
(* the next day's returns. The strategy is measured with all four finance *)
(* metrics, and both an equity curve and a drawdown chart are rendered *)
(* inline. *)
(* *)
(* The pipeline shape — *)
(* *price → signal → lagged signal × pct_change → metrics → equity → *)
(* drawdown* — is shared with `02_sma_crossover_backtest.ml`; only the *)
(* signal rule differs. *)

(* %% vscode={"languageId": "ocaml"} *)
(* ocaml-jupyter on OCaml 5.x does not auto-load all stdlib modules;
   Nx requires Stdlib__Complex at link time. *)
#load "stdlib.cma"

#require "cairos"
#require "cairos_finance"
#require "cairos_plot"

(* %% vscode={"languageId": "ocaml"} *)
(* cairos_jupyter transitively requires jupyter.notebook, which the kernel
   crashes on if bundled with other #require directives — so it gets its
   own cell. *)
#require "cairos_jupyter"

(* %% [markdown] *)
(* ## Shared helpers *)
(* *)
(* Both backtest notebooks share a small `notebooks/_helpers.ml` file that *)
(* holds two pieces of boilerplate: `unwrap`, a terse notebook-style *)
(* `Result` unwrap that fails loudly on `Error`, and `product_1p`, the *)
(* expanding-window reducer that turns a returns window into an equity *)
(* multiplier. Factoring shared boilerplate into a sibling `.ml` file and *)
(* loading it with `#use` is the common real-world practice for notebook *)
(* series that share helper functions — keeping each notebook's body focused *)
(* on the strategy, not the ceremony. *)

(* %% vscode={"languageId": "ocaml"} *)
#use "_helpers.ml"

(* %% vscode={"languageId": "ocaml"} *)
open Cairos

(* %% [markdown] *)
(* ## Synthetic price series *)
(* *)
(* A 90-day daily price series built from a gentle upward drift, a small *)
(* oscillation, and three deliberate downward shocks: *)
(* *)
(* ```text *)
(* p[i] = 100 + 0.05 * i + 2 * sin(2π * i / 12) + shock[i] *)
(* shock[i] = -9 at i=32 ; -10 at i=57 ; -8 at i=80 ; 0 otherwise *)
(* ``` *)
(* *)
(* The oscillation amplitude (≈2) is too small on its own to cross a ±2σ *)
(* band around a 20-day SMA of the same signal — a pure sinusoid's peak *)
(* deviation from its mean is always inside its own ±2σ envelope. The *)
(* deliberate shocks are what produce the band crossings the strategy is *)
(* supposed to react to: each shock day sits far enough below the lower *)
(* band to trigger an entry, and the price then reverts to the drift line *)
(* within a day or two, crossing back above the SMA and triggering an *)
(* exit. *)
(* *)
(* The notebook is intentionally sized above the ~60-day target so that *)
(* the 20-day SMA has ≥70 days of valid signal and all three shock/revert *)
(* cycles sit well inside the active window. *)

(* %% vscode={"languageId": "ocaml"} *)
let n_days = 90

let price_values =
  Array.init n_days (fun i ->
      let t = float_of_int i in
      let base =
        100.0 +. (0.05 *. t) +. (2.0 *. sin (2.0 *. Float.pi *. t /. 12.0))
      in
      let shock =
        if i = 32 then -9.0
        else if i = 57 then -10.0
        else if i = 80 then -8.0
        else 0.0
      in
      base +. shock)

let date_strings =
  (* Consecutive calendar days starting 2025-01-02. Index.daily only
     requires strictly monotonic ISO-8601 strings, not a business-day
     calendar. *)
  let start =
    match Ptime.of_date (2025, 1, 2) with
    | Some t -> t
    | None -> failwith "invalid start date"
  in
  let one_day = Ptime.Span.of_int_s (24 * 3600) in
  Array.init n_days (fun i ->
      let rec advance t k =
        if k = 0 then t
        else
          match Ptime.add_span t one_day with
          | Some t' -> advance t' (k - 1)
          | None -> failwith "ptime overflow"
      in
      let t = advance start i in
      let (y, m, d), _ = Ptime.to_date_time t in
      Printf.sprintf "%04d-%02d-%02d" y m d)

let prices =
  let idx = unwrap "Index.daily" (Index.daily date_strings) in
  unwrap "Series.make"
    (Series.make idx (Nx.create Nx.float64 [| n_days |] price_values))

let () = Cairos_jupyter.pp_series "prices" prices

(* %% [markdown] *)
(* ## 20-day SMA, rolling std, and Bollinger bands *)
(* *)
(* `Window.sma` and `Window.rolling_std` both emit [Float.nan] for the *)
(* first 19 positions (strict warmup). The two resulting series share the *)
(* daily index of `prices`, so `Align.align ~strategy:`Inner` is a no-op *)
(* on index shape — but it is still the *value* that unlocks `map2`, and *)
(* crucially the single aligned value is reused for BOTH band *)
(* derivations. Computing `upper` and `lower` from the same *)
(* `aligned_mean_std` (rather than calling `Align.align` twice) is the *)
(* shape the PRD asks us to exercise: one alignment, two `map2` calls. *)

(* %% vscode={"languageId": "ocaml"} *)
let sma_20 = Window.sma ~n:20 prices
let std_20 = Window.rolling_std ~n:20 prices

let aligned_mean_std =
  unwrap "align sma_20/std_20"
    (Align.align ~strategy:`Inner sma_20 std_20)

let upper_band =
  Align.map2 (fun m s -> m +. (2.0 *. s)) aligned_mean_std

let lower_band =
  Align.map2 (fun m s -> m -. (2.0 *. s)) aligned_mean_std

let () =
  Cairos_jupyter.pp_first_valid "sma_20" sma_20;
  Cairos_jupyter.pp_first_valid "upper_band" upper_band;
  Cairos_jupyter.pp_first_valid "lower_band" lower_band

(* %% [markdown] *)
(* ## Signal via two crossing indicators and an expanding state scan *)
(* *)
(* The mean-reversion signal is stateful: once we enter (price crossed *)
(* below the lower band), we stay long until the price crosses back above *)
(* the 20-day SMA. Cairos does not expose a scan primitive on series, so *)
(* we implement the state machine using the public API only: *)
(* *)
(* 1. `below_lower[i] = 1.0` when `price[i] < lower_band[i]`, else `0.0`. *)
(* 2. `above_sma[i]  = 1.0` when `price[i] > sma_20[i]`, else `0.0`. *)
(* 3. `event[i] = below_lower[i] - above_sma[i]` — a signed indicator: *)
(*    `+1` on entry, `-1` on exit, `0` otherwise. *)
(* 4. `position = Window.expanding last_state event` where `last_state` *)
(*    scans the growing window backward for the most recent non-zero *)
(*    event and returns `1.0` for `+1`, `0.0` for `-1` or no event. *)
(* *)
(* During the SMA warmup (`i < 19`), every comparison against `NaN` *)
(* evaluates to `false` under IEEE 754, so both indicators are `0.0`, the *)
(* event series is `0.0`, and the position stays flat — the correct *)
(* behaviour for a warmup period. *)
(* *)
(* This is the "option (a)" shape described in the task: two indicator *)
(* series via `Align.align` + `map2`, then `Window.expanding` with a *)
(* last-crossing reducer. The expanding scan is O(n²) (each position *)
(* rescans its whole prefix), which is invisible at 90 points but *)
(* already the wrong shape at realistic sizes — recorded as a finding. *)

(* %% vscode={"languageId": "ocaml"} *)
let below_lower =
  let paired =
    unwrap "align prices/lower_band"
      (Align.align ~strategy:`Inner prices lower_band)
  in
  Align.map2 (fun p l -> if p < l then 1.0 else 0.0) paired

let above_sma =
  let paired =
    unwrap "align prices/sma_20"
      (Align.align ~strategy:`Inner prices sma_20)
  in
  Align.map2 (fun p m -> if p > m then 1.0 else 0.0) paired

let event =
  let paired =
    unwrap "align below_lower/above_sma"
      (Align.align ~strategy:`Inner below_lower above_sma)
  in
  Align.map2 (fun b a -> b -. a) paired

let last_state window =
  let arr = Nx.to_array window in
  let rec scan i =
    if i < 0 then 0.0
    else
      let e = arr.(i) in
      if e > 0.5 then 1.0
      else if e < -0.5 then 0.0
      else scan (i - 1)
  in
  scan (Array.length arr - 1)

let position = Window.expanding last_state event

let () = Cairos_jupyter.pp_series ~n:5 "position" position

(* %% [markdown] *)
(* ## Lagged signal and strategy returns *)
(* *)
(* `Series.shift 1 position` holds yesterday's position at each index, *)
(* which prevents the backtest from using today's signal to earn today's *)
(* return (look-ahead bias). Strategy returns are `lagged_position * *)
(* pct_change(price)`: `1.0 * r` when we were long, `0.0 * r = 0.0` when *)
(* flat. *)
(* *)
(* The first position of `lagged_position` is `NaN` (shift-vacated), so *)
(* the first value of `stgret` is `NaN`. We slice the series to drop the *)
(* full 20-day SMA warmup before feeding it into the metrics and into *)
(* `drawdown_series` (which explicitly requires NaN-free input). Slicing *)
(* the whole warmup rather than only the leading NaN keeps the metrics *)
(* from being dominated by the 19 days of fake-flat zero returns during *)
(* which the signal was structurally unable to fire — same choice made *)
(* in `02_sma_crossover_backtest.ml`. *)

(* %% vscode={"languageId": "ocaml"} *)
let lagged_position = Series.shift 1 position

let returns = Series.pct_change prices

let stgret =
  let paired =
    unwrap "align lagged_position/returns"
      (Align.align ~strategy:`Inner lagged_position returns)
  in
  Align.map2 ( *. ) paired

let stgret_active =
  Series.slice ~start:20 ~stop:(Series.length stgret) stgret

let () = Cairos_jupyter.pp_series ~n:5 "stgret_active" stgret_active

(* %% [markdown] *)
(* ## Strategy metrics *)
(* *)
(* All four `cairos_finance` scalar metrics on the active strategy *)
(* returns. On synthetic short series `sharpe` may come out as `nan` *)
(* when a constant segment collapses the std to zero — that is *)
(* acceptable on pedagogical data and is documented in the function's *)
(* contract. *)

(* %% vscode={"languageId": "ocaml"} *)
let () =
  Printf.printf "Annualised return: %.4f\n"
    (Cairos_finance.annualised_return stgret_active);
  Printf.printf "Annualised vol:    %.4f\n"
    (Cairos_finance.annualised_vol stgret_active);
  Printf.printf "Sharpe (rf=0):     %.4f\n"
    (Cairos_finance.sharpe ~risk_free:0.0 stgret_active);
  Printf.printf "Max drawdown:      %.4f\n"
    (Cairos_finance.max_drawdown stgret_active)

(* %% [markdown] *)
(* ## Equity curve *)
(* *)
(* `Window.expanding product_1p stgret_active` computes the cumulative *)
(* product of `(1 + r)` at every position, producing the equity *)
(* multiplier series starting from `1.0`. Same public-API-only approach *)
(* (and same O(n²) finding) as the SMA crossover notebook. *)

(* %% vscode={"languageId": "ocaml"} *)
let equity = Window.expanding product_1p stgret_active

let () =
  Cairos_plot.line_chart ~title:"Bollinger reversion — Equity curve" equity
  |> Cairos_jupyter.display

(* %% [markdown] *)
(* ## Drawdown *)
(* *)
(* `Cairos_finance.drawdown_series` returns a same-length series of *)
(* non-positive drawdown fractions (0.0 at equity highs, negative *)
(* underwater). Rendered as an inverted area chart via *)
(* `Cairos_plot.drawdown_chart`. *)

(* %% vscode={"languageId": "ocaml"} *)
let dd = Cairos_finance.drawdown_series stgret_active

let () =
  Cairos_plot.drawdown_chart ~title:"Bollinger reversion — Drawdown" dd
  |> Cairos_jupyter.display
