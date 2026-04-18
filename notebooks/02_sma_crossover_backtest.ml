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
(* # SMA Crossover Backtest *)
(* *)
(* A long/flat trend-following backtest on a synthetic daily price series: *)
(* go long when the fast SMA (10) is above the slow SMA (50), flat otherwise. *)
(* The signal is lagged one day to avoid look-ahead, then combined with the *)
(* next day's returns. The strategy is measured with all four finance *)
(* metrics, and both an equity curve and a drawdown chart are rendered *)
(* inline. *)
(* *)
(* The pipeline shape — *)
(* *price → signal → lagged signal × pct_change → metrics → equity → *)
(* drawdown* — is shared with `03_bollinger_reversion_backtest.ml`; only the *)
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
(* holds one piece of boilerplate: `unwrap`, a terse notebook-style *)
(* `Result` unwrap that fails loudly on `Error`. Factoring shared *)
(* boilerplate into a sibling `.ml` file and loading it with `#use` is the *)
(* common real-world practice for notebook series that share helper *)
(* functions — keeping each notebook's body focused on the strategy, not *)
(* the ceremony. *)

(* %% vscode={"languageId": "ocaml"} *)
#use "_helpers.ml"

(* %% vscode={"languageId": "ocaml"} *)
open Cairos

(* %% [markdown] *)
(* ## Synthetic price series *)
(* *)
(* A 90-day daily price series built from a drifting mean plus a *)
(* period-30 sinusoid: *)
(* *)
(* ```text *)
(* p[i] = 100 + 0.1 * i + 6 * sin(2π * i / 30) *)
(* ``` *)
(* *)
(* The drift is gentle enough that the slow SMA (50) is close to a *)
(* straight line through the series, while the fast SMA (10) tracks the *)
(* sinusoid with some damping. This produces multiple fast-above-slow *)
(* crossings in the valid-signal window (days 49..89), so the signal is *)
(* not constant and the strategy metrics are not trivially zero. *)
(* *)
(* The notebook is intentionally sized slightly above the ~60-day target *)
(* so that the 50-day slow SMA has ≥30 days of valid signal to exhibit *)
(* crossings — anything much shorter collapses the signal window to a *)
(* handful of points and makes the metrics pathological. *)

(* %% vscode={"languageId": "ocaml"} *)
let n_days = 90

let price_values =
  Array.init n_days (fun i ->
      let t = float_of_int i in
      100.0 +. (0.1 *. t) +. (6.0 *. sin (2.0 *. Float.pi *. t /. 30.0)))

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
(* ## Fast and slow simple moving averages *)
(* *)
(* `Window.sma` returns a series of the same length as the input with *)
(* the first `n-1` positions set to `Float.nan` (strict warmup). The *)
(* slow SMA has a 49-day warmup; the fast SMA has a 9-day warmup. *)

(* %% vscode={"languageId": "ocaml"} *)
let fast_sma = Window.sma ~n:10 prices
let slow_sma = Window.sma ~n:50 prices

let () =
  Cairos_jupyter.pp_first_valid "fast_sma (10)" fast_sma;
  Cairos_jupyter.pp_first_valid "slow_sma (50)" slow_sma

(* %% [markdown] *)
(* ## Signal via `Align.align` + `map2` *)
(* *)
(* The crossover signal is a binary long/flat indicator: `1.0` when the *)
(* fast SMA is strictly above the slow SMA, `0.0` otherwise. The two *)
(* SMAs share the daily index of `prices`, so an `Inner` alignment is *)
(* a no-op on index shape — but it is still the *value* that unlocks *)
(* `map2`, which is the only way to compose two series element-wise. *)
(* *)
(* During the warmup (either SMA is `NaN`), `NaN > _` is `false` under *)
(* IEEE 754, so the signal is `0.0` for warmup days — not `NaN`. The *)
(* strategy is flat during warmup, which is the correct behaviour. *)

(* %% vscode={"languageId": "ocaml"} *)
let aligned_smas =
  unwrap "align fast/slow SMA"
    (Align.align ~strategy:`Inner fast_sma slow_sma)

let signal =
  Align.map2 (fun fast slow -> if fast > slow then 1.0 else 0.0) aligned_smas

let () = Cairos_jupyter.pp_series ~n:5 "signal" signal

(* %% [markdown] *)
(* ## Lagged signal and strategy returns *)
(* *)
(* `Series.shift 1 signal` holds yesterday's signal at each position, *)
(* which prevents the backtest from using today's signal to earn today's *)
(* return (look-ahead bias). Strategy returns are `lagged_signal * *)
(* pct_change(price)`: `1.0 * r` when we were long, `0.0 * r = 0.0` when *)
(* flat. *)
(* *)
(* The first position of `lagged_signal` is `NaN` (shift-vacated), so *)
(* the first value of `stgret` is `NaN`. We slice the series to drop the *)
(* warmup and the leading `NaN` before feeding it into the metrics and *)
(* into `drawdown_series` (which explicitly requires NaN-free input). *)

(* %% vscode={"languageId": "ocaml"} *)
let lagged_signal = Series.shift 1 signal

let returns = Series.pct_change prices

let stgret =
  let paired =
    unwrap "align lagged_signal/returns"
      (Align.align ~strategy:`Inner lagged_signal returns)
  in
  Align.map2 ( *. ) paired

(* Drop the 50-day SMA warmup so the metrics and charts see only the
   period in which the signal is actually active. *)
let stgret_active =
  Series.slice ~start:50 ~stop:(Series.length stgret) stgret

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
(* The equity multiplier at each day is the running product of `(1 + r)` *)
(* over the active strategy returns. `Series.cumprod` is the idiomatic *)
(* Cairos primitive for this: a single linear pass that preserves the *)
(* daily index and the frequency phantom type. `Series.map` first lifts *)
(* each return `r` to `1 + r`; `Series.cumprod` then threads the *)
(* multiplicative accumulator forward, starting from `1.0`. *)

(* %% vscode={"languageId": "ocaml"} *)
let equity = Series.cumprod (Series.map (fun t -> Nx.add_s t 1.0) stgret_active)

let () =
  Cairos_plot.line_chart ~title:"SMA crossover — Equity curve" equity
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
  Cairos_plot.drawdown_chart ~title:"SMA crossover — Drawdown" dd
  |> Cairos_jupyter.display

(* %% [markdown] *)
(* ## Type system note — frequency tag rejection (FR-4) *)
(* *)
(* While authoring this notebook I deliberately attempted a *)
(* daily-vs-weekly alignment to confirm that the phantom frequency tag *)
(* on `Series.t` actually rejects the mistake at compile time. The *)
(* candidate code was: *)
(* *)
(* ```ocaml *)
(* let weekly = unwrap "resample" (Resample.resample ~agg:`Last Freq.Week prices) in *)
(* let _ = Align.align ~strategy:`Inner prices weekly in *)
(* () *)
(* ``` *)
(* *)
(* The OCaml compiler rejected this with: *)
(* *)
(* ```text *)
(* Error: The value "weekly" has type *)
(*          "([ `Weekly ], (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t" *)
(*        but an expression was expected of type *)
(*          "([ `Daily ], (float, 'a) Nx.t) Cairos.Series.t" *)
(*        These two variant types have no intersection *)
(* ``` *)
(* *)
(* The constraint doing the work is the phantom `[ \`Daily ]` vs *)
(* `[ \`Weekly ]` frequency tag on `Series.t`, which `Align.align` *)
(* propagates through its signature: both series must share the same *)
(* `'freq` type parameter. There is no runtime path through which a *)
(* daily series can be aligned against a weekly one. *)
(* *)
(* In Pandas the same mistake — calling `daily_df.align(weekly_df)` — *)
(* would silently succeed, returning a reindexed pair whose behaviour *)
(* depends on undocumented join semantics, and the bug would only *)
(* surface later as a nonsensical backtest result. Cairos catches it *)
(* before the notebook runs. *)
(* *)
(* The rejected code is not committed; only this note. *)
