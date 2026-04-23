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
(* # Cairos Core API Tour *)
(* *)
(* One linear story that touches every public API across `cairos`, *)
(* `cairos_finance`, `cairos_plot`, and `cairos_jupyter`. Intended as the *)
(* canonical walkthrough for someone learning the library: construct a *)
(* daily price series, compute rolling statistics, build Bollinger bands *)
(* via a single `Align.align` + two `map2` calls, shift, resample to *)
(* weekly, group into a frame, inspect, compute returns and metrics, *)
(* render charts inline. *)
(* *)
(* Strategy backtesting belongs in the dedicated backtest notebooks — *)
(* this file is the API tour, not a trading rule. *)

(* %% vscode={"languageId": "ocaml"} *)
(* ocaml-jupyter on OCaml 5.x does not auto-load all stdlib modules;
   Nx requires Stdlib__Complex at link time. *)
#load "stdlib.cma"

#require "cairos"
#require "cairos_io"
#require "cairos_finance"
#require "cairos_plot"

(* %% vscode={"languageId": "ocaml"} *)
(* cairos_jupyter transitively requires jupyter.notebook, which the kernel
   crashes on if bundled with other #require directives — so it gets its
   own cell. *)
#require "cairos_jupyter"

(* %% vscode={"languageId": "ocaml"} *)
open Cairos

(* %% [markdown] *)
(* ## Constructing a Daily Price Series *)
(* *)
(* `Cairos_io.of_csv` loads a two-column CSV (date, price) and returns a *)
(* frequency-tagged `[`Daily] Series.t` directly — the timestamp index is *)
(* parsed and validated for monotonicity inside the loader, so the *)
(* notebook never touches `Ptime` or `Index.daily`. The result is *)
(* unwrapped at the boundary per the notebook convention: crash loudly on *)
(* a load failure, let the rest of the file assume clean inputs. *)

(* %% vscode={"languageId": "ocaml"} *)
let prices =
  match
    Cairos_io.of_csv ~freq:Freq.Day "data/01_core_tour.csv"
  with
  | Ok s -> s
  | Error e -> failwith (Printf.sprintf "Price series load failed: %s" e)

let () = Cairos_jupyter.pp_series "prices" prices

(* %% [markdown] *)
(* ## Rolling Statistics *)
(* *)
(* `Window.sma` and `Window.rolling_std` each return a series of the same *)
(* length as the input; the first `n-1` values are NaN (warmup). We'll *)
(* use the 20-period versions together to build Bollinger bands below. *)

(* %% vscode={"languageId": "ocaml"} *)
let sma_20 = Window.sma ~n:20 prices
let std_20 = Window.rolling_std ~n:20 prices

let () =
  Cairos_jupyter.pp_first_valid "SMA-20" sma_20;
  Cairos_jupyter.pp_first_valid "Std-20" std_20

(* %% [markdown] *)
(* ## Bollinger Bands: One `align`, Two `map2` *)
(* *)
(* Both upper and lower bands are derived from the same aligned pair of *)
(* `(sma_20, std_20)`. We align once and reuse the `aligned` value for *)
(* two separate `map2` calls — demonstrating that alignment is a value, *)
(* not a side effect, and that binary operations on already-aligned *)
(* inputs do not need to re-pay the alignment cost. *)

(* %% vscode={"languageId": "ocaml"} *)
let aligned_stats =
  match Align.align ~strategy:`Inner sma_20 std_20 with
  | Ok a -> a
  | Error e -> failwith (Printf.sprintf "SMA/Std alignment failed: %s" e)

let upper_band = Align.map2 (fun m s -> m +. (2.0 *. s)) aligned_stats
let lower_band = Align.map2 (fun m s -> m -. (2.0 *. s)) aligned_stats

let () =
  Cairos_jupyter.pp_series ~n:5 "upper_band" upper_band;
  Cairos_jupyter.pp_series ~n:5 "lower_band" lower_band

(* %% [markdown] *)
(* ## Shifting a Series *)
(* *)
(* `Series.shift 1` shifts values forward by one period — each position *)
(* holds the previous day's value and the first position is NaN. This is *)
(* the primitive used in backtests to express "act on yesterday's *)
(* signal", though here we just show the shape. *)

(* %% vscode={"languageId": "ocaml"} *)
let prev_close = Series.shift 1 prices

let () =
  let ov = Nx.to_array (Series.values prices) in
  let sv = Nx.to_array (Series.values prev_close) in
  Printf.printf "Original vs prev_close (first 5):\n";
  for i = 0 to 4 do
    Printf.printf "  [%d]: %.1f -> %.1f\n" i ov.(i) sv.(i)
  done

(* %% [markdown] *)
(* ## Resampling Daily → Weekly *)
(* *)
(* `Resample.resample` downsamples to a lower frequency using a named *)
(* aggregation. The resulting series carries a different phantom *)
(* frequency tag (`[`Weekly]`), which makes accidental alignment against *)
(* the daily series a compile error. *)

(* %% vscode={"languageId": "ocaml"} *)
let weekly_prices =
  match Resample.resample ~agg:`Last Freq.Week prices with
  | Ok w -> w
  | Error e -> failwith (Printf.sprintf "Resample failed: %s" e)

let () = Cairos_jupyter.pp_series ~n:5 "weekly_prices" weekly_prices

(* %% [markdown] *)
(* ## Building a Frame *)
(* *)
(* `Frame.of_series` groups several same-index series into a named *)
(* multi-series structure. Upper and lower bands share the daily index *)
(* with `prices` and `sma_20` (NaN warmup values are still values, not *)
(* missing index entries), so a four-column frame is a direct call. *)

(* %% vscode={"languageId": "ocaml"} *)
let frame =
  match
    Frame.of_series
      (Nonempty.make
         ("price", prices)
         [
           ("sma_20", sma_20);
           ("upper", upper_band);
           ("lower", lower_band);
         ])
  with
  | Ok f -> f
  | Error e -> failwith (Printf.sprintf "Frame construction failed: %s" e)

let () = Cairos_jupyter.pp_frame frame

(* %% [markdown] *)
(* ## Inspecting Series and Frame *)
(* *)
(* `Series.head`, `Series.tail`, and `Series.first_valid` peek at the *)
(* head/tail/first-non-NaN of a series. `Frame.head` and `Frame.describe` *)
(* give frame-level views. *)

(* %% vscode={"languageId": "ocaml"} *)
let () =
  Printf.printf "--- Series.head / tail ---\n";
  Cairos_jupyter.pp_series ~n:5 "prices (head 5)" (Series.head 5 prices);
  Cairos_jupyter.pp_series ~n:5 "prices (tail 5)" (Series.tail 5 prices)

(* %% vscode={"languageId": "ocaml"} *)
let () =
  Printf.printf "--- Series.first_valid ---\n";
  (match Series.first_valid sma_20 with
  | Some (i, v) -> Printf.printf "  sma_20 first valid: idx=%d value=%.4f\n" i v
  | None -> Printf.printf "  sma_20 first valid: none\n");
  Cairos_jupyter.pp_first_valid "upper_band" upper_band;
  Cairos_jupyter.pp_first_valid "lower_band" lower_band

(* %% vscode={"languageId": "ocaml"} *)
let () =
  Printf.printf "--- Frame.head 5 ---\n";
  Cairos_jupyter.pp_frame ~n:5 (Frame.head 5 frame)

(* %% vscode={"languageId": "ocaml"} *)
let () =
  Printf.printf "--- Frame.describe ---\n";
  Cairos_jupyter.pp_describe frame

(* %% [markdown] *)
(* ## Returns and Financial Metrics *)
(* *)
(* `Series.pct_change` produces period-over-period returns (first value *)
(* is NaN); we slice it off before feeding into the finance metrics. All *)
(* five `cairos_finance` surface functions are called here. *)

(* %% vscode={"languageId": "ocaml"} *)
let returns =
  Series.pct_change prices |> fun r ->
  Series.slice ~start:1 ~stop:(Series.length r) r

let () =
  Printf.printf "Annualised return: %.4f\n"
    (Cairos_finance.annualised_return returns);
  Printf.printf "Annualised vol:    %.4f\n"
    (Cairos_finance.annualised_vol returns);
  Printf.printf "Sharpe ratio:      %.4f\n"
    (Cairos_finance.sharpe ~risk_free:0.0 returns);
  Printf.printf "Max drawdown:      %.4f\n"
    (Cairos_finance.max_drawdown returns)

(* %% vscode={"languageId": "ocaml"} *)
let dd = Cairos_finance.drawdown_series returns

let () = Cairos_jupyter.pp_series "drawdown" dd

(* %% [markdown] *)
(* ## Charts *)
(* *)
(* `Cairos_plot.line_chart` and `Cairos_plot.drawdown_chart` produce *)
(* `Scene.t` values; `Cairos_jupyter.display` renders them inline as *)
(* SVG. Multi-series overlay (bands on the same axes as price) is out of *)
(* scope for the PoC. *)

(* %% vscode={"languageId": "ocaml"} *)
let () =
  Cairos_plot.line_chart ~title:"Core tour — Price" prices
  |> Cairos_jupyter.display

(* %% vscode={"languageId": "ocaml"} *)
let () =
  Cairos_plot.drawdown_chart ~title:"Core tour — Drawdown" dd
  |> Cairos_jupyter.display
