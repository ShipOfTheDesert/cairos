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
(*     display_name: OCaml *)
(*     language: ocaml *)
(*     name: ocaml-jupyter *)
(* --- *)

(* %% [markdown] *)
(* # SMA Crossover Walkthrough *)
(* *)
(* This notebook exercises the core Cairos modules through a simple moving *)
(* average (SMA) crossover analysis: *)
(* *)
(* - **Index** — daily timestamp index from ISO 8601 strings *)
(* - **Series** — indexed float series construction *)
(* - **Window** — SMA-20 and SMA-50 computation *)
(* - **Align** — inner-aligning two SMAs for element-wise operations *)
(* - **Resample** — downsampling daily prices to weekly *)
(* - **Frame** — grouping price + SMA-20 + SMA-50 into a multi-series frame *)

(* %% vscode={"languageId": "ocaml"} *)
(* ocaml-jupyter on OCaml 5.x does not auto-load all stdlib modules;
   Nx requires Stdlib__Complex at link time. *)
#load "stdlib.cma"

#require "cairos"

(* %% vscode={"languageId": "ocaml"} *)
open Cairos

#mod_use "notebook_helpers.ml"

let ( let* ) = Result.bind

(* %% [markdown] *)
(* ## Constructing a Daily Price Series *)
(* *)
(* We define ~60 trading days of hardcoded price data and construct a *)
(* daily-frequency indexed series. *)

(* %% vscode={"languageId": "ocaml"} *)
let prices =
  let dates =
    [|
      "2025-01-02";
      "2025-01-03";
      "2025-01-06";
      "2025-01-07";
      "2025-01-08";
      "2025-01-09";
      "2025-01-10";
      "2025-01-13";
      "2025-01-14";
      "2025-01-15";
      "2025-01-16";
      "2025-01-17";
      "2025-01-20";
      "2025-01-21";
      "2025-01-22";
      "2025-01-23";
      "2025-01-24";
      "2025-01-27";
      "2025-01-28";
      "2025-01-29";
      "2025-01-30";
      "2025-01-31";
      "2025-02-03";
      "2025-02-04";
      "2025-02-05";
      "2025-02-06";
      "2025-02-07";
      "2025-02-10";
      "2025-02-11";
      "2025-02-12";
      "2025-02-13";
      "2025-02-14";
      "2025-02-17";
      "2025-02-18";
      "2025-02-19";
      "2025-02-20";
      "2025-02-21";
      "2025-02-24";
      "2025-02-25";
      "2025-02-26";
      "2025-02-27";
      "2025-02-28";
      "2025-03-03";
      "2025-03-04";
      "2025-03-05";
      "2025-03-06";
      "2025-03-07";
      "2025-03-10";
      "2025-03-11";
      "2025-03-12";
      "2025-03-13";
      "2025-03-14";
      "2025-03-17";
      "2025-03-18";
      "2025-03-19";
      "2025-03-20";
      "2025-03-21";
      "2025-03-24";
      "2025-03-25";
      "2025-03-26";
    |]
  in
  let values =
    [|
      100.0;
      101.5;
      102.0;
      101.0;
      103.0;
      104.5;
      105.0;
      104.0;
      103.5;
      105.0;
      106.5;
      107.0;
      108.0;
      107.5;
      109.0;
      110.0;
      111.5;
      112.0;
      111.0;
      113.0;
      114.5;
      115.0;
      114.0;
      113.5;
      115.0;
      116.5;
      117.0;
      118.0;
      117.5;
      119.0;
      120.0;
      121.5;
      122.0;
      121.0;
      123.0;
      124.5;
      125.0;
      124.0;
      123.5;
      125.0;
      126.5;
      127.0;
      128.0;
      127.5;
      129.0;
      130.0;
      131.5;
      132.0;
      131.0;
      133.0;
      134.5;
      135.0;
      134.0;
      133.5;
      135.0;
      136.5;
      137.0;
      138.0;
      137.5;
      139.0;
    |]
  in
  let* idx = Index.daily dates in
  Series.make idx (Nx.create Nx.float64 [| Array.length values |] values)

let prices =
  match prices with
  | Ok s -> s
  | Error e -> failwith (Printf.sprintf "Price series construction failed: %s" e)

let () = Notebook_helpers.pp_series "prices" prices

(* %% [markdown] *)
(* ## Computing SMAs *)
(* *)
(* We compute SMA-20 and SMA-50. The first n-1 values are NaN (warmup). *)

(* %% vscode={"languageId": "ocaml"} *)
let sma_20 = Window.sma ~n:20 prices
let sma_50 = Window.sma ~n:50 prices

let () =
  Notebook_helpers.pp_first_valid "SMA-20" sma_20;
  Notebook_helpers.pp_first_valid "SMA-50" sma_50

(* %% [markdown] *)
(* ## Aligning the Two SMAs *)
(* *)
(* Both SMAs share the same index (same length, same timestamps — NaN is a *)
(* value, not a missing index entry). Inner alignment preserves the full *)
(* index. We compute the spread: SMA-20 − SMA-50. *)

(* %% vscode={"languageId": "ocaml"} *)
let aligned_smas =
  match Align.align ~strategy:`Inner sma_20 sma_50 with
  | Ok a -> a
  | Error e -> failwith (Printf.sprintf "SMA alignment failed: %s" e)

let spread = Align.map2 (fun a b -> a -. b) aligned_smas

let () = Notebook_helpers.pp_series ~n:5 "spread" spread

(* %% [markdown] *)
(* ## Resampling to Weekly *)
(* *)
(* Downsample the daily price series to weekly frequency using last-value *)
(* aggregation. *)

(* %% vscode={"languageId": "ocaml"} *)
let weekly_prices =
  match Resample.resample ~agg:`Last Freq.Week prices with
  | Ok w -> w
  | Error e -> failwith (Printf.sprintf "Resample failed: %s" e)

let () = Notebook_helpers.pp_series ~n:5 "weekly_prices" weekly_prices

(* %% [markdown] *)
(* ## Grouping into a Frame *)
(* *)
(* All three series (price, SMA-20, SMA-50) share the same index, so *)
(* `Frame.of_series` succeeds directly. NaN values in the SMA columns *)
(* for the warmup period are the correct representation. *)

(* %% vscode={"languageId": "ocaml"} *)
let frame =
  match
    Frame.of_series
      [ ("price", prices); ("sma_20", sma_20); ("sma_50", sma_50) ]
  with
  | Ok f -> f
  | Error e -> failwith (Printf.sprintf "Frame construction failed: %s" e)

let () = Notebook_helpers.pp_frame frame

(* %% [markdown] *)
(* ## Inspecting Series and Frame *)
(* *)
(* `Series.head`, `Series.tail`, and `Series.first_valid` let us peek at *)
(* data without printing the full series. `Frame.head` and `Frame.describe` *)
(* provide frame-level inspection. *)

(* %% vscode={"languageId": "ocaml"} *)
let () =
  Printf.printf "--- Series.head / tail ---\n";
  Notebook_helpers.pp_series ~n:5 "prices (head 5)" (Series.head 5 prices);
  Notebook_helpers.pp_series ~n:5 "prices (tail 5)" (Series.tail 5 prices)

(* %% vscode={"languageId": "ocaml"} *)
let () =
  Printf.printf "--- Series.first_valid ---\n";
  Notebook_helpers.pp_first_valid "SMA-20" sma_20;
  Notebook_helpers.pp_first_valid "SMA-50" sma_50;
  Notebook_helpers.pp_first_valid "spread" spread

(* %% vscode={"languageId": "ocaml"} *)
let () =
  Printf.printf "--- Frame.head 5 ---\n";
  Notebook_helpers.pp_frame ~n:5 (Frame.head 5 frame)

(* %% vscode={"languageId": "ocaml"} *)
let () =
  Printf.printf "--- Frame.describe ---\n";
  Notebook_helpers.pp_describe frame

(* %% [markdown] *)
(* ## Next Steps *)
(* *)
(* TODO(e02-finance): Compute Sharpe ratio from daily returns here *)
(* TODO(e02-finance): Add maximum drawdown calculation *)
(* TODO(e03-plot): Add line chart of price vs SMA crossover *)
