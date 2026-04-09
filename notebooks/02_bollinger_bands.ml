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
(* # Bollinger Bands Walkthrough *)
(* *)
(* This notebook constructs Bollinger Bands from daily price data, *)
(* exercising rolling statistics, alignment, and shifted series: *)
(* *)
(* - **Index** — daily timestamp index from ISO 8601 strings *)
(* - **Series** — indexed float series construction and shifting *)
(* - **Window** — SMA-20 and rolling standard deviation *)
(* - **Align** — aligning SMA and std for band construction *)

(* %% *)
(* ocaml-jupyter on OCaml 5.x does not auto-load all stdlib modules;
   Nx requires Stdlib__Complex at link time. *)
#load "stdlib.cma"

#require "cairos"

(* %% *)
open Cairos

#mod_use "notebook_helpers.ml"

let ( let* ) = Result.bind

(* %% [markdown] *)
(* ## Price Series Construction *)
(* *)
(* We define ~60 trading days of hardcoded price data and construct a *)
(* daily-frequency indexed series. *)

(* %% *)
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
      150.0;
      152.0;
      149.5;
      151.0;
      153.0;
      155.5;
      154.0;
      156.0;
      158.0;
      157.0;
      155.5;
      158.0;
      160.0;
      159.0;
      161.5;
      163.0;
      162.0;
      164.0;
      166.0;
      165.0;
      163.5;
      166.0;
      168.0;
      167.0;
      169.5;
      171.0;
      170.0;
      172.0;
      174.0;
      173.0;
      171.5;
      174.0;
      176.0;
      175.0;
      177.5;
      179.0;
      178.0;
      180.0;
      182.0;
      181.0;
      179.5;
      182.0;
      184.0;
      183.0;
      185.5;
      187.0;
      186.0;
      188.0;
      190.0;
      189.0;
      187.5;
      190.0;
      192.0;
      191.0;
      193.5;
      195.0;
      194.0;
      196.0;
      198.0;
      197.0;
    |]
  in
  let* idx = Index.daily dates in
  Series.make idx (Nx.create Nx.float64 [| Array.length values |] values)

let prices =
  match prices with
  | Ok s -> s
  | Error e ->
      failwith (Printf.sprintf "Price series construction failed: %s" e)

let () = Notebook_helpers.pp_series "prices" prices

(* %% [markdown] *)
(* ## Rolling Statistics *)
(* *)
(* We compute SMA-20 and the 20-period rolling standard deviation. *)
(* The first 19 values of each are NaN (warmup period). *)

(* %% *)
let sma_20 = Window.sma ~n:20 prices
let std_20 = Window.rolling_std ~n:20 prices

let () =
  Notebook_helpers.pp_first_valid "SMA-20" sma_20;
  Notebook_helpers.pp_first_valid "Std-20" std_20

(* %% [markdown] *)
(* ## Constructing Bollinger Bands *)
(* *)
(* Upper band = SMA-20 + 2 × rolling_std. *)
(* Lower band = SMA-20 − 2 × rolling_std. *)
(* Both series share the same index, so inner alignment preserves all values. *)

(* %% *)
let aligned_stats =
  match Align.align ~strategy:`Inner sma_20 std_20 with
  | Ok a -> a
  | Error e -> failwith (Printf.sprintf "SMA/Std alignment failed: %s" e)

let upper_band = Align.map2 (fun a b -> a +. (2.0 *. b)) aligned_stats
let lower_band = Align.map2 (fun a b -> a -. (2.0 *. b)) aligned_stats

let () =
  Notebook_helpers.pp_series ~n:5 "upper_band" upper_band;
  Notebook_helpers.pp_series ~n:5 "lower_band" lower_band

(* %% [markdown] *)
(* ## Shifted Price for Comparison *)
(* *)
(* Shift the price series forward by 1 period. The first value becomes NaN *)
(* and each subsequent value is the previous day's close. *)

(* %% *)
let shifted = Series.shift 1 prices

let () =
  let ov = Nx.to_array (Series.values prices) in
  let sv = Nx.to_array (Series.values shifted) in
  Printf.printf "Original vs Shifted (first 5):\n";
  for i = 0 to 4 do
    Printf.printf "  [%d]: %.1f -> %.1f\n" i ov.(i) sv.(i)
  done

(* %% [markdown] *)
(* ## Inspecting Series and Frame *)
(* *)
(* Use `Series.head`, `Series.tail`, and `Series.first_valid` to peek at *)
(* the bands, and `Frame.describe` for summary statistics. *)

(* %% *)
let () =
  Printf.printf "--- Series.head / tail ---\n";
  Notebook_helpers.pp_series ~n:5 "upper (head 5)" (Series.head 5 upper_band);
  Notebook_helpers.pp_series ~n:5 "lower (tail 5)" (Series.tail 5 lower_band)

(* %% *)
let () =
  Printf.printf "--- Series.first_valid ---\n";
  Notebook_helpers.pp_first_valid "upper_band" upper_band;
  Notebook_helpers.pp_first_valid "lower_band" lower_band

(* %% *)
let band_frame =
  match
    Frame.of_series
      [
        ("price", prices);
        ("sma_20", sma_20);
        ("upper", upper_band);
        ("lower", lower_band);
      ]
  with
  | Ok f -> f
  | Error e -> failwith (Printf.sprintf "Band frame construction failed: %s" e)

let () =
  Printf.printf "--- Frame.describe ---\n";
  Notebook_helpers.pp_describe band_frame

(* %% [markdown] *)
(* ## Next Steps *)
(* *)
(* TODO(e02-finance): Compute percent B and bandwidth from Bollinger Bands *)
(* TODO(e03-plot): Add band overlay chart with price, upper, and lower bands *)
