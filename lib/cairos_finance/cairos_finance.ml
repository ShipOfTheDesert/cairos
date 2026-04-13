(* Annualization factor lookup. Internal — not exposed in the .mli.

   Locally abstract type annotation forces the compiler to enforce
   exhaustiveness on the GADT without a wildcard branch. If a new
   constructor is added to [Cairos.Freq.t], this match becomes a hard
   compile error rather than silently selecting a default. *)
let annualization_factor (type freq) (f : freq Cairos.Freq.t) : float =
  match f with
  | Cairos.Freq.Day -> 252.0
  | Cairos.Freq.Hour -> 1638.0
  | Cairos.Freq.Minute -> 98280.0
  | Cairos.Freq.Week -> 52.0

let cumulative_return (returns : ('freq, (float, 'b) Nx.t) Cairos.Series.t) :
    float =
  let arr = Nx.to_array (Cairos.Series.values returns) in
  let product =
    Array.fold_left
      (fun acc r -> if Float.is_nan r then acc else acc *. (1.0 +. r))
      1.0 arr
  in
  product -. 1.0

(* Welford's online algorithm for the mean and sample (ddof=1) standard
   deviation of an array, skipping NaN. Single pass, no cancellation risk.
   Returns [(n, nan, nan)] when fewer than two non-NaN values are present —
   sample std is undefined for [n < 2]. Internal; not exposed in the .mli. *)
let sample_mean_std_skipna (arr : float array) : int * float * float =
  let n = ref 0 in
  let mean = ref 0.0 in
  let m2 = ref 0.0 in
  Array.iter
    (fun x ->
      if not (Float.is_nan x) then (
        incr n;
        let delta = x -. !mean in
        mean := !mean +. (delta /. float_of_int !n);
        let delta2 = x -. !mean in
        m2 := !m2 +. (delta *. delta2)))
    arr;
  if !n < 2 then (!n, Float.nan, Float.nan)
  else
    let variance = !m2 /. float_of_int (!n - 1) in
    (!n, !mean, Float.sqrt variance)

let annualised_return (returns : ('freq, (float, 'b) Nx.t) Cairos.Series.t) :
    float =
  (* Single-pass fold: accumulate the (1 + r) product and the count of
     non-NaN periods together. Reusing [cumulative_return] would require a
     second pass to compute [n] (see RFC §"Options Considered" Option C). *)
  let arr = Nx.to_array (Cairos.Series.values returns) in
  let len = Array.length arr in
  let product = ref 1.0 in
  let n = ref 0 in
  for i = 0 to len - 1 do
    let r = arr.(i) in
    if not (Float.is_nan r) then (
      product := !product *. (1.0 +. r);
      incr n)
  done;
  if !n = 0 then Float.nan
  else
    let cum_ret = !product -. 1.0 in
    let ann_factor =
      annualization_factor (Cairos.Index.freq (Cairos.Series.index returns))
    in
    ((1.0 +. cum_ret) ** (ann_factor /. float_of_int !n)) -. 1.0

let annualised_vol (returns : ('freq, (float, 'b) Nx.t) Cairos.Series.t) : float
    =
  let arr = Nx.to_array (Cairos.Series.values returns) in
  let n, _mean, std = sample_mean_std_skipna arr in
  if n < 2 then Float.nan
  else
    let ann_factor =
      annualization_factor (Cairos.Index.freq (Cairos.Series.index returns))
    in
    std *. Float.sqrt ann_factor

let sharpe ~(risk_free : float)
    (returns : ('freq, (float, 'b) Nx.t) Cairos.Series.t) : float =
  let arr = Nx.to_array (Cairos.Series.values returns) in
  let ann_factor =
    annualization_factor (Cairos.Index.freq (Cairos.Series.index returns))
  in
  let rf_per_period = ((1.0 +. risk_free) ** (1.0 /. ann_factor)) -. 1.0 in
  let excess =
    Array.map
      (fun r -> if Float.is_nan r then Float.nan else r -. rf_per_period)
      arr
  in
  let n, mean, std = sample_mean_std_skipna excess in
  if n < 2 || Float.equal std 0.0 then Float.nan
  else mean /. std *. Float.sqrt ann_factor

(* Internal helper — drawdown over a NaN-free returns array.
   Single-pass loop threading wealth and peak scalars.
   Not exposed in the .mli. *)
let drawdown_array (returns : float array) : float array =
  let n = Array.length returns in
  if n = 0 then [||]
  else
    let out = Array.make n 0.0 in
    let wealth = ref (1.0 +. returns.(0)) in
    let peak = ref !wealth in
    out.(0) <- (!wealth -. !peak) /. !peak;
    for i = 1 to n - 1 do
      wealth := !wealth *. (1.0 +. returns.(i));
      if !wealth > !peak then peak := !wealth;
      out.(i) <- (!wealth -. !peak) /. !peak
    done;
    out

let drawdown_series (returns : ('freq, (float, 'b) Nx.t) Cairos.Series.t) :
    ('freq, (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t =
  let arr = Nx.to_array (Cairos.Series.values returns) in
  let dd_arr = drawdown_array arr in
  let dd_nx = Nx.create Nx.float64 [| Array.length dd_arr |] dd_arr in
  Cairos.Series.make_unsafe (Cairos.Series.index returns) dd_nx

let max_drawdown (returns : ('freq, (float, 'b) Nx.t) Cairos.Series.t) : float =
  let arr = Nx.to_array (Cairos.Series.values returns) in
  let n = Array.length arr in
  if n = 0 then 0.0
  else
    let wealth = ref (1.0 +. arr.(0)) in
    let peak = ref !wealth in
    let min_dd = ref 0.0 in
    for i = 1 to n - 1 do
      wealth := !wealth *. (1.0 +. arr.(i));
      if !wealth > !peak then peak := !wealth;
      let dd = (!wealth -. !peak) /. !peak in
      if dd < !min_dd then min_dd := dd
    done;
    Float.abs !min_dd
