(* Property suite for [Cairos.Window]. RFC 0046 TP-Window-1..4.

   Each property runs at [~count:200] per RFC 0046 Test Plan; CI/local
   reproducibility is provided by [Qcheck_gen.pin_seed_from_env] (FR-4).

   Per Appendix A row 5 and ~/.claude/solutions/ocaml/cairos-series-map-is-tensor-level.md,
   [Window.rolling] / [Window.expanding] take an [(float, 'b) Nx.t -> float]
   reducer (not a [float -> float]) — closures below operate on the window
   as a whole tensor and extract a scalar via [Nx.item]. *)

let mean_f w = Nx.item [] (Nx.mean w)
let max_f w = Nx.item [] (Nx.max w)

(* TP-Window-1 — [Window.rolling ~n f s] preserves the input length for any
   [n >= 1]. The output index is the input's index; only the values payload
   is rewritten. Catches a regression that allocates an output of a different
   shape (e.g. only the [n-1..len-1] tail). [n] ranges over [1, 32]; values
   above [length s] still satisfy the contract because [rolling] then yields
   an all-NaN array of the same length. *)
let series_with_rolling_n_arb =
  let open QCheck in
  let gen =
    let open Gen in
    let* s = get_gen Qcheck_gen.daily_finite_float_series_arb in
    let* n = int_range 1 32 in
    return (s, n)
  in
  make
    ~print:(fun (s, n) ->
      Printf.sprintf "<series len=%d rolling n=%d>" (Cairos.Series.length s) n)
    gen

let rolling_preserves_length =
  QCheck.Test.make ~count:200 ~name:"rolling_preserves_length"
    series_with_rolling_n_arb (fun (s, n) ->
      Cairos.Series.length (Cairos.Window.rolling ~n mean_f s)
      = Cairos.Series.length s)

(* TP-Window-2 — [Window.rolling ~n] seeds the first [n - 1] positions with
   [Float.nan] (strict warmup, per window.mli:13-15). [n] is restricted to
   [n >= 2] so a warmup region exists, and [QCheck.assume (n <= length s)]
   restricts to inputs where positions [0, n-1) lie inside the array. Catches
   a regression that fills warmup positions with [0.0] or with the partial
   reduce-of-prefix value. *)
let series_with_warmup_n_arb =
  let open QCheck in
  let gen =
    let open Gen in
    let* s = get_gen Qcheck_gen.daily_finite_float_series_arb in
    let* n = int_range 2 32 in
    return (s, n)
  in
  make
    ~print:(fun (s, n) ->
      Printf.sprintf "<series len=%d warmup n=%d>" (Cairos.Series.length s) n)
    gen

let rolling_warmup_is_nan =
  QCheck.Test.make ~count:200 ~name:"rolling_warmup_is_nan"
    series_with_warmup_n_arb (fun (s, n) ->
      QCheck.assume (n <= Cairos.Series.length s);
      let result = Cairos.Window.rolling ~n mean_f s in
      let arr = Nx.to_array (Cairos.Series.values result) in
      arr
      |> Array.to_seqi
      |> Seq.for_all (fun (i, x) -> i >= n - 1 || Float.is_nan x))

(* TP-Window-3 — [rolling ~n:1] with the Nx-tensor identity reducer recovers
   the input values bitwise. The reducer's signature is
   [(float, 'b) Nx.t -> float] (Appendix A row 5), so [Fn.id] would be a
   type error.

   The RFC §Test Plan and Appendix A both spell the closure as
   [(fun w -> Nx.item [] w)], which is a transcription error: [Nx.item]
   requires one index per tensor dimension (nx.mli:1179, frontend.ml:3336-3339
   raises [Invalid_argument "item: need 1 indices for 1-d tensor, got 0"]).
   For [n=1], the window [w] is a 1-D length-1 slice of the input values
   (lib/window.ml:7 takes [Nx.slice [R (i, i+1)] vals], a length-preserving
   range slice), so the identity reducer is [(fun w -> Nx.item [0] w)].
   This honours the invariant exactly — n=1 rolling output is bitwise equal
   to the input — and matches the precedent set in TP-Series-3's reflections
   (RFC §Test Plan code listing typos are corrected in the test, not
   propagated). Catches a regression that mishandles [n=1] by drifting from
   the input (e.g. allocating a fresh tensor whose NaN bit-pattern differs). *)
let rolling_n1_is_identity =
  QCheck.Test.make ~count:200 ~name:"rolling_n1_is_identity"
    Qcheck_gen.daily_finite_float_series_arb (fun s ->
      let result = Cairos.Window.rolling ~n:1 (fun w -> Nx.item [ 0 ] w) s in
      Qcheck_gen.float_arrays_bitwise_equal
        (Nx.to_array (Cairos.Series.values result))
        (Nx.to_array (Cairos.Series.values s)))

(* TP-Window-4 — [Window.expanding] with [Nx.max] over a non-negative input
   yields a non-decreasing output: each prefix's maximum is at least the
   previous prefix's maximum because the previous prefix is contained in
   the current one. Catches a regression where [expanding] processes
   windows in the wrong order or drops entries from the prefix. *)
let expanding_max_is_non_decreasing =
  QCheck.Test.make ~count:200 ~name:"expanding_max_is_non_decreasing"
    Qcheck_gen.daily_non_negative_series_arb (fun s ->
      let result = Cairos.Window.expanding max_f s in
      let arr = Nx.to_array (Cairos.Series.values result) in
      arr
      |> Array.to_seqi
      |> Seq.for_all (fun (i, x) -> i = 0 || arr.(i - 1) <= x))

let () =
  Qcheck_gen.pin_seed_from_env ();
  let tests =
    List.map QCheck_alcotest.to_alcotest
      [
        rolling_preserves_length;
        rolling_warmup_is_nan;
        rolling_n1_is_identity;
        expanding_max_is_non_decreasing;
      ]
  in
  Alcotest.run "Window.props" [ ("property", tests) ]
