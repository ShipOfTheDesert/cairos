(** Rolling and expanding window operations over float series.

    All operations preserve series length — the output has the same index as the
    input. Rolling operations fill the first [n-1] positions with [Float.nan]
    (strict NaN warmup). Expanding operations produce a value at every position.
*)

val rolling :
  n:int ->
  ((float, 'b) Nx.t -> float) ->
  ('freq, (float, 'b) Nx.t) Series.t ->
  ('freq, (float, Bigarray.float64_elt) Nx.t) Series.t
(** [rolling ~n f series] applies [f] to each contiguous window of size [n].
    Position [i] (for [i >= n-1]) receives [f] applied to the sub-tensor from
    [i - n + 1] to [i] inclusive. The first [n-1] positions are [Float.nan].

    When [n <= 0], all positions are [Float.nan]. When [n > length series], all
    positions are [Float.nan]. When the series is empty, returns an empty
    series. *)

val expanding :
  ((float, 'b) Nx.t -> float) ->
  ('freq, (float, 'b) Nx.t) Series.t ->
  ('freq, (float, Bigarray.float64_elt) Nx.t) Series.t
(** [expanding f series] applies [f] to a growing window starting from position
    0. Position [i] receives [f] applied to the sub-tensor from [0] to [i]
    inclusive. All positions produce values (no NaN warmup).

    When the series is empty, returns an empty series. *)

val sma :
  n:int ->
  ('freq, (float, 'b) Nx.t) Series.t ->
  ('freq, (float, Bigarray.float64_elt) Nx.t) Series.t
(** [sma ~n series] computes the simple moving average with window size [n].
    Equivalent to [rolling ~n (fun w -> Nx.mean w |> Nx.item []) series]. *)

val rolling_std :
  n:int ->
  ('freq, (float, 'b) Nx.t) Series.t ->
  ('freq, (float, Bigarray.float64_elt) Nx.t) Series.t
(** [rolling_std ~n series] computes rolling population standard deviation
    (denominator N, not N-1) with window size [n]. Equivalent to
    [rolling ~n (fun w -> Nx.std w |> Nx.item []) series]. *)

val rolling_min :
  n:int ->
  ('freq, (float, 'b) Nx.t) Series.t ->
  ('freq, (float, Bigarray.float64_elt) Nx.t) Series.t
(** [rolling_min ~n series] computes rolling minimum with window size [n].
    Equivalent to [rolling ~n (fun w -> Nx.min w |> Nx.item []) series]. *)

val rolling_max :
  n:int ->
  ('freq, (float, 'b) Nx.t) Series.t ->
  ('freq, (float, Bigarray.float64_elt) Nx.t) Series.t
(** [rolling_max ~n series] computes rolling maximum with window size [n].
    Equivalent to [rolling ~n (fun w -> Nx.max w |> Nx.item []) series]. *)
