(** Financial metrics for Cairos time-series.

    Scalar metrics computed over returns series, validated against Pandas
    reference implementations via the cross-validation harness in
    [test/unit/cairos_finance/]. All functions take a returns series (typically
    the output of {!Cairos.Series.pct_change}); see each function's docstring
    for the precise contract on NaN and degenerate inputs. *)

val cumulative_return : ('freq, (float, 'b) Nx.t) Cairos.Series.t -> float
(** [cumulative_return returns] computes the total return of a returns series.

    Formula: [product(1 + ri) - 1] for all non-NaN [ri] in the series. NaN
    values (e.g. the first element from {!Cairos.Series.pct_change}) are
    skipped. If the series is empty or all-NaN, returns [0.0] (empty product is
    [1.0]).

    The input must be a returns series (percentage changes), not a price series.
    Use {!Cairos.Series.pct_change} to convert prices to returns first. *)

val annualised_return : ('freq, (float, 'b) Nx.t) Cairos.Series.t -> float
(** [annualised_return returns] scales the cumulative return to a per-year
    figure using the frequency-aware annualization factor.

    Formula: [(1 + cumulative_return) ** (annualization_factor / n) - 1] where
    [n] is the number of non-NaN return periods and [annualization_factor] is
    derived from the series frequency (Day→252, Hour→1638, Minute→98280,
    Week→52).

    NaN values are excluded from both the cumulative return calculation and the
    period count [n]. If [n = 0] (empty or all-NaN), returns [nan].

    The input must be a returns series, not a price series. *)

val annualised_vol : ('freq, (float, 'b) Nx.t) Cairos.Series.t -> float
(** [annualised_vol returns] is the annualised standard deviation of a returns
    series.

    Formula: [std(returns, ddof=1) * sqrt(annualization_factor)] over the
    non-NaN elements of the series, where [annualization_factor] is derived from
    the series frequency (Day→252, Hour→1638, Minute→98280, Week→52).

    Sample standard deviation (ddof=1) is used to match Pandas [.std()]
    convention. NaN values are skipped (matching {!Cairos.Series.pct_change}
    leading-NaN convention). If fewer than 2 non-NaN values are present, returns
    [nan] (sample std is undefined for n < 2).

    The input must be a returns series, not a price series. *)

val sharpe :
  risk_free:float -> ('freq, (float, 'b) Nx.t) Cairos.Series.t -> float
(** [sharpe ~risk_free returns] is the annualised Sharpe ratio of a returns
    series.

    [risk_free] is a required annualised rate. It is required (not optional)
    because it is a behavioural parameter — it changes the numeric output — and
    behavioural parameters must be explicit at every call site. Callers that
    want the classical zero-rate Sharpe pass [~risk_free:0.0] explicitly.

    [risk_free] is converted to a per-period rate geometrically:
    [rf_per_period = (1 +. risk_free) ** (1.0 /. ann_factor) -. 1.0], consistent
    with the geometric compounding used by {!annualised_return}.

    Formula: [excess = returns -. rf_per_period], then
    [sharpe = mean(excess) /. std(excess, ddof=1) *. sqrt(ann_factor)].

    The standard deviation is computed on the excess returns (textbook
    definition). For a constant scalar [risk_free], this is numerically
    identical to [std(returns)] but the textbook form is preserved for
    forward-compatibility with a time-varying risk-free series.

    NaN values in [returns] are skipped. Returns [nan] when fewer than 2 non-NaN
    values are present, or when the std of excess returns is [0.0] (a constant
    series). *)

val drawdown_series :
  ('freq, (float, 'b) Nx.t) Cairos.Series.t ->
  ('freq, (float, Bigarray.float64_elt) Nx.t) Cairos.Series.t
(** [drawdown_series returns] is the per-element drawdown of a returns series,
    defined as [(wealth - peak) / peak] where [wealth = cumprod(1 +. r)] and
    [peak = cummax(wealth)].

    The output series has the same length and the same {!Cairos.Index.t} as the
    input. Output values are non-positive: [0.0] when the wealth index is at a
    new high, and a negative fraction when underwater.

    The caller is responsible for dropping the leading NaN produced by
    {!Cairos.Series.pct_change} before calling this function. A series-returning
    metric must produce an output index aligned with its input; silently
    dropping the leading NaN would break that alignment. The standard call site
    is:
    {[
      let returns =
        Cairos.Series.pct_change prices |> fun r ->
        Cairos.Series.slice ~start:1 ~stop:(Cairos.Series.length r) r
      in
      Cairos_finance.drawdown_series returns
    ]}

    Behaviour is undefined if the input contains NaN. *)

val max_drawdown : ('freq, (float, 'b) Nx.t) Cairos.Series.t -> float
(** [max_drawdown returns] is the magnitude of the worst peak-to-trough decline
    of the wealth index implied by [returns], returned as a non-negative
    fraction in [0.0, 1.0].

    For a flat returns series (all zeros), the wealth index is constant at
    [1.0], the drawdown series is identically [0.0], and [max_drawdown] returns
    [0.0] — not [nan]. This is the financially correct answer (a strategy that
    never lost has zero drawdown) and matches the Pandas reference directly.

    The caller is responsible for dropping the leading NaN produced by
    {!Cairos.Series.pct_change} before calling this function. Behaviour is
    undefined if the input contains NaN. *)
