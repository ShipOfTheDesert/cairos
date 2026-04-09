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
