# /// script
# dependencies = ["pandas>=2.0", "numpy>=1.26"]
# requires-python = ">=3.12"
# ///

import math
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd

FIXTURES = Path(__file__).parent / "fixtures"

SERIES = {
    "normal": [100.0, 101.5, 99.8, 102.3, 104.1, 103.0, 105.5, 107.2, 106.8, 109.0],
    "drawdown": [100.0, 105.0, 110.0, 95.0, 80.0, 85.0, 90.0, 88.0, 92.0, 95.0],
    "flat": [100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0],
    "extreme": [100.0, 200.0, 50.0, 150.0, 25.0, 175.0, 10.0, 300.0, 5.0, 500.0],
}


def write_csv(name: str, values: list[float]) -> None:
    path = FIXTURES / f"{name}.csv"
    with open(path, "w") as f:
        for v in values:
            f.write(f"{v:.17g}\n")


def write_scalar(name: str, value: float) -> None:
    write_csv(name, [value])


def write_series_csv(name: str, rows: list[tuple[int, float]]) -> None:
    path = FIXTURES / f"{name}.csv"
    with open(path, "w") as f:
        f.write("index,value\n")
        for idx, val in rows:
            f.write(f"{idx},{val:.17g}\n")


def cumulative_return(prices: list[float]) -> float:
    returns = pd.Series(prices).pct_change()
    return float((1 + returns).prod() - 1)


def annualised_return(prices: list[float], ann_factor: float = 252.0) -> float:
    returns = pd.Series(prices).pct_change().dropna()
    n = len(returns)
    cum_ret = float((1 + returns).prod() - 1)
    return float((1 + cum_ret) ** (ann_factor / n) - 1)


def annualised_vol(prices: list[float], ann_factor: float = 252.0) -> float:
    returns = pd.Series(prices).pct_change().dropna()
    # pd.Series.std() defaults to ddof=1 (sample std). This MUST stay ddof=1
    # to match the OCaml implementation and the chosen Pandas-aligned
    # convention. Do not switch to NumPy .std() without
    # passing ddof=1 explicitly — NumPy defaults to ddof=0.
    return float(returns.std() * math.sqrt(ann_factor))


def sharpe(
    prices: list[float],
    risk_free: float,
    ann_factor: float = 252.0,
) -> float:
    returns = pd.Series(prices).pct_change().dropna()
    rf_per_period = (1 + risk_free) ** (1 / ann_factor) - 1
    excess = returns - rf_per_period
    # ddof=1 — see comment in annualised_vol.
    std = excess.std()
    # Constant series (std == 0) yields nan, matching
    # the OCaml semantic. Without this guard, Pandas would return ±inf
    # whenever excess.mean() != 0 (e.g. flat returns with risk_free > 0).
    if len(excess) < 2 or std == 0.0:
        return float("nan")
    return float(excess.mean() / std * math.sqrt(ann_factor))


def drawdown_series(prices: list[float]) -> list[tuple[int, float]]:
    returns = pd.Series(prices).pct_change().dropna().reset_index(drop=True)
    wealth = (1 + returns).cumprod()
    peak = wealth.cummax()
    dd = (wealth - peak) / peak
    return list(enumerate(dd.tolist()))


def max_drawdown(prices: list[float]) -> float:
    returns = pd.Series(prices).pct_change().dropna()
    wealth = (1 + returns).cumprod()
    peak = wealth.cummax()
    dd = (wealth - peak) / peak
    # Returned as a positive magnitude.
    # Flat series produces dd = [0,0,...], min = 0.0, abs = 0.0
    # — no special-case guard needed; Pandas yields 0.0 directly.
    return float(abs(dd.min()))


def cumprod_series(prices: list[float]) -> list[tuple[int, float]]:
    returns = pd.Series(prices).pct_change().dropna().reset_index(drop=True)
    result = (1 + returns).cumprod()
    return list(enumerate(result.tolist()))


def cumsum_series(prices: list[float]) -> list[tuple[int, float]]:
    returns = pd.Series(prices).pct_change().dropna().reset_index(drop=True)
    result = returns.cumsum()
    return list(enumerate(result.tolist()))


# --- Frame cross-sectional fixtures --------------------------------------------
#
# Encodes the cross-sectional contract: rank uses method='average' tie-breaking,
# zscore uses ddof=1, NaN cells passthrough and are excluded from N. The oracle
# encodes the contract regardless of whether Pandas defaults coincidentally
# agree: the contract specifies all-NaN output rows for std=0
# (constant row) and N<2 (≤1 non-NaN cell), so [frame_zscore] explicitly
# masks those rows after the arithmetic — independent of whether [df.std]
# returns 0 or NaN for a particular fixture shape.

NAN = float("nan")

FRAME_XSEC_INDEX = [
    "2026-01-01",
    "2026-01-02",
    "2026-01-03",
    "2026-01-04",
    "2026-01-05",
    "2026-01-06",
    "2026-01-07",
    "2026-01-08",
    "2026-01-09",
    "2026-01-10",
]

FRAME_XSEC_FIXTURES: dict[str, dict] = {
    # Distinct-and-tie mix; bounded to [-100, 100].
    "full": {
        "columns": ["a", "b", "c", "d"],
        "rows": [
            [1.0, 2.0, 3.0, 4.0],
            [4.0, 3.0, 2.0, 1.0],
            [-2.5, 7.5, 0.0, 12.5],
            [1.0, 2.0, 2.0, 3.0],
            [5.0, -5.0, 5.0, -5.0],
            [10.0, 20.0, 30.0, 40.0],
            [-10.0, -20.0, -30.0, -40.0],
            [1.5, 2.5, 2.5, 2.5],
            [100.0, -100.0, 50.0, -50.0],
            [-3.14, 2.71, 1.41, -1.0],
        ],
    },
    # One row carries a single NaN — exercises NaN passthrough/exclusion-from-N.
    "partial_nan": {
        "columns": ["a", "b", "c", "d"],
        "rows": [
            [1.0, 2.0, 3.0, 4.0],
            [4.0, 3.0, 2.0, 1.0],
            [-2.5, 7.5, 0.0, 12.5],
            [1.0, 2.0, 2.0, 3.0],
            [5.0, -5.0, NAN, -5.0],
            [10.0, 20.0, 30.0, 40.0],
            [-10.0, -20.0, -30.0, -40.0],
            [1.5, 2.5, 2.5, 2.5],
            [100.0, -100.0, 50.0, -50.0],
            [-3.14, 2.71, 1.41, -1.0],
        ],
    },
    # Row 7 is a constant non-NaN row — std=0 → zscore all-NaN.
    "constant_row": {
        "columns": ["a", "b", "c", "d"],
        "rows": [
            [1.0, 2.0, 3.0, 4.0],
            [4.0, 3.0, 2.0, 1.0],
            [-2.5, 7.5, 0.0, 12.5],
            [1.0, 2.0, 2.0, 3.0],
            [5.0, -5.0, 5.0, -5.0],
            [10.0, 20.0, 30.0, 40.0],
            [5.0, 5.0, 5.0, 5.0],
            [1.5, 2.5, 2.5, 2.5],
            [100.0, -100.0, 50.0, -50.0],
            [-3.14, 2.71, 1.41, -1.0],
        ],
    },
    # 10×1 — every row has N≤1 non-NaN cell. rank → 1.0 for non-NaN rows,
    # NaN for the NaN row; zscore → all-NaN (≤1-non-NaN-cell row).
    "single_column": {
        "columns": ["a"],
        "rows": [
            [1.0],
            [2.5],
            [-3.0],
            [4.2],
            [NAN],
            [6.0],
            [-7.5],
            [8.0],
            [9.0],
            [0.5],
        ],
    },
}


def _format_cell(v: float) -> str:
    # NaN encoded as empty cell to match Cairos_io.frame_of_csv parsing
    # (lib/cairos_io/cairos_io.ml: empty between commas → Float.nan).
    if pd.isna(v):
        return ""
    return f"{float(v):.17g}"


def write_frame_csv(
    name: str,
    columns: list[str],
    dates: list[str],
    rows: list[list[float]],
) -> None:
    path = FIXTURES / f"{name}.csv"
    with open(path, "w") as f:
        f.write("timestamp," + ",".join(columns) + "\n")
        for d, row in zip(dates, rows):
            f.write(",".join([d] + [_format_cell(v) for v in row]) + "\n")


def write_frame_expected(
    name: str,
    columns: list[str],
    dates: list[str],
    df: pd.DataFrame,
) -> None:
    path = FIXTURES / f"{name}.csv"
    with open(path, "w") as f:
        f.write("timestamp," + ",".join(columns) + "\n")
        for d, (_, row) in zip(dates, df.iterrows()):
            cells = [d] + [_format_cell(row[col]) for col in columns]
            f.write(",".join(cells) + "\n")


def frame_rank(rows: list[list[float]], columns: list[str]) -> pd.DataFrame:
    df = pd.DataFrame(rows, columns=columns)
    # method='average' (Pandas-aligned tie-breaking).
    return df.rank(axis=1, method="average")


def frame_zscore(rows: list[list[float]], columns: list[str]) -> pd.DataFrame:
    df = pd.DataFrame(rows, columns=columns)
    # ddof=1 — see comment in annualised_vol.
    n_per_row = df.notna().sum(axis=1)
    mean = df.mean(axis=1, skipna=True)
    std = df.std(axis=1, ddof=1, skipna=True)
    result = df.sub(mean, axis=0).div(std, axis=0)
    # Contract: rows with N<2 or std=0 produce all-NaN output. Apply
    # explicitly rather than relying on Pandas' default arithmetic happening
    # to emit NaN for these shapes.
    invalid_row = (n_per_row < 2) | std.eq(0.0)
    result.loc[invalid_row] = float("nan")
    return result


# --- Daily->monthly resample fixtures ------------------------------------------
#
# Oracle for the monthly resample (Freq.Month) Layer 2 cross-validation of
# Cairos.Resample.resample from daily to monthly. Authored independently from
# the stated bucketing convention below, NOT transcribed from lib/resample.ml,
# so the oracle can catch an implementation bug rather than mirror one:
#
#   Monthly buckets are keyed on the timestamp's calendar (year, month) and
#   labelled bucket-start — the label is the FIRST instant of the calendar
#   month at 00:00 UTC, synthesised (the anchor need not appear in the source
#   index). Empty buckets are omitted; the output length equals the number of
#   non-empty (year, month) buckets.
#
# Pandas mapping: resample('MS') anchors each bucket to the month start, which
# matches the bucket-start convention (the same anchoring Week already uses,
# verified against the Weekly 'W-MON' precedent). Aggregations exercised: 'sum'
# and 'last' (order-sensitive, so an out-of-order bucket reduction is caught
# that 'sum' cannot see).
#
# EMPTY-BUCKET CONTRACT (encoded in the oracle, not the comparator):
# pandas emits every calendar month in the range, including ones no source row
# falls in; the Cairos contract omits a bucket exactly when it has NO SOURCE
# ROWS. The oracle drops periods by ROW COUNT (resample(...).size() == 0) — not
# by dropna(), and not by resample(...).count():
#
#   dropna() would delete a legitimately-NaN aggregate (a 'last' over a bucket
#   whose final observation is NaN).
#
#   .count() counts non-NaN values, so it reads 0 for a bucket whose every row
#   is NaN. That bucket is real under the contract and must be emitted — with
#   its NaN aggregate under 'last', or with 0.0 under the count aggregation.
#
# Both would conflate "no rows" with "rows that are all NaN". Corrected from
# .count() to .size() on 2026-08-03, when the mixed-NaN fixture below made the
# two diverge for the first time. The business-day fixture is NaN-free and has
# a row in every month it spans, so its emitted values are unchanged by the
# correction.

RESAMPLE_DAILY_SEED = 62
RESAMPLE_START = "2024-01-02"
RESAMPLE_END = "2025-03-14"


def resample_daily_input() -> pd.Series:
    # Business days Mon-Fri (bdate_range, no holiday calendar) spanning
    # leap February 2024, every 28/29/30/31-day month length, the
    # Dec 2024 -> Jan 2025 year boundary, and a final month (March 2025)
    # truncated mid-month at 2025-03-14. Values from a fixed-seed RNG so the
    # fixture is deterministic under the pinned uv.lock.
    dates = pd.bdate_range(RESAMPLE_START, RESAMPLE_END)
    rng = np.random.default_rng(RESAMPLE_DAILY_SEED)
    values = rng.normal(0.0, 1.0, len(dates))
    return pd.Series(values, index=dates)


def resample_monthly_reference(daily: pd.Series, agg: str) -> pd.Series:
    # agg in {"sum", "last", "count"}. Bucket-start anchoring via resample('MS').
    resampled = daily.resample("MS").agg(agg)
    rows_per_bucket = daily.resample("MS").size()
    return resampled[rows_per_bucket > 0]


# --- Mixed-NaN daily->monthly count fixture ------------------------------------
#
# Input for the count aggregation's Layer 2 cross-validation. This is a fixture
# INPUT, not a reference implementation: the expected output is pandas' own
# resample('MS').count(), and there is no algorithm here that could be derived
# independently of anything. What the input has to do is reach a case the
# business-day fixture above cannot. That one is NaN-free (rng.normal over
# bdate_range), so count == size in every one of its buckets and the non-NaN
# rule it is supposed to pin is never exercised by it.
#
# Small enough to check by eye. Per calendar month:
#
#   2025-01  three rows, one NaN in the interior  -> count 2
#   2025-02  two rows, leading NaN                -> count 1
#   2025-03  no rows at all                       -> bucket OMITTED
#   2025-04  three rows, trailing NaN             -> count 2
#   2025-05  two rows, both NaN                   -> count 0, bucket EMITTED
#
# The last two months are the pair the contract distinguishes and pandas'
# .count() does not. A bucket with no source rows is omitted entirely; a bucket
# whose every row is NaN is a real bucket that reports zero observations. See
# the empty-bucket contract note above resample_monthly_reference.

RESAMPLE_NAN_DAILY: list[tuple[str, float]] = [
    ("2025-01-05", 1.0),
    ("2025-01-06", NAN),
    ("2025-01-07", 2.0),
    ("2025-02-03", NAN),
    ("2025-02-04", 4.0),
    ("2025-04-01", 5.0),
    ("2025-04-02", 6.0),
    ("2025-04-03", NAN),
    ("2025-05-06", NAN),
    ("2025-05-07", NAN),
]


def resample_nan_daily_input() -> pd.Series:
    dates = pd.to_datetime([d for d, _ in RESAMPLE_NAN_DAILY])
    return pd.Series([v for _, v in RESAMPLE_NAN_DAILY], index=dates)


def write_resample_series_csv(name: str, series: pd.Series) -> None:
    # timestamp,value shape with full RFC 3339 labels (T00:00:00Z) so the
    # OCaml harness parses them via Ptime.of_rfc3339 and the synthesised
    # month-start anchor is explicit.
    path = FIXTURES / f"{name}.csv"
    with open(path, "w") as f:
        f.write("timestamp,value\n")
        for ts, v in series.items():
            f.write(f"{_ts_str(ts)},{_format_cell(v)}\n")


# --- Backtest engine fixtures --------------------------------------------------

BACKTEST_INSTRUMENTS = ["A", "B", "C", "D", "E"]
BACKTEST_N_BARS = 50
# Weekly rebalances on a 50-bar daily fixture: bars [2, 7, 12, 17, 22, 27, 32,
# 37, 42, 47]. First rebalance at bar 2 (not bar 0) so equity_curve[0] == 1.0
# holds (first equity-curve cell is 1.0 only when bar 0 is
# not a rebalance). Last rebalance at bar 47 so exec_bar = 48 is valid (the
# engine's entrypoint precondition step 6 forbids a rebalance at the last
# bar). Both backtest_reference() and the OCaml cross_validate harness
# (test/unit/cairos_engine/cross_validate.ml) consume this schedule by index;
# it is hard-coded on both sides — there is no separate rebalance fixture.
BACKTEST_REBALANCE_BAR_INDICES = list(range(2, BACKTEST_N_BARS, 5))
BACKTEST_COMMISSION = 0.001
BACKTEST_SLIPPAGE = 0.0005
BACKTEST_PRICE_SEED = 42
BACKTEST_SIGNAL_SEED = 43
BACKTEST_START_DATE = "2024-01-01"


@dataclass
class _Trade:
    entry_timestamp: pd.Timestamp
    exit_timestamp: pd.Timestamp
    instrument: str
    entry_price: float
    exit_price: float
    pnl: float
    holding_period_bars: int


def backtest_dates() -> pd.DatetimeIndex:
    return pd.date_range(BACKTEST_START_DATE, periods=BACKTEST_N_BARS, freq="D")


def backtest_prices_df() -> pd.DataFrame:
    # Normalised prices anchored at 1.0. Under the turnover-notional cost
    # convention, cost is a fraction of pre-cost NAV
    # and is independent of the absolute price level, so the anchor is now a
    # plain normalisation choice, not a cost-collapse-avoidance measure: the
    # pre-fix execution_price formula drove per-rebalance cost into the
    # 1%-of-NAV range and compounded to NAV collapse only because it was
    # dimensioned in price. The price fixture itself is unchanged by A1 (this
    # generator is untouched); only the equity/returns/trade fixtures move,
    # because the cost line downstream now multiplies by nav_after_mtm.
    rng = np.random.default_rng(BACKTEST_PRICE_SEED)
    n_inst = len(BACKTEST_INSTRUMENTS)
    log_returns = rng.normal(0.0, 0.01, size=(BACKTEST_N_BARS - 1, n_inst))
    prices = np.empty((BACKTEST_N_BARS, n_inst))
    prices[0] = 1.0
    prices[1:] = np.exp(np.cumsum(log_returns, axis=0))
    return pd.DataFrame(prices, index=backtest_dates(), columns=BACKTEST_INSTRUMENTS)


def backtest_signals_df() -> pd.DataFrame:
    rng = np.random.default_rng(BACKTEST_SIGNAL_SEED)
    n_inst = len(BACKTEST_INSTRUMENTS)
    sig = np.zeros((BACKTEST_N_BARS, n_inst))
    for bar in BACKTEST_REBALANCE_BAR_INDICES:
        sig[bar] = rng.uniform(-0.5, 0.5, size=n_inst)
    return pd.DataFrame(sig, index=backtest_dates(), columns=BACKTEST_INSTRUMENTS)


# =============================================================================
# PROVENANCE: backtest_reference
# =============================================================================
# Origin. This reference was written from the backtest engine specification
# alone. Deliberately excluded and not available to the author: the OCaml
# implementation this oracle cross-validates, any earlier Python reference, and
# the implementation's own test suite. It is not a port or a transliteration of
# anything; its purpose is to be *able* to disagree with the implementation, and
# a reference derived from the implementation could only ever prove that the
# code equals its own port. Where the specification is silent, the silence is
# recorded at the bottom of this block rather than filled in with a guess at
# what an implementation "probably" does.
#
# -----------------------------------------------------------------------------
# What this encodes. Stated substantively: the specification documents are not
# committed to this repository, so a citation like "per clause N" would be
# unreadable to anyone here. The rules themselves:
#
#   Signal timing (close-to-open). A target weight read at rebalance bar r is
#   executed at the open of bar r+1. Every execution price and every entry/exit
#   timestamp of a rebalance-driven trade event is therefore taken from bar r+1,
#   never from bar r. The signal is computed from bar r's close, so filling at
#   bar r's close would trade on information only knowable once bar r ended.
#
#   Positions. Weights are fractions of current NAV. Negative is short, zero is
#   flat, magnitude above 1.0 is leverage. No range validation, no clamping. The
#   weights need not sum to 1.0; the unallocated remainder is cash and earns
#   nothing.
#
#   Costs. Charged at rebalance bars only. At rebalance bar r, per instrument j,
#   with w_held the weight carried into the bar and w_target the signal value,
#   dw = w_target - w_held and
#       cost_j     = (commission + slippage) * abs(dw_j) * nav_pre
#       total_cost = sum_j cost_j
#       nav_after  = nav_pre - total_cost
#   where nav_pre is the NAV at bar r *after* marking the previously held
#   weights from bar r-1 to bar r and *before* any deduction. Only then do the
#   held weights become the targets: new positions are funded out of nav_after,
#   not nav_pre. That ordering is load-bearing and is preserved here.
#
#   THE COST NOTIONAL IS TURNOVER IN NAV TERMS, NOT A PRICE LEVEL. abs(dw) is a
#   dimensionless fraction of NAV and (commission + slippage) is a dimensionless
#   fraction of notional, so the thing they multiply must be a money amount, and
#   the only money amount in scope is NAV. Two strategies identical except that
#   one instrument prints at 400 and the other at 4 incur *identical* costs.
#   Nothing in this function multiplies a cost by a price, and nobody should
#   "fix" it back toward a price-dimensioned formula. The equity path below
#   exploits exactly this: because total_cost is proportional to nav_pre, the
#   deduction is the scalar factor (1 - (commission + slippage) * turnover_r),
#   which is algebraically identical to the subtraction above.
#
#   Mark-to-market (weighted-return form). For the step from bar t-1 to bar t
#   with w_j the weight held across that step:
#       nav_t = nav_{t-1} * (1 + sum_j w_j * (p_{t,j} / p_{t-1,j} - 1))
#   This coincides with the "sum of weighted price relatives" form only when the
#   weights sum to 1.0. Unlike that form it stays positive for shorts and is
#   well defined when the book is not fully invested, both of which this input
#   distribution produces constantly. A zero-weight portfolio drifts trivially.
#
#   Equity curve. One value per price bar, N values for N bars, carrying the
#   price frame's index. equity[0] = 1.0 (NAV is normalised; there is no
#   currency). At a non-rebalance bar the value is the mark-to-market result
#   using the currently held weights. At a rebalance bar r the value is
#   nav_after: marked from r-1 to r with the *previously* held weights, then
#   reduced by the total cost. It is post-cost and pre-next-mark-to-market. New
#   targets take effect at bar r and govern the step r -> r+1. After the final
#   rebalance the held weights stay fixed and continue to be marked to market
#   through the end of the frame; that is intended, not a truncation.
#
#   Held weights. The weight in force for instrument j at bar t is the target of
#   the most recent rebalance bar <= t, and it governs the step t -> t+1. Bars
#   before the first rebalance carry 0.0. These are *held* weights, not signal
#   rows forward-filled: the two differ in general. Not returned; used
#   internally, and it is the panel the mark-to-market consumes.
#
#   Returns. Per-bar arithmetic change of the equity curve, same index and
#   length, first value NaN because there is no prior bar. Computed explicitly
#   as equity[t]/equity[t-1] - 1 rather than through a percentage-change helper,
#   so that no library fill/interpolate default can participate. The leading NaN
#   is a contractual sentinel that survives into the fixture as an empty cell.
#
#   Trades: one record per round trip per instrument. Inception is the held
#   weight going from zero to non-zero, or the opening side of a sign flip.
#   Resolution is the target going to zero, the closing side of a sign flip, or
#   the end of the run with the position still open. A same-direction size
#   adjustment (both non-zero, same sign, different magnitude) updates the
#   in-flight record and neither resolves it nor creates a new one: fifty size
#   adjustments before a close produce one trade, not fifty-one. A cost is
#   charged at every rebalance with non-zero dw regardless of which event it is.
#
#   Trade fields. entry_timestamp/entry_price come from the inception's
#   execution bar (r_inception + 1) and are never moved by an adjustment; an
#   adjustment's own execution price affects pnl and nothing else.
#   exit_timestamp/exit_price come from the resolution's execution bar
#   (r_resolution + 1), or from the last bar for a force-close.
#   holding_period_bars is the integer row distance exit_bar - entry_bar and is
#   not subdivided by adjustments.
#
#   Cost attribution. At an inception, an adjustment, or a plain resolution the
#   whole per-instrument cost belongs to the single affected trade. At a sign
#   flip the one per-instrument cost is split in proportion to each side's share
#   of the turnover:
#       closing_share = cost_j * abs(w_old) / (abs(w_old) + abs(w_new))
#       opening_share = cost_j * abs(w_new) / (abs(w_old) + abs(w_new))
#   For a flip abs(dw) == abs(w_old) + abs(w_new), so the shares sum to cost_j
#   exactly: nothing is lost and nothing is double-counted. Symmetric flips
#   split 50/50, asymmetric ones do not.
#
#   Force-close. Every instrument still holding a non-zero weight at the end of
#   the run emits a final record: exit at the last bar's timestamp, exit_price
#   the last bar's single price (its close), and NO cost, because costs are
#   incurred at rebalance bars and the end of the data is not one.
#
#   Ordering (part of the contract; comparison is positional). Records are
#   appended on resolution, so they appear in order of resolution bar, in price
#   frame column order within a bar, with all force-closed records last in
#   column order. A flip's closing record therefore is not adjacent to the
#   record it incepts.
#
# -----------------------------------------------------------------------------
# Independence claim, scoped honestly.
#
#   Equity / returns / held-weights path: STRUCTURAL independence. It is a
#   forward-filled weight matrix multiplied against a matrix of per-bar price
#   relatives, with the cost factor applied on the rebalance rows and a single
#   cumulative product producing the curve. There is no bar-by-bar loop. This is
#   not about speed (the fixture is fifty bars) - it is so that this path cannot
#   reproduce the implementation's control flow, and reaches the same numbers by
#   a different route or disagrees.
#
#   Trade scan: PROVENANCE-ONLY independence. It is sequential, and unavoidably
#   so: in-flight records mutated by adjustments, a flip resolving and incepting
#   at one execution bar, segment-summed P&L, force-close at the end. A
#   vectorised state machine would itself need validating. Its independence
#   rests solely on the fact that it was written from the specification with the
#   implementation unavailable. That is a weaker claim than the one above and
#   this comment does not pretend otherwise.
#
# -----------------------------------------------------------------------------
# Silences and tensions in the specification, and how they were resolved.
#
# (1) P&L: segment form vs. accrual form. The specification states pnl as a sum
#     over constant-weight segments, sum_k w_k * NAV_at_segment_entry_k *
#     (p_{i_{k+1}}/p_{i_k} - 1), and separately pins the post-condition that
#     1 + sum(pnl) equals the final equity value to a per-trade tolerance of
#     1e-12. Those two are not the same number. "NAV at segment entry" is frozen
#     for the whole segment, whereas the equity curve accrues each step against
#     the running NAV at that step; they agree only in special cases (a fully
#     invested single instrument, where nav_t/nav_entry telescopes to
#     p_t/p_entry, or a segment spanning one step). Away from those they differ,
#     and this input distribution is away from those on essentially every trade.
#     Resolution: the accrual reading is implemented, because it is the one that
#     satisfies the specification *as a whole*. The equity curve is pinned in
#     full by the cost, mark-to-market, and held-weight rules; the reconciliation
#     identity is asserted by the fixture generator and aborts the run when it
#     fails; so the only free quantity is pnl, and only the accrual reading
#     makes the identity hold. Concretely, each trade's pnl is the sum over the
#     mark-to-market steps it owns of nav_{t-1} * w_held * (p_t/p_{t-1} - 1),
#     minus its attributed costs. Because the trade spans partition, per
#     instrument and without overlap, exactly the steps where the held weight is
#     non-zero, and because every non-zero-dw cost is attributed to exactly one
#     trade (flips splitting to an exact sum), the total telescopes to the final
#     equity value identically rather than approximately. The segment structure
#     survives: a trade's step range is the union of its constant-weight
#     segments' steps, and summing per step is summing per segment regrouped.
#
# (2) A one-bar offset between the record fields and the P&L span, which follows
#     from (1) and is left in place deliberately. Execution timing puts a trade's
#     entry_price at bar r_inception+1, while the weight panel says the new
#     target takes effect at bar r and governs the step r -> r+1. So the position
#     earns p_{r+1}/p_r - 1 in the equity curve, but reports p_{r+1} as its entry
#     price. The single-price convention is what makes these irreconcilable:
#     with one value per (bar, instrument) serving as both close and open, the
#     "open of bar r+1" is p_{r+1}, not p_r, so the close-to-open story implies a
#     gap the weight panel never experiences. Both rules are pinned explicitly
#     and independently, so each is applied where it governs: bar r+1 for record
#     prices and timestamps and for holding_period_bars, the weight panel for
#     the accrual. CONSEQUENCE FOR READERS: pnl is *not* recoverable from
#     entry_price and exit_price. A test asserting pnl ~= w * nav *
#     (exit_price/entry_price - 1) would be asserting a rule the specification
#     does not contain. The same offset exists on the cost side, where the
#     deduction lands at bar r while the fill is priced at bar r+1.
#
# (3) A rebalance at bar 0 is not addressed. The cost rule would charge a cost
#     there (dw = target - 0), while the equity rule states equity[0] = 1.0
#     unconditionally and the post-condition asserts it. Resolution: the cost
#     rule is applied uniformly to every rebalance bar, so a rebalance at bar 0
#     would yield equity[0] = 1 - total_cost_0 and trip the assertion. That is
#     preferred over special-casing bar 0, because the justification given for
#     equity[0] == 1.0 is the initial state of a zero-weight portfolio, which is
#     a statement about the *pre-cost* NAV. Unreachable from the fixture inputs,
#     whose rebalance bars start at 2; if it ever becomes reachable the assertion
#     failure is the correct signal that the specification needs a ruling.
#
# (4) A rebalance whose target exactly equals the held weight (dw == 0, same
#     sign, same magnitude) is not named as an event. Treated as a no-op: zero
#     cost, no record created, no record resolved, the in-flight record's entry
#     fields untouched. This is the same-direction branch with a zero-size
#     adjustment and is the only reading consistent with adjustments not
#     resolving anything.
#
# (5) The specification says nothing about the pandas Series `name` attribute,
#     so none is set; the fixture writers pin the CSV headers themselves.
#
# (6) Degenerate inputs (NaN or non-positive prices, missing signal cells) are
#     out of scope: the real entry point validates inputs before calling, and
#     this function deliberately adds no re-checks that could mask a difference.
# =============================================================================


def backtest_reference(
    prices_df: pd.DataFrame,
    signals_df: pd.DataFrame,
    rebalance_bar_indices: list[int],
    commission: float,
    slippage: float,
) -> tuple[pd.Series, pd.Series, list[_Trade]]:
    index = prices_df.index
    instruments = list(prices_df.columns)
    n_bars = len(index)
    n_instruments = len(instruments)

    prices = prices_df.to_numpy(dtype=float)
    signals = signals_df.to_numpy(dtype=float)
    rebalance_bars = np.asarray(rebalance_bar_indices, dtype=np.int64)

    # Dimensionless fraction of notional, charged per unit of NAV turnover.
    cost_per_unit_turnover = float(commission) + float(slippage)

    # -------------------------------------------------------------------------
    # VECTORISED PATH (1/4): held weights.
    # held[t, j] is the target of the most recent rebalance bar <= t, or 0.0 for
    # bars preceding the first rebalance. It governs the step t -> t+1.
    # -------------------------------------------------------------------------
    held = np.zeros((n_bars, n_instruments), dtype=float)
    most_recent = np.searchsorted(rebalance_bars, np.arange(n_bars), side="right") - 1
    after_first_rebalance = most_recent >= 0
    held[after_first_rebalance] = signals[rebalance_bars[most_recent[after_first_rebalance]]]

    # held_into_bar[t] is the weight carried *into* bar t, i.e. the weight that
    # governs the mark-to-market step ending at bar t, and the w_held that the
    # cost rule differences against the new target at a rebalance bar.
    held_into_bar = np.vstack((np.zeros((1, n_instruments), dtype=float), held[:-1]))

    # -------------------------------------------------------------------------
    # VECTORISED PATH (2/4): per-bar price relatives, p_t / p_{t-1} - 1.
    # Row 0 is zero: there is no prior bar, and the weight carried into bar 0 is
    # zero anyway, so the zero never contributes.
    # -------------------------------------------------------------------------
    relatives = np.zeros((n_bars, n_instruments), dtype=float)
    relatives[1:] = prices[1:] / prices[:-1] - 1.0

    # -------------------------------------------------------------------------
    # VECTORISED PATH (3/4): costs, applied on the rebalance rows only.
    # -------------------------------------------------------------------------
    delta_weight = np.zeros((n_bars, n_instruments), dtype=float)
    delta_weight[rebalance_bars] = held[rebalance_bars] - held_into_bar[rebalance_bars]
    turnover = np.abs(delta_weight).sum(axis=1)

    # Fraction of pre-cost NAV removed at each bar; identically zero off the
    # rebalance rows. Note again: no price appears here.
    cost_fraction = cost_per_unit_turnover * turnover

    # -------------------------------------------------------------------------
    # VECTORISED PATH (4/4): equity curve and returns.
    #   gross_growth[t] = 1 + sum_j held_into_bar[t, j] * relatives[t, j]
    #   equity[t]       = equity[t-1] * gross_growth[t] * (1 - cost_fraction[t])
    # The second factor is the cost deduction: because total_cost is
    # cost_fraction * nav_pre, subtracting it and scaling by (1 - cost_fraction)
    # are the same operation. The subtraction form is recovered explicitly below
    # as cost_money, which the trade scan needs per instrument.
    # -------------------------------------------------------------------------
    gross_growth = 1.0 + (held_into_bar * relatives).sum(axis=1)
    equity_values = np.cumprod(gross_growth * (1.0 - cost_fraction))

    # NAV carried into each bar; 1.0 into bar 0 (normalised, zero-weight start).
    nav_into_bar = np.empty(n_bars, dtype=float)
    nav_into_bar[0] = 1.0
    nav_into_bar[1:] = equity_values[:-1]

    # Pre-cost NAV at each bar: marked to market with the previously held
    # weights, before any deduction. This is the notional the cost rule uses.
    nav_pre_cost = nav_into_bar * gross_growth

    # Money P&L contributed by instrument j over the step ending at bar t,
    # accrued against the running NAV. Summing this whole matrix gives the
    # equity curve's total gross gain, which is what makes the reconciliation
    # identity exact rather than approximate.
    contribution = nav_into_bar[:, None] * held_into_bar * relatives

    # Per-instrument money cost at each rebalance bar; zero elsewhere.
    cost_money = cost_per_unit_turnover * np.abs(delta_weight) * nav_pre_cost[:, None]

    # Returns: first value NaN by contract. Written out rather than delegated to
    # a percentage-change helper, so no fill default can apply.
    returns_values = np.empty(n_bars, dtype=float)
    returns_values[0] = np.nan
    returns_values[1:] = equity_values[1:] / equity_values[:-1] - 1.0

    equity_curve = pd.Series(equity_values, index=index)
    returns = pd.Series(returns_values, index=index)

    # -------------------------------------------------------------------------
    # SEQUENTIAL TRADE SCAN. Deliberately not vectorised: the state machine
    # (in-flight records mutated by same-direction adjustments, a flip resolving
    # one record and incepting another at a single execution bar, force-close at
    # the end) is inherently sequential, and a vectorised version would need
    # validating in its own right. Independence for this section is by
    # provenance only -- see the block above.
    #
    # In-flight state per instrument:
    #   entry_bar         execution bar of the inception (r_inception + 1), the
    #                     bar the record's entry timestamp and price come from
    #   first_accrual_bar first mark-to-market step credited to this record,
    #                     i.e. the step r_inception -> r_inception + 1, labelled
    #                     by its ending bar. Numerically equal to entry_bar; the
    #                     two are kept apart because they mean different things
    #                     and diverge at the exit end (see silence (2)).
    #   costs             costs attributed so far: inception (or opening share
    #                     of a flip) plus every adjustment
    # -------------------------------------------------------------------------
    trades: list[_Trade] = []
    in_flight: list[dict | None] = [None] * n_instruments

    def resolve(instrument_idx: int, exit_bar: int, last_accrual_bar: int, exit_cost: float) -> _Trade:
        """Close the in-flight record and build its _Trade. Segment-summed P&L:
        the sum over the mark-to-market steps this record owns, which is the sum
        over its constant-weight segments regrouped step by step."""
        state = in_flight[instrument_idx]
        gross_pnl = float(
            contribution[state["first_accrual_bar"] : last_accrual_bar + 1, instrument_idx].sum()
        )
        attributed_costs = state["costs"] + exit_cost
        entry_bar = state["entry_bar"]
        in_flight[instrument_idx] = None
        return _Trade(
            entry_timestamp=index[entry_bar],
            exit_timestamp=index[exit_bar],
            instrument=instruments[instrument_idx],
            entry_price=float(prices[entry_bar, instrument_idx]),
            exit_price=float(prices[exit_bar, instrument_idx]),
            pnl=gross_pnl - attributed_costs,
            holding_period_bars=int(exit_bar - entry_bar),
        )

    for rebalance_bar in rebalance_bars.tolist():
        execution_bar = rebalance_bar + 1  # close-to-open: fills at bar r+1

        for j in range(n_instruments):  # price-frame column order
            weight_old = float(held_into_bar[rebalance_bar, j])
            weight_new = float(held[rebalance_bar, j])

            if weight_old == 0.0 and weight_new == 0.0:
                continue  # flat to flat: no turnover, no cost, no event

            instrument_cost = float(cost_money[rebalance_bar, j])

            if weight_old == 0.0:
                # Inception from flat.
                in_flight[j] = {
                    "entry_bar": execution_bar,
                    "first_accrual_bar": rebalance_bar + 1,
                    "costs": instrument_cost,
                }

            elif weight_new == 0.0:
                # Plain resolution to flat. The record owns steps up to and
                # including the one ending at the rebalance bar, because the
                # weight it held governed the step (r-1 -> r) and the new (zero)
                # weight governs (r -> r+1).
                trades.append(
                    resolve(
                        instrument_idx=j,
                        exit_bar=execution_bar,
                        last_accrual_bar=rebalance_bar,
                        exit_cost=instrument_cost,
                    )
                )

            elif (weight_old > 0.0) == (weight_new > 0.0):
                # Same-direction size adjustment: updates the in-flight record,
                # does not resolve it and does not create a new one. Entry
                # timestamp, entry price and the accrual start are untouched;
                # only the cost attaches, and the changed weight is already in
                # the panel that drives the accrual from here on.
                in_flight[j]["costs"] += instrument_cost

            else:
                # Sign flip: one close and one open at the same execution bar,
                # sharing a single per-instrument cost split by each side's
                # contribution to the turnover. abs(dw) == abs(old) + abs(new)
                # here, so the two shares sum to the cost exactly.
                turnover_basis = abs(weight_old) + abs(weight_new)
                closing_share = instrument_cost * abs(weight_old) / turnover_basis
                opening_share = instrument_cost * abs(weight_new) / turnover_basis

                trades.append(
                    resolve(
                        instrument_idx=j,
                        exit_bar=execution_bar,
                        last_accrual_bar=rebalance_bar,
                        exit_cost=closing_share,
                    )
                )
                in_flight[j] = {
                    "entry_bar": execution_bar,
                    "first_accrual_bar": rebalance_bar + 1,
                    "costs": opening_share,
                }

    # Force-close: positions still open when the data ends. Exit at the last
    # bar's timestamp and its single price (its close, not a synthesised next
    # open), and no cost, since the end of the frame is not a rebalance. These
    # resolve last, in column order.
    last_bar = n_bars - 1
    for j in range(n_instruments):
        if in_flight[j] is None:
            continue
        trades.append(
            resolve(
                instrument_idx=j,
                exit_bar=last_bar,
                last_accrual_bar=last_bar,
                exit_cost=0.0,
            )
        )

    return equity_curve, returns, trades


def _date_str(ts: pd.Timestamp) -> str:
    return ts.strftime("%Y-%m-%d")


def _ts_str(ts: pd.Timestamp) -> str:
    # RFC 3339 / ISO 8601 with explicit Z, matching Ptime.to_rfc3339 output
    # consumed by test/unit/cairos_engine/test_known_outcomes.ml's
    # ptime_of_date helper.
    return ts.strftime("%Y-%m-%dT%H:%M:%SZ")


def write_backtest_frame_csv(
    name: str, df: pd.DataFrame, dates: list[str]
) -> None:
    write_frame_csv(name, list(df.columns), dates, df.to_numpy().tolist())


def write_backtest_series_csv(name: str, series: pd.Series) -> None:
    path = FIXTURES / f"{name}.csv"
    with open(path, "w") as f:
        f.write("timestamp,value\n")
        for ts, v in series.items():
            f.write(f"{_date_str(ts)},{_format_cell(v)}\n")


def write_backtest_trades_csv(name: str, trades: list[_Trade]) -> None:
    path = FIXTURES / f"{name}.csv"
    with open(path, "w") as f:
        f.write(
            "entry_timestamp,exit_timestamp,instrument,"
            "entry_price,exit_price,pnl,holding_period_bars\n"
        )
        for tr in trades:
            f.write(
                ",".join(
                    [
                        _ts_str(tr.entry_timestamp),
                        _ts_str(tr.exit_timestamp),
                        tr.instrument,
                        f"{tr.entry_price:.17g}",
                        f"{tr.exit_price:.17g}",
                        f"{tr.pnl:.17g}",
                        str(tr.holding_period_bars),
                    ]
                )
                + "\n"
            )


def _check_backtest_invariants(
    equity_curve: pd.Series, trades: list[_Trade]
) -> None:
    # Layer 3 invariants applied to the Pandas
    # output. If any of these fail the Python implementation is wrong and
    # must be fixed before the fixtures are trusted.
    assert equity_curve.iloc[0] == 1.0, (
        f"equity_curve[0] must be 1.0, got {equity_curve.iloc[0]!r}"
    )
    assert (equity_curve > 0.0).all(), "equity_curve must be strictly positive"
    pnl_sum = sum(tr.pnl for tr in trades)
    last = float(equity_curve.iloc[-1])
    diff = abs((1.0 + pnl_sum) - last)
    tol = max(len(trades), 1) * 1e-12
    assert diff < tol, (
        f"sum(pnl) + 1.0 = {1.0 + pnl_sum!r}, last equity = {last!r}, "
        f"|diff| = {diff!r} exceeds {tol!r}"
    )


def main():
    FIXTURES.mkdir(parents=True, exist_ok=True)
    for name, prices in SERIES.items():
        write_csv(f"input_{name}", prices)
        write_scalar(f"cumulative_return_{name}", cumulative_return(prices))
        write_scalar(f"annualised_return_{name}", annualised_return(prices))
        write_scalar(f"annualised_vol_{name}", annualised_vol(prices))
        write_scalar(f"sharpe_rf0_{name}", sharpe(prices, risk_free=0.0))
        write_scalar(f"sharpe_rf4_{name}", sharpe(prices, risk_free=0.04))
        write_scalar(f"max_drawdown_{name}", max_drawdown(prices))
        write_series_csv(f"drawdown_series_{name}", drawdown_series(prices))
        write_series_csv(f"cumprod_{name}", cumprod_series(prices))
        write_series_csv(f"cumsum_{name}", cumsum_series(prices))

    for fname, spec in FRAME_XSEC_FIXTURES.items():
        columns = spec["columns"]
        rows = spec["rows"]
        write_frame_csv(f"frame_xsec_{fname}", columns, FRAME_XSEC_INDEX, rows)
        write_frame_expected(
            f"frame_xsec_{fname}_rank_expected",
            columns,
            FRAME_XSEC_INDEX,
            frame_rank(rows, columns),
        )
        write_frame_expected(
            f"frame_xsec_{fname}_zscore_expected",
            columns,
            FRAME_XSEC_INDEX,
            frame_zscore(rows, columns),
        )

    prices_df = backtest_prices_df()
    signals_df = backtest_signals_df()
    date_strs = [_date_str(ts) for ts in prices_df.index]
    write_backtest_frame_csv("backtest_prices", prices_df, date_strs)
    write_backtest_frame_csv("backtest_signals", signals_df, date_strs)
    equity_curve, returns, trades = backtest_reference(
        prices_df,
        signals_df,
        BACKTEST_REBALANCE_BAR_INDICES,
        BACKTEST_COMMISSION,
        BACKTEST_SLIPPAGE,
    )
    _check_backtest_invariants(equity_curve, trades)
    write_backtest_series_csv("backtest_equity_curve", equity_curve)
    write_backtest_series_csv("backtest_returns", returns)
    write_backtest_trades_csv("backtest_trades", trades)

    daily = resample_daily_input()
    write_resample_series_csv("resample_daily_input", daily)
    write_resample_series_csv(
        "resample_monthly_sum_expected",
        resample_monthly_reference(daily, "sum"),
    )
    write_resample_series_csv(
        "resample_monthly_last_expected",
        resample_monthly_reference(daily, "last"),
    )

    nan_daily = resample_nan_daily_input()
    write_resample_series_csv("resample_nan_daily_input", nan_daily)
    write_resample_series_csv(
        "resample_nan_monthly_count_expected",
        resample_monthly_reference(nan_daily, "count"),
    )


if __name__ == "__main__":
    main()
