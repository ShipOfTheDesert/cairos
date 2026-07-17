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
    # convention (PRD 0021 Decision 1). Do not switch to NumPy .std() without
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
    # PRD 0021 Decision 4: constant series (std == 0) yields nan, matching
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
    # Decision 3: returned as a positive magnitude.
    # Decision 4: flat series produces dd = [0,0,...], min = 0.0, abs = 0.0
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


# --- Frame cross-sectional fixtures (RFC 0050) ---------------------------------
#
# Encodes the PRD 0049 contract: rank uses method='average' tie-breaking, zscore
# uses ddof=1, NaN cells passthrough and are excluded from N. Per
# ~/.claude/solutions/general/oracle-encodes-contract-not-library-default.md
# the oracle encodes the contract regardless of whether Pandas defaults
# coincidentally agree: PRD FR-3 specifies all-NaN output rows for std=0
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
    # Distinct-and-tie mix; bounded to [-100, 100] per RFC 0050 R1.
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
    # Row 7 is a constant non-NaN row — std=0 → zscore all-NaN per FR-3.
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
    # NaN for the NaN row; zscore → all-NaN per FR-3 (≤1-non-NaN-cell row).
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
    # method='average' per PRD 0049 Decision 1 (Pandas-aligned tie-breaking).
    return df.rank(axis=1, method="average")


def frame_zscore(rows: list[list[float]], columns: list[str]) -> pd.DataFrame:
    df = pd.DataFrame(rows, columns=columns)
    # ddof=1 per PRD 0049 Decision 2 — see comment in annualised_vol.
    n_per_row = df.notna().sum(axis=1)
    mean = df.mean(axis=1, skipna=True)
    std = df.std(axis=1, ddof=1, skipna=True)
    result = df.sub(mean, axis=0).div(std, axis=0)
    # PRD FR-3 contract: rows with N<2 or std=0 produce all-NaN output. Apply
    # explicitly rather than relying on Pandas' default arithmetic happening
    # to emit NaN for these shapes.
    invalid_row = (n_per_row < 2) | std.eq(0.0)
    result.loc[invalid_row] = float("nan")
    return result


# --- Backtest engine fixtures (RFC 0056) ---------------------------------------
#
# Encodes RFC 0052's seven-step rebalance loop, RFC 0052 OC-3 / OC-6
# mark-to-market formula (extended to non-fully-invested portfolios per RFC
# 0056 §Implementation Decisions Decision 1), the proportional sign-flip cost
# split (RFC 0056 Decision 2), the turnover-notional cost formula
# [(commission +. slippage) *. abs(weight_delta) *. nav_after_mtm], the
# NAV-update ordering (deduct cost from nav_after_mtm, then apply new weights),
# and the end-of-backtest force-close at the last bar's close with no exit cost
# per PRD 0053 FR-10.
#
# COST CONVENTION (RFC 0052 Amendment A1, ANALYSIS.md 2.1): cost is a fraction
# of pre-cost NAV, never a function of the absolute price level. The pre-fix
# formula multiplied by execution_price, which is dimensionally wrong; A1
# superseded RFC 0052 OC-3's pinned formula. Both sides of the cross-validation
# now use nav_after_mtm.
#
# PROVENANCE: this backtest reference is still a transliteration of the OCaml
# engine, not an independent oracle — it shares the engine's derivation, so
# Layer 2 checks transcription only. Feature CG-5 replaces it wholesale with a
# vectorbt-based independent oracle; correctness of the cost fix rests on the
# Layer 1 hand derivations in test_known_outcomes.ml, not on this file.
#
# Per ~/.claude/solutions/general/oracle-encodes-contract-not-library-default.md
# this reference encodes RFC 0052's conventions (as amended by A1), not Pandas
# defaults. There is no Pandas backtest primitive whose default this could
# shadow; the conventions above are a from-scratch reimplementation of the
# OCaml engine in NumPy.
#
# Trade column order pinned by RFC 0054 OC-5 — do not reorder without updating
# lib/cairos_engine/cairos_engine.mli's Trade.t field declaration order.

BACKTEST_INSTRUMENTS = ["A", "B", "C", "D", "E"]
BACKTEST_N_BARS = 50
# Weekly rebalances on a 50-bar daily fixture: bars [2, 7, 12, 17, 22, 27, 32,
# 37, 42, 47]. First rebalance at bar 2 (not bar 0) so equity_curve[0] == 1.0
# holds (RFC 0052 OC-11 — first equity-curve cell is 1.0 only when bar 0 is
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
    # convention (RFC 0052 Amendment A1), cost is a fraction of pre-cost NAV
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


def backtest_reference(
    prices_df: pd.DataFrame,
    signals_df: pd.DataFrame,
    rebalance_bar_indices: list[int],
    commission: float,
    slippage: float,
) -> tuple[pd.Series, pd.Series, list[_Trade]]:
    columns = list(prices_df.columns)
    n_cols = len(columns)
    n_bars = len(prices_df.index)
    timestamps = list(prices_df.index)
    prices = prices_df.to_numpy()
    signals = signals_df.to_numpy()
    cs = commission + slippage
    rebalance_set = set(rebalance_bar_indices)

    current_weights = [0.0] * n_cols
    current_nav = 1.0
    in_flight: list[dict | None] = [None] * n_cols
    trades: list[_Trade] = []
    equity_buf = [0.0] * n_bars

    for t in range(n_bars):
        # Mark-to-market step (RFC 0056 Decision 1 — fractional-weight
        # weighted-return form, equivalent to OC-6's literal wording at
        # full investment, well-defined for shorts and partial cash).
        if t > 0:
            nav_pre = current_nav
            total_ret = 0.0
            for j in range(n_cols):
                w = current_weights[j]
                if w != 0.0:
                    r = prices[t, j] / prices[t - 1, j] - 1.0
                    total_ret += w * r
                    ift = in_flight[j]
                    if ift is not None:
                        ift["pnl_acc"] += w * nav_pre * r
            current_nav = nav_pre * (1.0 + total_ret)

        # Rebalance step (RFC 0052 OC-6 step 1–7 / NAV-update ordering OC-3).
        if t in rebalance_set:
            exec_bar = t + 1
            nav_after_mtm = current_nav
            dws = [0.0] * n_cols
            costs = [0.0] * n_cols
            total_cost = 0.0
            for j in range(n_cols):
                target = float(signals[t, j])
                w_old = current_weights[j]
                dw = target - w_old
                # Cost is turnover (|dw|) as a fraction of pre-cost NAV
                # (nav_after_mtm), not of the absolute price level. See RFC 0052
                # Amendment A1 and ANALYSIS.md 2.1 — the price-dimensioned form
                # was dimensionally wrong. exec_price is bound only in the
                # second (trade-record) loop below, where it is a trade field.
                cost = cs * abs(dw) * nav_after_mtm
                dws[j] = dw
                costs[j] = cost
                total_cost += cost
            nav_after_cost = nav_after_mtm - total_cost

            for j in range(n_cols):
                dw = dws[j]
                if dw == 0.0:
                    continue
                target = float(signals[t, j])
                w_old = current_weights[j]
                cost_j = costs[j]
                exec_price = float(prices[exec_bar, j])
                exec_ts = timestamps[exec_bar]
                if w_old == 0.0:
                    in_flight[j] = {
                        "entry_timestamp": exec_ts,
                        "entry_bar": exec_bar,
                        "entry_price": exec_price,
                        "pnl_acc": -cost_j,
                    }
                elif target == 0.0:
                    ift = in_flight[j]
                    assert ift is not None
                    trades.append(
                        _Trade(
                            entry_timestamp=ift["entry_timestamp"],
                            exit_timestamp=exec_ts,
                            instrument=columns[j],
                            entry_price=ift["entry_price"],
                            exit_price=exec_price,
                            pnl=ift["pnl_acc"] - cost_j,
                            holding_period_bars=exec_bar - ift["entry_bar"],
                        )
                    )
                    in_flight[j] = None
                elif (w_old > 0 and target > 0) or (w_old < 0 and target < 0):
                    ift = in_flight[j]
                    assert ift is not None
                    ift["pnl_acc"] -= cost_j
                else:
                    # Sign flip — proportional cost split (RFC 0056 Decision 2).
                    abs_old = abs(w_old)
                    abs_new = abs(target)
                    denom = abs_old + abs_new
                    close_share = cost_j * abs_old / denom
                    open_share = cost_j * abs_new / denom
                    ift = in_flight[j]
                    assert ift is not None
                    trades.append(
                        _Trade(
                            entry_timestamp=ift["entry_timestamp"],
                            exit_timestamp=exec_ts,
                            instrument=columns[j],
                            entry_price=ift["entry_price"],
                            exit_price=exec_price,
                            pnl=ift["pnl_acc"] - close_share,
                            holding_period_bars=exec_bar - ift["entry_bar"],
                        )
                    )
                    in_flight[j] = {
                        "entry_timestamp": exec_ts,
                        "entry_bar": exec_bar,
                        "entry_price": exec_price,
                        "pnl_acc": -open_share,
                    }

            for j in range(n_cols):
                current_weights[j] = float(signals[t, j])
            current_nav = nav_after_cost

        equity_buf[t] = current_nav

    # End-of-backtest force-close (PRD 0053 FR-10): each still-open trade
    # resolves at the last bar's close with no exit cost.
    last_bar = n_bars - 1
    last_ts = timestamps[last_bar]
    for j in range(n_cols):
        ift = in_flight[j]
        if ift is not None:
            trades.append(
                _Trade(
                    entry_timestamp=ift["entry_timestamp"],
                    exit_timestamp=last_ts,
                    instrument=columns[j],
                    entry_price=ift["entry_price"],
                    exit_price=float(prices[last_bar, j]),
                    pnl=ift["pnl_acc"],
                    holding_period_bars=last_bar - ift["entry_bar"],
                )
            )
            in_flight[j] = None

    equity_curve = pd.Series(equity_buf, index=prices_df.index, name="value")
    returns = equity_curve.pct_change(fill_method=None)
    returns.name = "value"
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
    # Layer 3 invariants from RFC 0056 §Test Plan applied to the Pandas
    # output. If any of these fail the Python implementation is wrong and
    # must be fixed before Task 6 lands.
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


if __name__ == "__main__":
    main()
