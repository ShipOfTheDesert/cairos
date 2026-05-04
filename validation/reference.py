# /// script
# dependencies = ["pandas>=2.0"]
# requires-python = ">=3.12"
# ///

import math
import pandas as pd
from pathlib import Path

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


if __name__ == "__main__":
    main()
