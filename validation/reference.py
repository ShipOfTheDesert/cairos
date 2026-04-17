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


if __name__ == "__main__":
    main()
