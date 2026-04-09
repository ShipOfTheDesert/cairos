# /// script
# dependencies = ["pandas>=2.0"]
# requires-python = ">=3.12"
# ///

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


def cumulative_return(prices: list[float]) -> float:
    returns = pd.Series(prices).pct_change()
    return float((1 + returns).prod() - 1)


def annualised_return(prices: list[float], ann_factor: float = 252.0) -> float:
    returns = pd.Series(prices).pct_change().dropna()
    n = len(returns)
    cum_ret = float((1 + returns).prod() - 1)
    return float((1 + cum_ret) ** (ann_factor / n) - 1)


def main():
    FIXTURES.mkdir(parents=True, exist_ok=True)
    for name, prices in SERIES.items():
        write_csv(f"input_{name}", prices)
        write_scalar(f"cumulative_return_{name}", cumulative_return(prices))
        write_scalar(f"annualised_return_{name}", annualised_return(prices))


if __name__ == "__main__":
    main()
