# /// script
# requires-python = ">=3.12,<3.13"
# dependencies = ["vectorbt==1.1.0", "pandas>=2.0", "numpy>=1.26"]
# ///
"""vectorbt oracle: the shared scenarios run through a third-party order engine.

Third of three implementations of the same scenarios. The engine is the system
under test; `reference.py`'s clean-room `backtest_reference` proves conformance
to the specification; this script and `nautilus_oracle.py` are independently
authored backtesters that can indicate the specification itself is wrong. Their
output is committed under `oracle_fixtures/` and compared by
`test/unit/cairos_engine/cross_validate_oracles.exe`.

Pinned to an isolated per-script environment by the PEP 723 header above:
`uv run validation/vectorbt_oracle.py`. The upper Python bound is not
decoration — vectorbt pulls numba, whose wheel availability lags new CPython
releases, and an unbounded floor would let a fresh interpreter fail resolution
with an error that looks like a repository problem.

Nothing here is installed by the default gate, by `just validate`, or by CI.

Not validated against Cairos anywhere in this file: comparing an oracle to the
system it exists to check would recreate the circularity the whole exercise
removes. Its trustworthiness rests on being third-party code, on scenarios
small enough to check by hand, and on a third system disagreeing when one of
the two is wrong.

TRANSLATION
-----------
Two distinct things are done to make an order-driven engine comparable to the
engine's continuously rebalanced, constant-weight book. They are not the same
kind of move and `oracle_scenarios.py` documents both in full:

  1. MODEL TRANSLATION (a claim about equivalence, and testable): the held
     target weight is re-issued as a `targetpercent` order at EVERY bar, not
     only on rebalance bars. vectorbt holds shares constant between orders and
     would otherwise drift; re-issuing restores the target weight each bar,
     which is what the engine's mark-to-market assumes. vectorbt still computes
     its own share counts, cash balance, fees and value path — this configures
     which book it simulates, it does not hand it any answers.

  2. SCENARIO ARRANGEMENT (not a claim about equivalence): the price paths are
     shaped so two genuine convention differences — post-cost-NAV vs
     pre-cost-value targeting basis, and nominal vs drifted turnover basis —
     have nothing to act on at any measurement point. Neither is bridged here,
     and neither is confirmed by these fixtures.

Costs are passed as a per-bar `fees` array carrying `commission + slippage` on
rebalance bars and zero elsewhere, with vectorbt's own `slippage` left at zero.
Two reasons, and both are about faithfulness rather than convenience. The engine
charges cost only at rebalance bars, so the every-bar re-issue has to be free —
leaving `slippage` on would price the tiny realignment orders and invent
turnover cost the engine never charges. And the engine's cost is additive on
notional, `(commission + slippage) * |dw| * nav`, whereas vectorbt's slippage
marks the fill price up multiplicatively, which is a different quantity.
Measured: passing `slippage` natively instead lands 5.7e-5 to 2.7e-4 away in
final NAV across the three scenarios. `fees` is charged additively on order
value, which is exactly the engine's rule.

EXECUTION TIMING: no signal shift. An order at bar r here corresponds to a
rebalance at bar r in the engine — both put the new weight in force for the step
r -> r+1. The engine's "filled at bar r+1's open" convention governs trade
*record* prices, which these fixtures do not carry. See the timing section of
`oracle_scenarios.py`.
"""

import numpy as np
import pandas as pd
import vectorbt as vbt

import oracle_scenarios as scen


def run_scenario(s: scen.Scenario) -> list[float]:
    """Run one scenario through vectorbt and return its per-bar portfolio value.

    Every scenario goes through this one path: the per-scenario differences are
    entirely in the data (`oracle_scenarios.py` records how each price path
    satisfies the flat-step constraints), so there is one translation to check
    by hand rather than three.
    """
    index = pd.to_datetime(s.timestamps())
    columns = list(s.instruments)
    prices = pd.DataFrame(
        [list(row) for row in s.prices], index=index, columns=columns
    )

    # Model translation (1): the held weight as a target at every bar. Bars
    # before the first rebalance hold nothing; NaN means "no order" rather than
    # "target zero percent", so vectorbt is never asked to trade there.
    held = pd.DataFrame(s.held_weights(), index=index, columns=columns)
    first_rebalance = s.rebalance_bars[0]
    held.iloc[:first_rebalance] = np.nan

    # Cost on rebalance bars only; the free re-issue on every other bar is what
    # makes the constant-weight book reproducible.
    rate = s.commission + s.slippage
    fee_by_bar = np.zeros(s.n_bars)
    fee_by_bar[list(s.rebalance_bars)] = rate
    fees = pd.DataFrame(
        np.repeat(fee_by_bar[:, None], len(columns), axis=1),
        index=index,
        columns=columns,
    )

    # Every parameter that affects the simulation is passed explicitly, including
    # those whose vectorbt default already happens to be the value wanted. An
    # oracle that inherits a default is an oracle whose model can be changed by
    # an upgrade to the library it is pinned against, silently and in a direction
    # nobody chose. The values below are the ones actually in force under
    # vectorbt 1.1.0; making them explicit is a no-op today and a guard tomorrow.
    portfolio = vbt.Portfolio.from_orders(
        prices,
        held,
        size_type="targetpercent",
        direction="both",
        # np.inf here is vectorbt's "the current bar's close": the single price
        # per (bar, instrument) that the scenarios carry, serving as both the
        # bar's close and its fill price. val_price makes positions valued at
        # that same price when a target percent is converted to an order, which
        # is what makes a target a fraction of the current portfolio value.
        price=np.inf,
        val_price=np.inf,
        ffill_val_price=True,
        # Value is not re-marked between the per-instrument orders of one bar, so
        # both legs of a two-instrument rebalance size against the same NAV — the
        # engine differences both targets against one pre-cost NAV too.
        update_value=False,
        fees=fees,
        fixed_fees=0.0,
        slippage=0.0,
        init_cash=1.0,
        # Fractional positions: the engine's weights are continuous and no
        # scenario has a lot size.
        size_granularity=np.nan,
        min_size=1e-08,
        max_size=np.inf,
        # Short positions raise cash rather than being blocked; the engine's
        # negative weights are unrestricted.
        lock_cash=False,
        # An order that cannot be filled in full is rejected rather than partly
        # filled, and a rejection raises instead of being skipped. The model
        # translation depends on the target weight being *reached* at every bar:
        # a silently partial or dropped order would leave the book off-target and
        # produce a fixture that is wrong by roughly the size of the shortfall.
        # This turns that into a loud failure. reject_prob 0.0 removes the only
        # stochastic element in the simulation.
        allow_partial=False,
        raise_reject=True,
        reject_prob=0.0,
        # One shared cash account across instruments, so a target percent is a
        # fraction of total portfolio value rather than of a per-instrument
        # sleeve — the engine's weights are fractions of one NAV. call_seq
        # 'auto' sells before it buys within a bar so a rebalance is not
        # rejected for want of cash it is about to raise.
        group_by=True,
        cash_sharing=True,
        call_seq="auto",
        freq="1D",
    )

    value = portfolio.value()
    if isinstance(value, pd.DataFrame):
        # Grouped portfolios return one column for the group.
        if value.shape[1] != 1:
            raise AssertionError(
                f"{s.scenario_id}: expected one grouped value column, "
                f"got {value.shape[1]}"
            )
        value = value.iloc[:, 0]

    if len(value) != s.n_bars:
        raise AssertionError(
            f"{s.scenario_id}: value path has {len(value)} bars, "
            f"scenario has {s.n_bars}"
        )
    if value.iloc[0] != 1.0:
        # Bar 0 precedes the first rebalance in every scenario, so nothing has
        # been traded yet and the normalised starting NAV must survive intact.
        # A failure here means the row alignment is off by a bar.
        raise AssertionError(
            f"{s.scenario_id}: value[0] is {value.iloc[0]!r}, expected 1.0"
        )
    return [float(v) for v in value.to_numpy()]


def main() -> None:
    scen.validate()
    for s in scen.SCENARIOS:
        equity = run_scenario(s)
        scen.write_series_csv(
            f"vectorbt_{s.scenario_id}_equity", s.timestamps(), equity
        )
        print(f"{s.scenario_id}: final NAV {equity[-1]:.17g}")
        print("  " + " ".join(f"{v:.12f}" for v in equity))


if __name__ == "__main__":
    main()
