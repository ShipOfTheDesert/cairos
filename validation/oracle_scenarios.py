# /// script
# requires-python = ">=3.12"
# dependencies = []
# ///
"""Shared scenario definitions for the third-party backtest oracles.

Single source of truth for the inputs every oracle is authored against. Both
`vectorbt_oracle.py` and `nautilus_oracle.py` import this module rather than
restating the numbers, because a three-way comparison across systems is
meaningless the moment two of them run different inputs. Deliberately
stdlib-only: each oracle resolves its own heavyweight dependency set in its own
isolated environment, and this module has to be importable from all of them.

Run it directly (`uv run validation/oracle_scenarios.py`) to (re)write the
shared input fixtures and re-check the price-path constraints below.


THE MODEL DIFFERENCE THE ORACLES HAVE TO BRIDGE
-----------------------------------------------
The Cairos engine holds *weights* constant against a running NAV: after a
rebalance the weight vector stays put and every bar is marked as

    nav_t = nav_{t-1} * (1 + sum_j w_j * (p_{t,j}/p_{t-1,j} - 1))

which is a continuously rebalanced book — the position is implicitly restored
to its target weight at every bar, free of charge. Order-driven backtesters
(vectorbt, Nautilus, and every live system) hold *shares* constant between
orders, so their weights drift with prices. On the scenarios below the two
models part company by 9.3e-3 to 4.3e-2 in final NAV: not a defect on either
side, a difference of convention. It is CD-001 in `KNOWN_DISCREPANCIES.md`;
the per-scenario figures and the full argument are in the investigation
document that index links to.

Each oracle bridges it by re-issuing the held target weight as an order at
*every* bar, with the cost charged only on rebalance bars. That is a MODEL
TRANSLATION: it configures the oracle to simulate the book Cairos simulates,
and the oracle still computes its own share counts, cash, fees and value path
from its own order engine. It is a claim about equivalence and it is testable —
if the translation were wrong the fixtures would disagree.


THE TWO CONVENTION DIFFERENCES THE PRICE PATHS SUPPRESS
--------------------------------------------------------
Two further differences are NOT bridged by any translation, because no
configuration of an order-driven engine reproduces them. Instead the price
paths below are shaped so that neither has anything to act on. This is NOT a
claim about equivalence — it is an arrangement that puts the difference out of
reach of every measurement point in every scenario:

  (a) Targeting basis. Cairos funds new positions out of post-cost NAV: a
      target weight w is w of NAV *after* the rebalance cost is deducted. An
      order-driven engine sizes against the pre-cost portfolio value, so its
      position is w of a slightly larger number. The gap is the cost fraction,
      and it turns into a NAV difference only when the position then earns a
      return. Measured at 4.262e-4 on a counterfactual variant; recorded as
      CD-002 in `KNOWN_DISCREPANCIES.md`, whose investigation document
      states the variant in full.
      SUPPRESSED BY: the step from every rebalance bar r to bar r+1 is flat, so
      the mis-sized position earns exactly zero over the one step where it is
      mis-sized. The every-bar re-issue realigns it at bar r+1.

  (b) Turnover basis. Cairos measures rebalance turnover as |w_target -
      w_held| where w_held is the *nominal* weight the last rebalance set. An
      order-driven engine measures it against the *drifted* actual weight,
      which is what it really has to trade out of. The two differ by whatever
      drift accumulated since the last rebalance, and the cost differs in
      proportion. Measured at 5.316e-5 on a counterfactual variant; recorded as
      CD-003 in `KNOWN_DISCREPANCIES.md`, whose investigation document
      states the variant in full.
      SUPPRESSED BY: the step into every rebalance bar r after the first (bar
      r-1 to bar r) is flat, so no drift accumulates and the nominal weight is
      the actual weight.

Both constraints are enforced by `validate()` below, so a later edit to a price
path that breaks one fails loudly here rather than silently reopening a ~1e-4
disagreement in the fixtures.


WHAT THE ORACLE LAYER THEREFORE CONFIRMS, AND WHAT IT DOES NOT
---------------------------------------------------------------
Confirms, at machine precision, against independently authored order engines:
execution timing, sign handling, short mark-to-market, the total turnover a sign
flip is charged on, multi-instrument aggregation at unequal weights, and the
cost notional's dimension and magnitude — a cost proportional to a price level
rather than to NAV is off by orders of magnitude at these price levels and
cannot hide.

Does not confirm, and the distinction matters:

  * The SPLIT of a sign flip's cost between the closing and opening legs. These
    fixtures carry equity paths and nothing else, and an equity path cannot see
    an allocation: any split that exhausts the total cost produces a
    bit-identical curve. What the flip scenario confirms is the total, |w_new -
    w_old| * nav = 1.3 * nav. The split is pinned by
    `sign_flip_costs_charged_on_both_legs` in
    `test/unit/cairos_engine/test_known_outcomes.ml`, which reads each leg's
    share out of the trade log, and by the specification-derived reference.

  * The cost basis under weight drift, cases (a) and (b) above. No measurement
    point in any scenario has drift, by construction.

Both are named limitations of the oracle layer, not omissions — covering either
needs a check that does not run through an order-driven system's equity path.


HOW THE TRANSLATION WAS PRODUCED
---------------------------------
The independence at stake in the oracle layer is not the libraries' — vectorbt
and Nautilus are obviously third-party. It is the TRANSLATION's: the every-bar
re-issue, the rebalance-only fee, the post-fill equity read, and the flat-step
arrangements were all authored here, by someone who could see the engine's
outputs. A translation tuned until agreement appeared would make the whole layer
agree for the wrong reason, so the production conditions are recorded rather
than left to inference. (The clean-room reference's own record is in
`backtest_reference_provenance.md`; this is the corresponding record for the two
oracles, and the claim it makes is weaker.)

Derived from the documented model difference, before any comparison was run:

  * The every-bar target re-issue. Follows from constant-weight vs
    constant-share alone, which is stated in the specification's mark-to-market
    rule; no fixture was consulted to arrive at it.
  * Charging cost only on rebalance bars. The engine charges at rebalance bars
    only; pricing the realignment orders would invent turnover the engine never
    charges.
  * Cost as a rate on filled notional rather than a fill-price markup. The
    engine's cost is additive on notional; a price markup is a different
    quantity.

Established empirically, and recorded as such:

  * Nautilus's same-bar fill timing. Read off a probe's own output before any
    alignment was written — see `nautilus_oracle.py`, which quotes the probe
    transcript verbatim and re-asserts the finding on every fill.
  * The post-fill equity read. A pre-trade read sits above the engine by exactly
    one bar's cost. This was found by observing a deviation, and the deviation
    is what identified where equity had to be sampled. It is a translation
    decision reached from a disagreement, which is the category that most needs
    saying out loud.

Arrived at from a deviation and then NOT closed by a translation — routed into
the discrepancy machinery instead, because closing them would have been tuning:

  * (a) targeting basis and (b) turnover basis above. Both are real convention
    differences with no order-engine configuration that reproduces the engine's
    side. They are recorded as CD-002 and CD-003, measured on counterfactual
    variants, and suppressed by scenario arrangement — which is labelled an
    arrangement everywhere it appears and claims nothing.

Nothing else was adjusted after seeing a comparison result. The one lever that
would constitute tuning — zeroing or shrinking the costs so the cost-basis
differences stop mattering — was considered and rejected: it would remove the
oracle layer's coverage of the cost notional, which is the defect class that
motivated this whole exercise. Costs run live and non-zero in every scenario.


EXECUTION TIMING — WHY NO SIGNAL SHIFT IS APPLIED
--------------------------------------------------
The engine's stated convention is that a signal read at rebalance bar r is
executed at the open of bar r+1, and a trade record's entry price and timestamp
come from bar r+1. But the *weight panel* has the new target in force at bar r,
governing the step r -> r+1, so in the equity path the position earns
p_{r+1}/p_r - 1 — it is economically filled at bar r's price. With one price per
(bar, instrument) serving as both that bar's close and its open, those two rules
are irreconcilable, and the engine's own reference documents the one-bar offset
as a known consequence: a trade's pnl is not recoverable from its recorded entry
and exit prices.

The oracles compare equity paths, not trade records. So the correct alignment is
an order at bar r in the oracle against a rebalance at bar r in Cairos, with NO
shift. Shifting the signal by one bar — the translation an order-driven system
usually needs — would be wrong here and would misalign every scenario by a bar.
"""

from dataclasses import dataclass
from datetime import date, timedelta
from decimal import Decimal
from pathlib import Path

FIXTURES = Path(__file__).parent / "oracle_fixtures"

# Every price must be exact at this many decimals. The Nautilus oracle declares
# its instruments at this price precision and rounds each bar's price to it, so
# a price needing more decimals would be silently rounded there and produce a
# fixture that is wrong and still green. The constraint belongs to the scenario
# data rather than to that one script — it is checked here, and that script
# reads this constant instead of restating it.
PRICE_DECIMALS = 2

# Matches the cost parameters the 50-bar engine cross-validation fixture uses,
# so a defect that only shows up at a particular cost magnitude is not dodged
# by the oracles running a different one.
COMMISSION = 0.001
SLIPPAGE = 0.0005

START_DATE = date(2024, 1, 1)


@dataclass(frozen=True)
class Scenario:
    """One scenario, authored once and run by every system.

    prices[bar][instrument] is the single price for that (bar, instrument),
    serving as both the bar's close and its open. rebalance maps a bar index to
    the target weight vector read at that bar; bars absent from it are not
    rebalance bars and carry a zero signal row.
    """

    scenario_id: str
    instruments: tuple[str, ...]
    prices: tuple[tuple[float, ...], ...]
    rebalance: tuple[tuple[int, tuple[float, ...]], ...]
    commission: float
    slippage: float

    @property
    def n_bars(self) -> int:
        return len(self.prices)

    @property
    def rebalance_bars(self) -> tuple[int, ...]:
        return tuple(bar for bar, _ in self.rebalance)

    def timestamps(self) -> list[str]:
        # Full RFC 3339 with an explicit Z. The date-only form parses too, but
        # the full form lets the comparison verify row alignment between the
        # two oracle families rather than assume it.
        return [
            (START_DATE + timedelta(days=i)).isoformat() + "T00:00:00Z"
            for i in range(self.n_bars)
        ]

    def signal_rows(self) -> list[list[float]]:
        """The signal frame the engine consumes: target weights on rebalance
        bars, zeros everywhere else."""
        targets = dict(self.rebalance)
        return [
            list(targets.get(bar, (0.0,) * len(self.instruments)))
            for bar in range(self.n_bars)
        ]

    def held_weights(self) -> list[list[float]]:
        """The weight in force at each bar: the target of the most recent
        rebalance bar <= t, zeros before the first rebalance. This is the panel
        each oracle re-issues as an order at every bar."""
        targets = dict(self.rebalance)
        rows: list[list[float]] = []
        current = [0.0] * len(self.instruments)
        for bar in range(self.n_bars):
            if bar in targets:
                current = list(targets[bar])
            rows.append(list(current))
        return rows


# Scenario 1 — single instrument, long, one rebalance.
# Weight 0.8 rather than 1.0 on purpose: a fully invested target cannot be
# reached by an order-driven engine that must also fund the fee out of the same
# cash, so it scales the order down and lands 2.2e-6 away. At 0.8 there is
# spare cash and the target is reachable exactly.
# MODEL TRANSLATION (equivalence claim): every-bar re-issue of the 0.8 target,
# so the oracle holds the weight rather than the share count across 2->3, 3->4,
# 4->5. ARRANGEMENT (no equivalence claim): constraint (a), bar 1 -> 2 flat
# (125 -> 125), which puts the targeting-basis difference out of reach. No
# second rebalance, so constraint (b) is vacuous here.
SINGLE_INSTRUMENT_LONG = Scenario(
    scenario_id="single_instrument_long",
    instruments=("A",),
    prices=((100.0,), (125.0,), (125.0,), (100.0,), (125.0,), (156.25,)),
    rebalance=((1, (0.8,)),),
    commission=COMMISSION,
    slippage=SLIPPAGE,
)

# Scenario 2 — long/short flip.
# The flip at bar 5 is asymmetric (+0.8 -> -0.5) so the cost splits 8/13 and
# 5/13 between the closing and opening legs: a rule that halves the cost, or
# charges it all to one leg, is a different number here. A symmetric flip could
# not tell those apart.
# MODEL TRANSLATION (equivalence claim): every-bar re-issue across both legs —
# live steps 2->3 and 3->4 long, 6->7 short. ARRANGEMENT (no equivalence claim):
# constraint (a), bar 1 -> 2 and bar 5 -> 6 flat; and constraint (b), bar 4 -> 5
# flat so the +0.8 leg carries no drift into the flip and the turnover the flip
# charges is the nominal 1.3 in both systems. This is the only shared scenario
# where constraint (b) is not vacuous.
LONG_SHORT_FLIP = Scenario(
    scenario_id="long_short_flip",
    instruments=("A",),
    prices=(
        (100.0,),
        (125.0,),
        (125.0,),
        (100.0,),
        (125.0,),
        (125.0,),
        (125.0,),
        (100.0,),
    ),
    rebalance=((1, (0.8,)), (5, (-0.5,))),
    commission=COMMISSION,
    slippage=SLIPPAGE,
)

# Scenario 3 — two instruments, one rebalance, unequal weights, opposite signs.
# 0.6 / -0.4 rather than an equal split so a per-instrument aggregation error
# or an equal-weight assumption moves the numbers. The two price paths diverge
# from bar 2 onward (A down then up, B up then down) so the aggregation is
# exercised rather than degenerating into a single scaled path.
# MODEL TRANSLATION (equivalence claim): every-bar re-issue of both targets.
# This scenario is where it matters most — two instruments moving in opposite
# directions drift apart fastest, and it is the largest measured
# constant-weight/constant-share divergence of the three at 4.264e-2.
# ARRANGEMENT (no equivalence claim): constraint (a), bar 1 -> 2 flat in BOTH
# instruments. No second rebalance, so constraint (b) is vacuous.
TWO_INSTRUMENTS_ONE_REBALANCE = Scenario(
    scenario_id="two_instruments_one_rebalance",
    instruments=("A", "B"),
    prices=(
        (100.0, 50.0),
        (125.0, 40.0),
        (125.0, 40.0),
        (100.0, 50.0),
        (125.0, 40.0),
        (156.25, 50.0),
    ),
    rebalance=((1, (0.6, -0.4)),),
    commission=COMMISSION,
    slippage=SLIPPAGE,
)

SCENARIOS: tuple[Scenario, ...] = (
    SINGLE_INSTRUMENT_LONG,
    LONG_SHORT_FLIP,
    TWO_INSTRUMENTS_ONE_REBALANCE,
)


def _fail(scenario_id: str, detail: str) -> None:
    raise AssertionError(f"oracle_scenarios[{scenario_id}]: {detail}")


def validate() -> None:
    """Enforce the structural preconditions every scenario has to satisfy.

    A violation here is not a stylistic complaint: breaking a flat-step
    constraint silently reopens a 1e-4-scale disagreement between the engine
    and both oracles that looks exactly like a modelling defect and is not one.
    """
    for s in SCENARIOS:
        n_inst = len(s.instruments)
        bars = s.rebalance_bars

        if not bars:
            _fail(s.scenario_id, "no rebalance bars")
        if list(bars) != sorted(set(bars)):
            _fail(s.scenario_id, f"rebalance bars not strictly increasing: {bars}")
        if bars[0] == 0:
            # The engine pins equity[0] == 1.0; a rebalance at bar 0 would
            # charge a cost there and contradict it.
            _fail(s.scenario_id, "first rebalance is at bar 0")
        if bars[-1] >= s.n_bars - 1:
            # The execution bar r+1 has to exist.
            _fail(s.scenario_id, f"rebalance at last bar {bars[-1]}")
        for row in s.prices:
            if len(row) != n_inst:
                _fail(s.scenario_id, f"price row width {len(row)} != {n_inst}")
            if any(p <= 0.0 for p in row):
                _fail(s.scenario_id, f"non-positive price in row {row}")
            for p in row:
                # repr() gives the shortest decimal that round-trips, so this
                # asks how many decimals the price actually needs rather than
                # how many its binary representation happens to print.
                if Decimal(repr(float(p))).as_tuple().exponent < -PRICE_DECIMALS:
                    _fail(
                        s.scenario_id,
                        f"price {p} needs more than {PRICE_DECIMALS} decimals; "
                        "the Nautilus oracle would round it silently and emit a "
                        "wrong fixture that still compares green",
                    )
        for bar, weights in s.rebalance:
            if len(weights) != n_inst:
                _fail(s.scenario_id, f"weight vector width at bar {bar}")

        for r in bars:
            # (a) targeting basis: the mis-sized position must earn nothing
            # over the single step where it is mis-sized.
            if s.prices[r] != s.prices[r + 1]:
                _fail(
                    s.scenario_id,
                    f"step {r} -> {r + 1} after rebalance bar {r} is not flat "
                    f"({s.prices[r]} -> {s.prices[r + 1]}); this reopens the "
                    "post-cost-NAV vs pre-cost-value targeting difference",
                )
            # (b) turnover basis: no drift may accumulate into a rebalance.
            if r != bars[0] and s.prices[r - 1] != s.prices[r]:
                _fail(
                    s.scenario_id,
                    f"step {r - 1} -> {r} into rebalance bar {r} is not flat "
                    f"({s.prices[r - 1]} -> {s.prices[r]}); this reopens the "
                    "nominal-weight vs drifted-weight turnover difference",
                )


def format_cell(v: float) -> str:
    # NaN as an empty cell, matching the shipped fixture convention that
    # Cairos_io's CSV reader parses back to Float.nan.
    if v != v:
        return ""
    return f"{float(v):.17g}"


def write_frame_csv(name: str, columns: tuple[str, ...], timestamps: list[str],
                    rows: list[list[float]]) -> None:
    FIXTURES.mkdir(parents=True, exist_ok=True)
    with open(FIXTURES / f"{name}.csv", "w") as f:
        f.write("timestamp," + ",".join(columns) + "\n")
        for ts, row in zip(timestamps, rows, strict=True):
            f.write(",".join([ts] + [format_cell(v) for v in row]) + "\n")


def write_series_csv(name: str, timestamps: list[str], values: list[float]) -> None:
    FIXTURES.mkdir(parents=True, exist_ok=True)
    with open(FIXTURES / f"{name}.csv", "w") as f:
        f.write("timestamp,value\n")
        for ts, v in zip(timestamps, values, strict=True):
            f.write(f"{ts},{format_cell(v)}\n")


def write_manifest() -> None:
    """Write the scenario roster the comparison binary iterates.

    So that adding a scenario here cannot leave that binary silently comparing
    the old set: it reads this file rather than holding its own list.
    """
    FIXTURES.mkdir(parents=True, exist_ok=True)
    with open(FIXTURES / "oracle_manifest.csv", "w") as f:
        f.write("scenario_id\n")
        for s in SCENARIOS:
            f.write(f"{s.scenario_id}\n")


def write_params(s: Scenario) -> None:
    """Write the inputs that are neither prices nor signals.

    The rebalance schedule and the two cost parameters used to be restated in
    OCaml inside the comparison binary. That is not survivable for a binary
    whose loudest output names a culprit: a seeded change to its private copy of
    this scenario's schedule made it report the engine as the odd system out,
    with the fault entirely in the binary. Emitting them here means all three
    systems provably run one schedule and one pair of cost parameters.
    """
    FIXTURES.mkdir(parents=True, exist_ok=True)
    with open(FIXTURES / f"oracle_{s.scenario_id}_params.csv", "w") as f:
        f.write("key,value\n")
        f.write(f"commission,{format_cell(s.commission)}\n")
        f.write(f"slippage,{format_cell(s.slippage)}\n")
        f.write("rebalance_bars," + ";".join(str(b) for b in s.rebalance_bars) + "\n")


def write_input_fixtures() -> None:
    """Write every input each system runs. Emitted once, from the definitions,
    so the two oracle families and the engine cannot drift apart on their
    inputs."""
    write_manifest()
    for s in SCENARIOS:
        ts = s.timestamps()
        write_frame_csv(
            f"oracle_{s.scenario_id}_prices", s.instruments, ts,
            [list(row) for row in s.prices],
        )
        write_frame_csv(
            f"oracle_{s.scenario_id}_signals", s.instruments, ts, s.signal_rows()
        )
        write_params(s)


def main() -> None:
    validate()
    write_input_fixtures()
    for s in SCENARIOS:
        print(
            f"{s.scenario_id}: {s.n_bars} bars, {len(s.instruments)} instrument(s), "
            f"rebalance bars {list(s.rebalance_bars)}, "
            f"commission={s.commission}, slippage={s.slippage}"
        )


if __name__ == "__main__":
    main()
