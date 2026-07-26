# CD-001 — Constant-weight vs constant-share portfolio model

**Status:** convention difference. Not a defect on any side.

**Parked:** no. The model translation described below bridges it, so all three
systems reach the comparison and agree; there is nothing for the skip list to
skip.

**Owner:** the project owner, at the next amendment to the engine's accounting
specification, for the documentation gap below. The replay-driver consequence is
owned by the project owner at Post-MVP v1 planning, which is when a
constant-share event-driven engine is first designed. Both are recorded in the
planning document's Layer 3 and Validation Strategy sections, since nothing
reads `validation/discrepancies/` at planning time.

**Scenario(s):** all three shared scenarios.

**Systems disagreeing:** cairos against both oracles, on the *untranslated*
configuration. With the translation described below applied, all three agree to
4.441e-16 per bar and this entry records why the translation is needed rather
than an outstanding disagreement.

---

## The three systems

- **cairos** — the backtest engine under test, run in-process by
  `test/unit/cairos_engine/cross_validate_oracles.exe` against the committed
  input fixtures.
- **vectorbt** — third-party vectorised order engine, driven by
  `vectorbt_oracle.py` through `Portfolio.from_orders` with
  `size_type='targetpercent'`; output committed as
  `oracle_fixtures/vectorbt_<scenario>_equity.csv`.
- **nautilus** — third-party event-driven backtester with a simulated venue,
  matching engine, account and fee model, driven by `nautilus_oracle.py`;
  output committed as `oracle_fixtures/nautilus_<scenario>_equity.csv`.

## What differs

The Cairos engine holds a **weight vector** constant between rebalances and
marks each bar as

    nav_t = nav_{t-1} * (1 + sum_j w_j * (p_{t,j}/p_{t-1,j} - 1))

That formula is only consistent with a book whose weights are `w` at the start
of every step. Since prices move within a holding period, the book must be
restored to `w` at each bar for the next step to earn `w_j * r_{t+1,j}` again.
The engine therefore models a **continuously rebalanced** portfolio, restored to
its target weights every bar at no cost.

Order-driven backtesters — vectorbt, Nautilus, and every live execution system —
hold a constant **share count** between orders. Their weights drift with prices,
and are restored only when an order is placed. Over a multi-bar hold the two
books are genuinely different portfolios, and neither is wrong.

Nothing in the engine's own specification states the continuous-rebalancing
commitment; it is implied by the mark-to-market formula. That silence is the
part of this entry worth carrying forward.

## Scenario inputs

All three scenarios: one price per (bar, instrument), serving as both that
bar's close and its open. `commission = 0.001`, `slippage = 0.0005`, so the
charged cost at a rebalance is `0.0015 * sum_j |dw_j| * nav`. Bar `i` is dated
`2024-01-(01+i)T00:00:00Z`. Initial NAV is `1.0`. A bar absent from the
rebalance schedule carries a zero signal row and is not a rebalance bar.

**`single_instrument_long`** — 6 bars, instrument `A`.

| bar | 0 | 1 | 2 | 3 | 4 | 5 |
|---|---|---|---|---|---|---|
| A | 100 | 125 | 125 | 100 | 125 | 156.25 |

Rebalance: bar 1 → `w_A = 0.8`.

**`long_short_flip`** — 8 bars, instrument `A`.

| bar | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
|---|---|---|---|---|---|---|---|---|
| A | 100 | 125 | 125 | 100 | 125 | 125 | 125 | 100 |

Rebalance: bar 1 → `w_A = 0.8`; bar 5 → `w_A = -0.5`.

**`two_instruments_one_rebalance`** — 6 bars, instruments `A`, `B`.

| bar | 0 | 1 | 2 | 3 | 4 | 5 |
|---|---|---|---|---|---|---|
| A | 100 | 125 | 125 | 100 | 125 | 156.25 |
| B | 50 | 40 | 40 | 50 | 40 | 50 |

Rebalance: bar 1 → `(w_A, w_B) = (0.6, -0.4)`.

## All three outputs

With the model translation applied — the configuration that ships. Values are
the committed fixtures and the engine's own output, at 17 significant digits.

**`single_instrument_long`**

| bar | timestamp | cairos | vectorbt | nautilus |
|---|---|---|---|---|
| 0 | 2024-01-01 | 1 | 1 | 1 |
| 1 | 2024-01-02 | 0.99880000000000002 | 0.99880000000000002 | 0.99879999999999991 |
| 2 | 2024-01-03 | 0.99880000000000002 | 0.99880000000000002 | 0.99879999999999991 |
| 3 | 2024-01-04 | 0.83899200000000007 | 0.83899199999999996 | 0.83899199999999996 |
| 4 | 2024-01-05 | 1.0067904000000001 | 1.0067904000000001 | 1.0067904000000001 |
| 5 | 2024-01-06 | 1.20814848 | 1.20814848 | 1.20814848 |

Worst deviation between any two systems: **1.110e-16**.

**`long_short_flip`**

| bar | timestamp | cairos | vectorbt | nautilus |
|---|---|---|---|---|
| 0 | 2024-01-01 | 1 | 1 | 1 |
| 1 | 2024-01-02 | 0.99880000000000002 | 0.99880000000000002 | 0.99879999999999991 |
| 2 | 2024-01-03 | 0.99880000000000002 | 0.99880000000000002 | 0.99879999999999991 |
| 3 | 2024-01-04 | 0.83899200000000007 | 0.83899199999999996 | 0.83899199999999996 |
| 4 | 2024-01-05 | 1.0067904000000001 | 1.0067904000000001 | 1.0067904000000001 |
| 5 | 2024-01-06 | 1.0048271587200002 | 1.0048271587200002 | 1.0048271587199999 |
| 6 | 2024-01-07 | 1.0048271587200002 | 1.0048271587200004 | 1.0048271587199999 |
| 7 | 2024-01-08 | 1.1053098745920003 | 1.1053098745920003 | 1.1053098745920003 |

Worst deviation between any two systems: **4.441e-16**.

**`two_instruments_one_rebalance`**

| bar | timestamp | cairos | vectorbt | nautilus |
|---|---|---|---|---|
| 0 | 2024-01-01 | 1 | 1 | 1 |
| 1 | 2024-01-02 | 0.99850000000000005 | 0.99849999999999994 | 0.99850000000000005 |
| 2 | 2024-01-03 | 0.99850000000000005 | 0.99849999999999994 | 0.99850000000000005 |
| 3 | 2024-01-04 | 0.77883000000000002 | 0.77883000000000002 | 0.77883000000000002 |
| 4 | 2024-01-05 | 0.9579609 | 0.9579609 | 0.9579609 |
| 5 | 2024-01-06 | 1.0058589449999999 | 1.0058589450000002 | 1.0058589449999999 |

Worst deviation between any two systems: **2.220e-16**.

### The untranslated configuration, which is what this entry is about

vectorbt with orders placed **only on rebalance bars** — every other setting
unchanged — against the clean-room specification-derived reference in
`reference.py`, final-NAV difference:

| scenario | orders on rebalance bars only | with the every-bar re-issue |
|---|---|---|
| `single_instrument_long` | 9.348e-3 | 0.0 |
| `long_short_flip` | 8.579e-3 | 0.0 |
| `two_instruments_one_rebalance` | 4.264e-2 | 2.2e-16 |

On a NAV of ~1.0 those are 0.9% to 4.3%. The effect grows with the number of
bars a position is held across and with how far the book is from fully invested:
a fully invested single instrument is the one case where constant weight and
constant share coincide exactly, which is why no scenario here uses a weight of
1.0.

**Provenance of the untranslated column:** measured in a working session, not by
anything committed to this repository. No script in the tree runs the oracles
without the re-issue. The scenario inputs above are complete, so the figures are
reproducible by re-running either oracle with orders restricted to its
`rebalance_bars`, but nothing here does that for you.

## Each system's translation, and the grounds for believing it

### cairos

No translation. The engine is the system under test and is run exactly as
shipped, on the committed input fixtures, with the rebalance schedule and cost
parameters restated in the comparison binary.

### vectorbt — MODEL TRANSLATION (a testable equivalence claim)

The held target weight is re-issued as a `targetpercent` order at **every** bar,
not only on rebalance bars, with the cost passed as a per-bar `fees` array
carrying `commission + slippage` on rebalance bars and zero elsewhere.
vectorbt's own `slippage` is left at zero, because the engine charges cost only
at rebalance bars and pricing the tiny realignment orders would invent turnover
cost the engine never charges — and because vectorbt's slippage marks the fill
price up multiplicatively where the engine's cost is additive on notional.

**Grounds.** The re-issue supplies exactly one bit — *which book to simulate* —
and vectorbt still computes its own share counts, cash balance, fee amounts,
position marks and value path from there. It is falsifiable in the way a
contamination is not: if the engine's mark-to-market were wrong, or its timing
off by a bar, or its sign handling inverted, the re-issued book would disagree
anyway. Two guards keep it honest: `allow_partial=False` and
`raise_reject=True`, so an order that cannot reach its target raises instead of
silently under-filling. Every behaviourally load-bearing `from_orders`
parameter is passed explicitly rather than inherited, so a library upgrade
cannot move the model underneath the fixture.

The alternative that was rejected: pre-computing a cost-adjusted target weight
and handing it to vectorbt. That supplies the answer rather than the question,
and its fixed point is not even clean — the adjusted target changes the
turnover, which changes the cost, which changes the adjustment.

### nautilus — the same MODEL TRANSLATION

Identical re-issue: submit the held target weight as an order at every bar,
charge the fee only on rebalance-tagged orders. The fee is carried by an order
tag rather than by a mutable "is this a rebalance bar" flag read at fill time,
so it travels with the order and is right whenever the fill lands.

**Grounds, and the execution-timing question.** Nautilus's fill timing was
established **empirically, before any translation comment was written**: an
order submitted inside `on_bar(i)` fills at bar `i`'s own price, so no signal
shift is applied. The probe also surfaced a second fact that decided the
fixture's shape — the fill is not synchronous inside `submit_order`, so
position and equity read immediately afterwards are still pre-trade. Equity is
therefore recorded inside `on_order_filled`, where the fill has settled and the
mark is still bar `i`'s.

That choice is measured, not argued: dropping the post-fill overwrite and
recording pre-trade puts every rebalance bar high by exactly that bar's cost —
bar 1 reads `1.000000` against `0.998800` (1.2e-3), the flip bar reads
`1.006790` against `1.004827` (2.0e-3). It is also the one mutation that
**no guard inside the oracle detects**; only the three-way comparison catches
it, which is why that comparison's per-bar check is load-bearing rather than a
refinement of its final-NAV check.

Inserting a `LatencyModel` so fills cross a bar boundary reddens the strategy's
own per-fill price assertion, which is what keeps the probe's finding true in
future rather than only on the day it was run.

### Not a translation — the scenario arrangement, stated so it is not mistaken for one

The price paths additionally hold flat over the step out of each rebalance bar
and into each rebalance bar after the first. Those are **arrangements**, not
translations: they put two further convention differences out of reach of every
measurement point rather than bridging them. They claim nothing and confirm
nothing. See CD-002 and CD-003.

## What has been ruled out

- **That the disagreement is an execution-timing offset.** Ruled out by direct
  measurement in both oracles. The engine's held weight takes effect *at*
  rebalance bar `r` and governs the step `r -> r+1`, which is exactly a
  same-bar-close order; a one-bar signal shift — the translation an order-driven
  system usually needs — misaligns every scenario. Applying it in the Nautilus
  oracle removes all cost from the curve and moves the final NAV to `1.2096`
  against `1.20814848`.
- **That it is a cost-model difference.** Ruled out by magnitude and by
  direction. The costs charged agree to machine precision once the model
  translation is applied, and the untranslated gap grows with holding length
  rather than with the number of rebalances.
- **That one of the oracles is simply wrong.** Ruled out by the two of them
  agreeing with each other through completely different machinery — a
  vectorised target-percent order matrix versus an order-by-order simulated
  venue with its own fee model and margin account.
- **That the engine has a defect here.** Ruled out: the engine's formula is
  internally consistent and matches the clean-room specification-derived
  reference exactly. The formula simply describes a different book.

## The open question

Nothing about the arithmetic is open. What remains is a documentation gap: the
engine's continuous-rebalancing commitment is a substantive, load-bearing
modelling property — it is what makes the equity path independent of
intra-holding drift — and it is implied by the mark-to-market formula rather
than stated anywhere. A user arriving from an order-driven backtester will read
a weight schedule as "buy this and hold it" and be wrong by percent-scale
amounts over a multi-bar hold. Should the engine's specification state the
property outright, and should the public documentation carry the measured
divergence against a constant-share book so the difference is discoverable
before it surprises someone?

A second consequence follows from the same fact and belongs to whoever plans the
event-driven replay driver: a realistic event-driven engine is necessarily
constant-share, so a differential test expecting identical equity paths from the
two engines cannot pass — they diverge by construction, at the percent scale
measured above, and not by defect.

## Reproducing

The shipped, translated numbers:

```
just validate-oracle
```

or, without either oracle dependency installed, against the committed fixtures:

```
opam exec -- dune exec test/unit/cairos_engine/cross_validate_oracles.exe
```

The untranslated figures are not reproducible by anything in the tree; see the
provenance note above the table.
