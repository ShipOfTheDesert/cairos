# CD-002 — Targeting basis: post-cost NAV vs pre-cost value

**Status:** convention difference. Not a defect on any side.

**Parked:** no. Not reachable on any shipped scenario, so it never reaches the
comparison and there is nothing to skip.

**Owner:** the project owner, at the next amendment to the engine's accounting
specification — the outstanding item is a statement in that document, not a code
change. Recorded in the planning document's Validation Strategy section, since
nothing reads `validation/discrepancies/` at planning time.

**Scenario(s):** reachable on none of the three shipped scenarios, by
construction. Measured on a counterfactual variant of
`two_instruments_one_rebalance`, stated in full below.

**Systems disagreeing:** cairos against both oracles. The oracles agree with
each other, so this is a clean two-against-one — and the odd system out is the
engine, whose sizing rule no order-driven backtester reproduces.

---

## The three systems

- **cairos** — the backtest engine under test, run in-process by
  `test/unit/cairos_engine/cross_validate_oracles.exe`.
- **vectorbt** — third-party vectorised order engine, `vectorbt_oracle.py`,
  `Portfolio.from_orders` with `size_type='targetpercent'`.
- **nautilus** — third-party event-driven backtester with a simulated venue,
  `nautilus_oracle.py`.

## What differs

At a rebalance the engine deducts the cost from NAV **first**, and the target
weight `w` is then `w` of what remains:

    nav_after_cost = nav_before * (1 - (commission + slippage) * sum_j |dw_j|)
    position_j     = w_j * nav_after_cost

An order-driven engine sizes the order against the portfolio value it can see at
order time — the **pre-cost** value — and pays the fee out of cash afterwards.
Its position is `w_j` of a slightly larger number. The relative difference is the
cost fraction: here `0.0015 * sum_j |dw_j|`, so 1.5e-3 of turnover.

The difference in position size is real at the instant of the rebalance. It only
becomes a difference in *NAV* once the mis-sized position earns a return.

## Scenario inputs

**This is a counterfactual variant. It is not a shipped scenario and no fixture
in this repository contains it.** It is `two_instruments_one_rebalance` with the
post-rebalance flat bar deleted, which is precisely the arrangement that
suppresses the difference in the shipped set.

5 bars, instruments `A` and `B`, one price per (bar, instrument) serving as both
close and open. Bar `i` dated `2024-01-(01+i)T00:00:00Z`. Initial NAV `1.0`.

| bar | 0 | 1 | 2 | 3 | 4 |
|---|---|---|---|---|---|
| A | 100 | 125 | 100 | 125 | 156.25 |
| B | 50 | 40 | 50 | 40 | 50 |

Rebalance: bar 1 → `(w_A, w_B) = (0.6, -0.4)`.
`commission = 0.001`, `slippage = 0.0005`. Every-bar target re-issue applied to
both oracles, exactly as in the shipped configuration.

Compare with the shipped `two_instruments_one_rebalance`, which inserts a
duplicate of bar 1's prices at bar 2 — `A: 100, 125, 125, 100, 125, 156.25`,
`B: 50, 40, 40, 50, 40, 50` — so that the step out of the rebalance earns
nothing.

## All three outputs

Final NAV on the counterfactual variant: cairos and the two oracles differ by
**4.262e-4**, with the two oracles agreeing.

**Provenance:** measured in a working session against the clean-room
specification-derived reference, not by anything committed to this repository.
The full inputs are stated above so the figure is reproducible by hand or by
re-running either oracle on the variant, but nothing in the tree does that for
you. This is the reason the per-bar table is absent: quoting per-bar values that
no committed artefact carries would be worse than stating the one figure that
was actually measured.

On the shipped scenarios the corresponding disagreement is exactly zero to
machine precision — worst per-bar deviation 4.441e-16 across all three — because
the arrangement below removes anything for the difference to act on.

## Each system's translation, and the grounds for believing it

### cairos

No translation. The post-cost sizing rule is the engine's stated accounting
model and is applied as shipped.

### vectorbt and nautilus — no translation exists for this

This is the section's real content: **nothing bridges this difference.** No
configuration of an order-driven engine reproduces "size the target against the
value that will remain after the fee is paid", because at order time the fee has
not been paid and the engine is sizing against the value it holds. The two
oracles agree with each other here for the same structural reason they disagree
with cairos.

### SCENARIO ARRANGEMENT (claims nothing, confirms nothing)

Instead of a translation, every shipped scenario holds prices **flat over the
step from each rebalance bar `r` to bar `r+1`**. The mis-sized position
therefore earns exactly zero over the single step on which it is mis-sized, and
the every-bar re-issue realigns it at bar `r+1`.

This is an arrangement, not an equivalence claim. It is stated here, in
`oracle_scenarios.py`, and in both oracle scripts, deliberately repetitively,
because a reader who saw "translation applied, agreement to 4.441e-16" would
reasonably conclude that the systems model cost basis identically. They do not.

The arrangement is enforced mechanically: `oracle_scenarios.validate()` fails if
any rebalance bar's prices differ from the next bar's, with the message naming
the step and saying which difference the edit reopens. A later change to a price
path that breaks it fails loudly there rather than silently reopening a ~1e-4
disagreement in the fixtures.

**Grounds for believing the arrangement does what it claims:** it was verified
by removing it. Deleting the flat bar is what produces the 4.262e-4 figure
above; changing scenario 1's bar-2 price from 125 to 130 trips
`oracle_scenarios.validate()` with
`step 1 -> 2 after rebalance bar 1 is not flat ((125.0,) -> (130.0,)); this
reopens the post-cost-NAV vs pre-cost-value targeting difference`.

## What has been ruled out

- **That this is the same effect as CD-001.** Ruled out by magnitude and by
  independence: CD-001 measures 9.3e-3 to 4.3e-2 and is removed entirely by the
  every-bar re-issue; this residue is 4.262e-4 and survives it.
- **That it is a defect in either oracle's fee accounting.** Ruled out by the
  two oracles agreeing with each other while using unrelated fee machinery — a
  per-bar `fees` array on a vectorised order matrix, and a simulated venue's own
  fee model on a margin account.
- **That it is a defect in the engine.** Ruled out: the engine's rule is
  internally consistent, matches its own accounting model, and matches the
  clean-room specification-derived reference exactly. Post-cost sizing is a
  defensible convention — it guarantees the book is never levered by the fee —
  and pre-cost sizing is equally defensible. They are different questions.
- **That the shipped scenarios secretly contain the effect at a level below
  tolerance.** Ruled out structurally rather than by measurement: the mis-sized
  position earns a return of exactly zero over the one step where it is
  mis-sized, so the contribution is identically zero, not merely small.

## The open question

Not the arithmetic — the coverage. The oracle layer does not confirm the
engine's cost basis under weight drift, because no measurement point in any
scenario has drift. That is a named limitation, not an omission: covering it
needs a check that does not run through an order-driven system at all, since
every such system disagrees with the engine here by convention.

The narrower version of the question, which a follow-up could settle cheaply: is
the engine's post-cost sizing what the accounting model intends, or an artefact
of the order in which the cost deduction and the weight application happen to be
written? Both readings produce the same numbers on every shipped scenario, so
nothing in the current suite distinguishes them. Owned per the **Owner** field
above; until that statement is written the two readings are indistinguishable
from the repository alone.

There is also a standing methodological worry worth stating rather than
burying. The same argument shape — "the difference is expected, so arrange it
away" — is how a genuine defect gets normalised. The defence here is that the
difference is convention on both sides, that suppressing it is what lets the
remaining behaviours be checked exactly rather than inside a tolerance wide
enough to hide a real defect, and that the arrangement is labelled as an
arrangement everywhere it appears. That defence is believed, and it is exactly
the part a reviewer should not take on trust.

## Reproducing

The shipped scenarios, where this difference is zero by construction:

```
just validate-oracle
```

The 4.262e-4 figure is **not** reproducible by anything in the tree. The variant
is stated in full above; reproducing it means running either oracle and the
engine on those five bars.
