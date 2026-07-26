# CD-003 — Turnover basis: nominal vs drifted held weight

**Status:** convention difference. Not a defect on any side.

**Parked:** no. Not reachable on any shipped scenario, so it never reaches the
comparison and there is nothing to skip.

**Owner:** the project owner, at the next `cairos_engine` feature that touches
`test/unit/cairos_engine/test_invariants.ml` — the outstanding item is the
zero-`dw` no-op property described below. It is listed in the planning
document's Layer 3 invariant set so it is picked up where that work is
scheduled; nothing reads `validation/discrepancies/` at planning time.

**Scenario(s):** reachable only where a rebalance follows a price move, which no
shipped scenario does. Measured on a counterfactual variant of
`long_short_flip`, stated in full below. `long_short_flip` is the only shipped
scenario where the constraint that suppresses it is not vacuous, since it is the
only one with a second rebalance.

**Systems disagreeing:** cairos against both oracles. The oracles agree with
each other — another clean two-against-one where the odd system out is the
engine, and where no order-driven system can be configured onto its side.

---

## The three systems

- **cairos** — the backtest engine under test, run in-process by
  `test/unit/cairos_engine/cross_validate_oracles.exe`.
- **vectorbt** — third-party vectorised order engine, `vectorbt_oracle.py`.
- **nautilus** — third-party event-driven backtester with a simulated venue,
  `nautilus_oracle.py`.

## What differs

At a rebalance the engine computes turnover as

    sum_j |w_target_j - w_held_j|

where `w_held` is the **nominal** weight the previous rebalance set — the number
in the weight panel, unchanged since. An order-driven engine computes it against
the **drifted actual** weight, because that is the position it really has to
trade out of. The two differ by whatever drift accumulated since the last
rebalance, and the charged cost differs in proportion.

## Scenario inputs

**This is a counterfactual variant. It is not a shipped scenario and no fixture
in this repository contains it.** It is `long_short_flip` with the flat step
*into* the flip deleted, and with a price move there instead.

8 bars, instrument `A`, one price per bar serving as both close and open. Bar
`i` dated `2024-01-(01+i)T00:00:00Z`. Initial NAV `1.0`.

| bar | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
|---|---|---|---|---|---|---|---|---|
| A | 100 | 125 | 125 | 100 | 125 | 150 | 150 | 120 |

Rebalance: bar 1 → `w_A = 0.8`; bar 5 → `w_A = -0.5`.
`commission = 0.001`, `slippage = 0.0005`. Every-bar target re-issue applied to
both oracles, exactly as in the shipped configuration.

Compare with the shipped `long_short_flip`, whose prices are
`100, 125, 125, 100, 125, 125, 125, 100`: bar 4 and bar 5 are equal there, so
nothing drifts into the flip.

## All three outputs

Final NAV on the counterfactual variant: cairos and the two oracles differ by
**5.316e-5**, with the two oracles agreeing.

**Worked instance, reproducible by hand from the table above.** Going into bar 5
the nominal weight is `0.8`. The step 4 → 5 moves the price 125 → 150, so the
position grows 20% while the book grows 16%, and the actual weight at the flip
bar is

    0.96 / 1.16 = 0.827586...

The engine charges turnover `|-0.5 - 0.8| = 1.3`. Both oracles charge
`|-0.5 - 0.827586| = 1.327586`. That is a 2.1% difference in the cost charged at
that single bar, which at `0.0015` per unit of turnover is a 4.1e-5 relative hit
to NAV, compounding into the 5.316e-5 final-NAV figure.

**Provenance:** the 5.316e-5 figure was measured in a working session against
the clean-room specification-derived reference, not by anything committed to
this repository. The worked instance above is arithmetic from the stated inputs
and needs no tooling to check. As with CD-002, no per-bar table is given because
no committed artefact carries one for this variant.

On the shipped scenarios the corresponding disagreement is exactly zero to
machine precision — worst per-bar deviation 4.441e-16 across all three.

## Each system's translation, and the grounds for believing it

### cairos

No translation. Nominal-weight turnover is the engine's stated rule and is
applied as shipped.

### vectorbt and nautilus — no translation exists for this

As with CD-002, **nothing bridges this difference.** An order-driven engine
computes the order it has to send from the position it actually holds; there is
no configuration under which it sizes a rebalance against a position it does not
have. The two oracles agree with each other here for that structural reason.

### SCENARIO ARRANGEMENT (claims nothing, confirms nothing)

Every shipped scenario holds prices **flat over the step into each rebalance bar
after the first**, so no drift accumulates and the nominal weight *is* the
actual weight. The two rules then compute the same number.

This is an arrangement, not an equivalence claim, and it is stated repetitively
across `oracle_scenarios.py`, both oracle scripts and this file for the same
reason as CD-002: agreement to 4.441e-16 must not be read as the two systems
sharing a turnover convention.

It is enforced mechanically by `oracle_scenarios.validate()`, which fails if the
step into any rebalance bar after the first is not flat, naming the step and
saying which difference the edit reopens.

**Grounds for believing the arrangement does what it claims:** removing it is
what produces the 5.316e-5 figure, and the worked instance above shows the
mechanism in closed form rather than as a measurement to be trusted.

## What has been ruled out

- **That this is CD-002 under another name.** Ruled out: they act at different
  moments and are suppressed by different constraints. CD-002 is about how a new
  position is *sized* out of the post-cost NAV and is suppressed by the flat step
  *out of* a rebalance; this is about how the traded quantity is *measured* and
  is suppressed by the flat step *into* a rebalance. The counterfactual variants
  differ accordingly.
- **That it is the sign-flip cost split.** Ruled out: the split of the flip's
  cost between the closing and opening legs is a separate rule, acting on a
  quantity this entry does not touch. This entry is about the *total* turnover
  the flip is charged on, before any split; changing the split moves neither the
  total nor the 5.316e-5 figure above.

  Note what does **not** support that ruling out, because it would be the natural
  thing to reach for: the three systems agreeing on `long_short_flip` says
  nothing about the split. The oracle fixtures carry equity paths only, and every
  allocation that exhausts the total cost yields a bit-identical equity path — so
  the split is invisible at every measurement point in this layer. It is pinned
  elsewhere: by `sign_flip_costs_charged_on_both_legs` in
  `test/unit/cairos_engine/test_known_outcomes.ml`, which reads each leg's share
  out of the trade log at the asymmetric `+0.8 → -0.5` weights, and by the
  specification-derived reference's per-trade comparison.
- **That either oracle mis-measures its own position.** Ruled out by the two
  oracles agreeing through unrelated machinery, and by the arithmetic: 0.827586
  is exactly `0.96/1.16`, which is what a constant-share book holds after that
  price move.
- **That it is a defect in the engine.** Ruled out: nominal-weight turnover is
  internally consistent, matches the clean-room specification-derived reference
  exactly, and is a defensible convention — it makes the charged cost a function
  of the weight schedule alone rather than of the price path.

## The open question

Same coverage question as CD-002, and it has a cheaper answer here. A black-box
property covers the nominal-vs-drifted reading without any oracle involvement:

> Inserting a rebalance bar whose target vector equals the currently held target
> leaves the equity curve bit-identical, whatever the prices did since the last
> rebalance.

Under the nominal reading `dw` is exactly zero, so the extra rebalance charges
nothing and changes nothing. Under a drifted reading the actual weight has moved
away from the target, so the extra rebalance charges a cost proportional to the
drift and the two curves separate. It needs no derivation and no reference —
two runs of the engine on the same prices with one rebalance bar added — and it
also pins the "a zero-`dw` rebalance is a no-op" clause that the specification
leaves unstated and that the clean-room reference had to resolve as a silence.

It is not part of the oracle layer and was deliberately not added alongside it:
the property set belongs to the correctness-gate work that owns Layer 3, and the
feature that shipped these oracles ships no new properties. Owned per the
**Owner** field above, and listed in the planning document's Layer 3 invariant
set so it surfaces where that work is scheduled. Size: roughly a 30-line
property plus a generator.

The open question proper, for whoever picks it up: is the nominal reading a
deliberate commitment — cost as a function of the weight schedule alone — or an
unexamined consequence of comparing against the panel rather than against a
tracked position? Both readings produce identical numbers on every shipped
scenario, so nothing in the current suite distinguishes them.

## Reproducing

The shipped scenarios, where this difference is zero by construction:

```
just validate-oracle
```

The 5.316e-5 figure is **not** reproducible by anything in the tree. The variant
is stated in full above, and the worked instance is checkable with a calculator.
