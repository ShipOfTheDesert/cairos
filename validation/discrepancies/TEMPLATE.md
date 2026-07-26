# CD-NNN — <one-line title>

<!--
Copy this file to `NNN-<slug>.md`, fill every section, and add one row to
`../KNOWN_DISCREPANCIES.md`.

This document is written to be pasted **whole** into a fresh session that holds
no prior context — not read alongside the conversation that produced it. The
session that resolves a discrepancy is rarely the one that found it, and the
reasoning that made the disagreement puzzling is the part that decays fastest.
So: restate, do not cite. A reader who has only this file must be able to
reproduce the numbers and understand why they were surprising.

Two hard constraints on what may appear here:

  * No paths outside this repository. Planning and specification directories
    are not committed, so a citation to one is unresolvable. Restate the
    relevant contract clause substantively instead.
  * No number without its provenance. Every figure states what produced it and
    how to reproduce it, or says outright that it is not reproducible from the
    tree and why.

Delete these instructions and every parenthetical prompt below as you fill it
in.
-->

**Status:** open | convention difference | resolved
<!-- "open" — cause not established; non-blocking, evidence preserved.
     "convention difference" — the systems model something differently and each
     is internally correct; nothing is pending.
     "resolved" — a defect was found and fixed; keep the document as the record
     of what the symptom looked like. -->

**Parked:** yes | no
<!-- A separate axis from status. Park when the disagreement would otherwise
     leave `just validate-oracle` permanently red and fixing it is not the next
     step — an "open" cause, or a "convention difference" that no scenario
     arrangement suppresses. Parking means adding the scenario to the `parked`
     list in `test/unit/cairos_engine/cross_validate_oracles.ml`, pointing at
     this file's path. That binary checks, before it compares anything, both
     that the path exists and that the entry names a scenario the manifest
     lists; either failing is a tooling error. So this document must be
     committed before the scenario is parked, not after. -->

**Scenario(s):** <scenario id(s), or "all three">

**Systems disagreeing:** <which of cairos / vectorbt / nautilus, and how they split>

**Owner:** <a named party at a named moment — "the CG-6 author, at feature
authoring" — not a destination. "Deferred to the Layer 3 work" names no one and
will be read by no one. If a follow-up is described below, this field says who
picks it up and when; if nothing is outstanding, say "none — nothing
outstanding".>

---

## The three systems

<!-- Restate, briefly, what each system is and why it is here. A fresh reader
     must not have to infer it. -->

- **cairos** — the engine under test, run in-process by
  `test/unit/cairos_engine/cross_validate_oracles.exe`.
- **vectorbt** — third-party vectorised order engine, `vectorbt_oracle.py`,
  output committed under `oracle_fixtures/vectorbt_<scenario>_equity.csv`.
- **nautilus** — third-party event-driven backtester with a simulated venue,
  `nautilus_oracle.py`, output committed under
  `oracle_fixtures/nautilus_<scenario>_equity.csv`.

## Scenario inputs

<!-- The full inputs, inline. Not a pointer to oracle_scenarios.py — a reader
     with only this file must be able to reproduce the run by hand. Prices per
     bar per instrument, the rebalance schedule with its target weight vectors,
     commission, slippage, and the bar count. If the discrepancy is only
     reachable on a counterfactual variant of a shipped scenario, state the
     variant in full and say plainly that it is not a shipped scenario. -->

## All three outputs

<!-- The per-bar equity path from each system, plus the final NAV, at enough
     precision to show the disagreement. A table with one column per system is
     the readable form. State the pairwise deviations explicitly rather than
     leaving them to be subtracted. -->

| bar | timestamp | cairos | vectorbt | nautilus |
|-----|-----------|--------|----------|----------|

## Each system's translation, and the grounds for believing it

<!-- The load-bearing section. For each system: what had to be done to make it
     comparable, and *why that is believed correct* — an empirical probe, a
     mutation that reddens if the translation is wrong, a hand derivation. A
     translation asserted rather than demonstrated is exactly how an oracle
     agrees for the wrong reason, so say which of the three it is.

     Distinguish, explicitly:
       - MODEL TRANSLATION — a testable claim that two configurations simulate
         the same book. Falsifiable: if it were wrong the fixtures disagree.
       - SCENARIO ARRANGEMENT — inputs shaped so a real difference has nothing
         to act on. Claims nothing, confirms nothing, and must never be allowed
         to read as agreement. -->

### cairos

### vectorbt

### nautilus

## What has been ruled out

<!-- Explicitly, so the next session does not repeat it. For each: what was
     checked, how, and what was observed. "Not investigated" is a legitimate
     entry and is more useful than silence. -->

## The open question

<!-- One paragraph, ending in a question. What would settle it, and what
     evidence would distinguish the candidate explanations. If the status is
     "convention difference" or "resolved", say what remains anyway — a
     documentation gap, an uncovered behaviour, a follow-up someone owns — or
     state that nothing does.

     Anything outstanding named here must match the **Owner** field above, and
     must also be visible from wherever that work gets scheduled. Nothing reads
     `validation/discrepancies/` at planning time, so a follow-up recorded only
     here is a follow-up nobody will find. -->

## Reproducing

<!-- The exact commands. If a figure here cannot be reproduced by running
     something in this repository, say so in this section rather than leaving a
     reader to discover it. -->

```
just validate-oracle
```
