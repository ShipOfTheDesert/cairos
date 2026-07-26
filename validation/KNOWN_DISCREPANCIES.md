# Known discrepancies

Index of every recorded disagreement between the three implementations of the
shared backtest scenarios: the Cairos engine, the vectorbt oracle, and the
Nautilus oracle. This file is an index only — each entry's evidence lives in its
own self-contained investigation document under `discrepancies/`, written to be
pasted whole into a fresh session that holds no prior context.

| id | scenario(s) | status | document |
|----|-------------|--------|----------|
| CD-001 | all three | convention difference | [Constant-weight vs constant-share portfolio model](discrepancies/001-constant-weight-vs-constant-share.md) |
| CD-002 | counterfactual variant of `two_instruments_one_rebalance` | convention difference | [Targeting basis: post-cost NAV vs pre-cost value](discrepancies/002-targeting-basis-post-cost-nav.md) |
| CD-003 | counterfactual variant of `long_short_flip` | convention difference | [Turnover basis: nominal vs drifted held weight](discrepancies/003-turnover-basis-nominal-vs-drifted.md) |

## Statuses

- **convention difference** — the systems model something differently and each
  is internally correct. Nothing is broken and nothing is pending. The document
  records it because a future reader would otherwise have to rediscover it from
  first principles, which is the part that decays fastest.
- **open** — a disagreement whose cause is not established. Non-blocking, with
  the evidence preserved.
- **resolved** — a defect was found and fixed. The document stays as the record
  of what the symptom looked like.

An entry is not an admission of a defect.

## Parking, which is a separate axis from status

Parking is about whether a scenario still *reaches* the comparison, not about
which status it carries. A discrepancy is parked — added to the `parked` list in
`test/unit/cairos_engine/cross_validate_oracles.ml` — when it would otherwise
leave `just validate-oracle` permanently red and fixing it is not the next step.
That covers an **open** disagreement whose cause is not established, and equally
a **convention difference** that no scenario arrangement suppresses. It does not
follow from the status: all three entries above are convention differences and
none of them is parked, because each is either bridged by the model translation
or arranged out of reach.

A parked entry is skipped with a printed notice naming its document, so it can
neither redden the recipe nor vanish quietly. Two guards run before any scenario
is compared, so the binary refuses to start rather than failing partway through a
run that has already printed green lines:

- the investigation document must exist on disk — parking without the evidence
  is a scenario silently dropped from the comparison;
- the parked entry must name a scenario the manifest actually lists — otherwise
  the scenario it was meant to skip gets compared anyway while the summary line
  still counts a park that is not happening.

Both are tooling failures (exit 2), not mismatches.

## Adding an entry

A disagreement where two systems agree against one identifies the odd system
out; it is a finding on that system and is investigated rather than parked. A
three-way split identifies no culprit, and so does a case where pairwise
agreement fails to be transitive at the comparison tolerance; those are what
parking is for. `cross_validate_oracles.exe` prints the classification and all
three values, and exits 1 either way — it does not decide that a disagreement is
acceptable.

Copy `discrepancies/TEMPLATE.md` to `discrepancies/NNN-<slug>.md`, fill every
section, and add one row above. Both branches produce a document: a finding
investigated before merge is no less worth writing down than a parked one, since
the document is what a later deep-investigation session is handed.

The three entries above are all convention differences, all currently
suppressed or bridged, and none is parked — `cross_validate_oracles.exe`
compares all three shared scenarios and they agree to 4.441e-16 per bar.
