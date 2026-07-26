# Provenance — clean-room `backtest_reference`

This file records how `validation/reference.py`'s `backtest_reference` was
produced. It is the *production-conditions* record; the substantive statement of
what the reference encodes lives inline, in that function's own `PROVENANCE:`
block, so the oracle stays auditable without reaching for this file.

Independence is a claim about how a document was written. It cannot be verified
after the fact by reading the result, which is why this file exists and why §6
demonstrates the claim empirically instead of restating it.

**Background.** The reference this replaces was a line-by-line transliteration of
the OCaml engine — same bar loop, same expressions, same variable names — and
said so in its own header. A transliteration cannot detect a modeling error,
because any modeling error present in the implementation is present identically
in the reference; the two agree and the comparison stays green. That failure mode
shipped a real defect in this project: a transaction-cost notional dimensioned in
*price* rather than in *NAV*, present on both sides, which cross-validation could
not see. Only the hand-derived Layer 1 tests caught it.

The specification documents cited by name below are maintained outside this
repository and cannot be opened by a reader of it. Every clause they contain that
the reference depends on is therefore restated substantively in §5, which is the
artefact that must stand alone.

---

## 1. Source set — the fragments the reference encodes

Four fragments, documents only. This is a wider set than the two documents
originally proposed for this work, for the reason given in §2: the narrower pair
is an *incomplete* specification, and an author restricted to it would reproduce
two known-wrong behaviours.

| Id | Fragment | What it supplies |
|----|----------|------------------|
| **F1** | Backtest execution conventions (RFC 0052) — options-considered entries OC-1 through OC-11, and its "Implementation Guidance" section | Signal timing, weight representation, cost model shape and NAV-update ordering, rebalance-schedule handling, the two-frequency loop body, the mark-to-market formula, entrypoint preconditions, initial state, and the trade-accumulator ordering |
| **F2** | The same document's post-implementation amendment §A1 ("Cost notional: price → NAV") | The cost *notional*: turnover as a fraction of NAV, replacing the price-dimensioned notional pinned in the frozen body |
| **F3** | Backtest result type (PRD 0053) — FR-1 through FR-12 and Decisions 1, 2, 3, 7 | The four result fields, the seven trade fields, the round-trip trade accounting model, the segment `pnl` formula, and the end-of-backtest force-close convention |
| **F4** | Vectorized backtest loop (RFC 0056) — Implementation Decisions 1 and 2 | The weighted-return mark-to-market form that keeps NAV positive on shorts, and the proportional sign-flip cost split |

Nothing else was supplied. In particular no OCaml source, no existing Python
reference, and no test file.

## 2. Supersession order

Stated verbatim in the generation prompt, because an author reading the
conventions document's *body* without it reproduces the defect described above:

> §A1 supersedes the body cost formula; RFC 0056 Decision 1 supersedes OC-6's
> literal MTM formula; RFC 0056 Decision 2 defines the sign-flip cost split.

In plain terms, for a reader without those documents:

1. The conventions document's frozen body pins the per-instrument cost as
   `(commission + slippage) * |weight_delta| * execution_price`. That formula is
   **superseded**. The notional is turnover in NAV terms:
   `(commission + slippage) * |weight_delta| * nav`. The frozen body is left
   unedited as the historical record, so it still reads the old way; the
   amendment is what is in force.
2. The conventions document pins mark-to-market as
   `nav_t = nav_{t-1} * Σ_j (w_j * p_{t,j} / p_{t-1,j})`. Read literally that
   goes negative whenever `Σ_j w_j < 0` — a single-instrument short — and is
   dimensionally undefined whenever `Σ_j w_j ≠ 1`. It is **superseded** by the
   weighted-return form, which coincides with it at full investment and behaves
   correctly otherwise.
3. Neither document specifies how a sign-flip rebalance's single per-instrument
   cost is shared between the trade it resolves and the trade it incepts at the
   same bar; one of them explicitly defers per-trade cost decomposition. The
   proportional split **fills that gap** rather than superseding anything.

The first two are the two behaviours a narrower source set would have got wrong,
and they are precisely the behaviours the reference exists to check.

## 3. Exclusions — what was kept out of the author's context

Held out of the clean-room session, and named here so the claim is falsifiable by
anyone who watched the session rather than vague:

- `lib/cairos_engine/cairos_engine.ml` — the implementation under validation.
- `lib/cairos_engine/cairos_engine.mli` — its interface, whose doc-comments
  paraphrase the same conventions and would have leaked the implementation's
  reading of them.
- The previous `backtest_reference` function body in `validation/reference.py`,
  and the commentary block above it, which together were the transliteration
  being replaced.
- `test/unit/cairos_engine/test_known_outcomes.ml` — the hand-derived Layer 1
  scenarios. These are independently derived and would not have leaked the
  *implementation*, but they do carry worked numeric answers, and an author who
  has the answers is no longer deriving.
- Any generated fixture under `validation/fixtures/`, for the same reason.
- The implementation session's own working notes — task logs, reflections,
  pre-review reports. These are the easiest contamination route of all: they
  carry worked numbers, the implementer's reading of every ambiguity, and the
  arguments for it, so an author who has them is reproducing a conclusion rather
  than reaching one. They are also the class most often pulled in accidentally by
  a bulk sweep of untracked files or a directory glob.
- Sections 1 through 4 and 6 of *this* file. §5 is pasted into the clean-room
  session on its own; the rest names the specific defects the reference is being
  built to detect, which is not knowledge its author should be steered by.

The last two are why the session is opened by pasting §5 as literal text, never
by handing it a path, a glob, or a "read the validation directory" instruction.

Deliberately **not** excluded, and supplied inside the prompt instead:

- The input generators (`backtest_prices_df`, `backtest_signals_df`) and the
  scenario constants. These define the inputs, not the model.
- The `_Trade` dataclass, the function signature, the CSV writers' output shape,
  and the post-conditions the fixture-generation path asserts. These are the
  interoperation contract; the replacement has to be drop-in for a comparison
  binary that is not modified.

## 4. Production protocol

The clean-room session is opened fresh, receives §5 and nothing else, and returns
a single self-contained code block: the `backtest_reference` function plus its
new inline `PROVENANCE:` block. It never opens `validation/reference.py`.

Splicing that block into the file is a mechanical step performed afterwards,
outside the clean-room session, and is anchor-based so it needs no reading of the
regions being removed. Two regions are replaced:

```bash
# Region A — the old commentary/provenance block: from the section banner down to
# the line before the first scenario constant.
# Region B — the old function: from its `def` line down to the line before the
# next top-level definition.
awk '/^# --- Backtest engine fixtures/,/^BACKTEST_INSTRUMENTS = /' validation/reference.py | head -n -1
awk '/^def backtest_reference\(/,/^def _date_str\(/' validation/reference.py | head -n -1
```

Run those two commands first to confirm the anchors still bound exactly the
intended text, then delete each region and insert the new block in its place.
Everything else in `validation/reference.py` — the non-engine references, the
scenario constants, `_Trade`, the input generators, the writers, the
post-condition check, and `main()` — is left untouched.

**Attestation.** Clean-room rewrite performed 2026-07-25.

*Mechanism — and a deliberate substitution.* The author was a fresh **Claude.ai
conversation**, not the fresh Claude Code session this protocol originally
described. The substitution was made because it is strictly stronger, and the
distinction is the whole point of an attestation. A Claude Code session retains
filesystem access whatever its prompt says, so "never opens
`validation/reference.py`" would have been a condition the author was *instructed*
to honour and trusted to have honoured — unverifiable afterwards by exactly the
argument §3 makes about unfalsifiable claims. A Claude.ai conversation has no
filesystem and no repository access at all, so every artefact in §3's exclusion
list was unreachable **by construction rather than by instruction**. What this
attestation records is therefore a property of the environment, not a report of
the author's discipline.

This is only affordable because §5 was built self-contained: it carries the input
generators, the scenario constants, the `_Trade` shape, the function signature,
and the full fixture output contract (file names, column headers, `%.17g` floats,
timestamp format, trade-column and trade-ordering rules), so a drop-in replacement
could be produced with no repository access whatsoever.

*Input.* §5 of this file, lines 151–541 — the fenced prompt body, exclusive of the
fences — pasted as literal text, and nothing else. No path, no glob, no directory
instruction, per §3's closing note.

*Exclusions, by construction.* Every artefact §3 lists was absent: the engine
implementation and its interface, the previous `backtest_reference` and its
commentary block, the Layer 1 known-outcomes tests, every generated fixture, the
implementation sessions' working notes and reflections, and §§1–4 and 6 of this
file. None of these could have been reached from the authoring environment.

*Return and splice.* The session returned one contiguous block — the new in-file
`PROVENANCE:` block followed by `backtest_reference`. Splicing was mechanical and
performed outside the clean-room session, anchor-verified before cutting: region A
(the old commentary block) and region B (the old function) were confirmed to match
their anchors exactly once each, and neither region's content was read or printed.
The returned block was spliced verbatim, kept contiguous at region B rather than
split across both regions, because provenance directly above the function it
documents is what "record provenance alongside the reference" asks for; region A's
section banner was preserved, since it labels the scenario constants and input
generators that remain.

*Outcome.* Fixtures regenerated and `test/unit/cairos_engine/cross_validate.exe`
run with the comparison binary at zero diff: `Cross-validate engine: OK`. The
fixture generator's reconciliation post-condition did not trip.

*Caveat on the strength of that agreement.* The prompt supplied the reconciliation
post-condition (`1 + Σ pnl` equals the final equity value) as a stated
post-condition, because the generation path asserts it and a replacement that
violated it could not run at all. The author's own comment records that this
identity is what drove its resolution of the P&L segment-versus-accrual tension.
The agreement on `pnl` is therefore partly attributable to the prompt, not wholly
to independent derivation — the equity, returns, cost, and trade-structure
agreements carry no such caveat. This is recorded because an unqualified "three
independent systems agreed" would overstate what happened on that one field.

*Follow-up, 2026-07-25 — the author's resolution was right, and the specification
was wrong.* The reference's silence (1) identified the tension between the
specification's segment-sum P&L clause and the reconciliation identity it is paired
with, and resolved toward accrual on the ground that only accrual satisfies the
specification as a whole. That reasoning has since been confirmed by hand
arithmetic on two analytic scenarios and on the 50-bar cross-validation fixture:
the segment-sum clause fails the identity under **both** readings of its ambiguous
segment endpoints — by roughly 7.9% of terminal NAV on the fixture under the
reading the clause's own field-linkage implies, and by 8.4e-04 under the more
charitable one — and all 28 fixture trades diverge from the accrued values,
including all 17 that have no size adjustment at all. The clause was amended and
the accrual form is now the pinned contract; the engine and this reference were
both already correct.

Two consequences for the caveat above. Supplying the identity in the prompt was
**faithful transcription, not steering**: the specification states the identity as
a property in its own right, as the stated justification for the force-close
convention, so an author reasoning from documents alone would have met it whether
or not this prompt restated it. The residual caveat nevertheless stands as
written — the identity narrowed the author's options on that one field, and
"agreed on `pnl`" remains a weaker claim than "independently derived `pnl`". What
has changed is that the narrowing pointed at the correct answer, which is a fact
about the specification rather than evidence about the reference.

## 5. Generation prompt (verbatim)

Everything between the fences is the prompt. It is committed verbatim so a later
reader can judge whether it steered the author toward a particular answer, and so
the rewrite can be reproduced.

````text
You are writing a reference implementation of a backtest engine in Python, to be
used as a cross-validation oracle against a separate OCaml implementation of the
same specification.

You must write it from this specification alone. The OCaml implementation, any
previous Python reference, and the implementation's test suite are deliberately
not available to you. Do not ask for them; if you find yourself wanting to check
what the implementation does, re-read the specification instead and, if it is
genuinely silent, say so in a comment rather than guessing toward what an
implementation "probably" does. A reference that agrees with the implementation
because it was written from the implementation proves only that the code equals
its own port. Your job is to be able to disagree with it.

## 0. Supersession order — read this before the conventions

  §A1 supersedes the body cost formula; RFC 0056 Decision 1 supersedes OC-6's
  literal MTM formula; RFC 0056 Decision 2 defines the sign-flip cost split.

That sentence is reproduced verbatim from the specification. Its content is
already folded into the conventions restated below — C3 states the cost notional
in force, C4 states the mark-to-market form in force, and C10 states the
sign-flip split. You do not need to reconcile anything; this note exists so you
know that where a superseded formula might be quoted to you from elsewhere, the
version below is the one that governs.

## 1. What you are writing

One Python function plus a comment block, as a single self-contained code block:

```python
@dataclass
class _Trade:
    entry_timestamp: pd.Timestamp
    exit_timestamp: pd.Timestamp
    instrument: str
    entry_price: float
    exit_price: float
    pnl: float
    holding_period_bars: int


def backtest_reference(
    prices_df: pd.DataFrame,
    signals_df: pd.DataFrame,
    rebalance_bar_indices: list[int],
    commission: float,
    slippage: float,
) -> tuple[pd.Series, pd.Series, list[_Trade]]:
    ...
```

The `_Trade` dataclass already exists in the target file and must not be
redefined or altered — it is shown so you know the exact field names, types, and
declaration order. Write only the function, and the `PROVENANCE:` comment block
described in §6.

`pandas as pd` and `numpy as np` are already imported. Python ≥ 3.12.

## 2. Inputs

`prices_df` — a `DataFrame` with a `DatetimeIndex` of `N` daily bars and one
column per instrument, in a fixed column order. There is exactly one price per
(bar, instrument): **this single value serves as both the bar's close, used for
marking positions to market, and the bar's open, used as an execution price.**
There is no OHLC data. Do not synthesise any.

`signals_df` — same index and same columns. The cell at (bar, instrument) is the
*target weight* requested for that instrument at that bar. Only the rows at
rebalance bars are ever read; the other rows exist but are not consulted.

`rebalance_bar_indices` — a sorted list of integer row positions into
`prices_df.index`. These are the rebalance bars.

`commission`, `slippage` — dimensionless fractions of notional (e.g. `0.001` is
10 basis points), not basis points and not currency amounts.

You may assume the inputs are already valid: both frames share an index and a
column order, the rebalance list is non-empty, every rebalance bar index is
within range and is not the last bar, and at least one target weight at some
rebalance bar is non-zero. The real entry point validates all of that before
calling; your function does not need to re-check it.

For concreteness, these are the inputs the fixtures are generated from. They are
given so you can reason about which code paths are exercised. Do not rewrite
them; they already exist in the target file.

```python
BACKTEST_INSTRUMENTS = ["A", "B", "C", "D", "E"]
BACKTEST_N_BARS = 50
BACKTEST_REBALANCE_BAR_INDICES = list(range(2, BACKTEST_N_BARS, 5))
BACKTEST_COMMISSION = 0.001
BACKTEST_SLIPPAGE = 0.0005
BACKTEST_PRICE_SEED = 42
BACKTEST_SIGNAL_SEED = 43
BACKTEST_START_DATE = "2024-01-01"


def backtest_dates() -> pd.DatetimeIndex:
    return pd.date_range(BACKTEST_START_DATE, periods=BACKTEST_N_BARS, freq="D")


def backtest_prices_df() -> pd.DataFrame:
    rng = np.random.default_rng(BACKTEST_PRICE_SEED)
    n_inst = len(BACKTEST_INSTRUMENTS)
    log_returns = rng.normal(0.0, 0.01, size=(BACKTEST_N_BARS - 1, n_inst))
    prices = np.empty((BACKTEST_N_BARS, n_inst))
    prices[0] = 1.0
    prices[1:] = np.exp(np.cumsum(log_returns, axis=0))
    return pd.DataFrame(prices, index=backtest_dates(), columns=BACKTEST_INSTRUMENTS)


def backtest_signals_df() -> pd.DataFrame:
    rng = np.random.default_rng(BACKTEST_SIGNAL_SEED)
    n_inst = len(BACKTEST_INSTRUMENTS)
    sig = np.zeros((BACKTEST_N_BARS, n_inst))
    for bar in BACKTEST_REBALANCE_BAR_INDICES:
        sig[bar] = rng.uniform(-0.5, 0.5, size=n_inst)
    return pd.DataFrame(sig, index=backtest_dates(), columns=BACKTEST_INSTRUMENTS)
```

Note what that input distribution exercises: target weights are drawn uniformly
from `[-0.5, 0.5]` independently per instrument per rebalance, so the portfolio
is not fully invested, short positions are common, sign flips between consecutive
rebalances are common, and same-direction size adjustments are common. Every
branch of the trade accounting model in C8 is reached.

## 3. The conventions to encode

These are restatements of the specification. Where a clause is silent on
something, it is silent — say so in a comment and choose the reading you can
defend from the rest of the specification, rather than inventing a rule.

**C1 — Signal timing: close-to-open.** A signal read at rebalance bar `r`
executes at the *open of bar `r + 1`*. Every execution price, entry timestamp and
exit timestamp for a rebalance-driven event is therefore the price and timestamp
of bar `r + 1`, never of bar `r`. The rationale is that the signal is computed
from bar `r`'s close, so executing at bar `r`'s close would trade on information
only knowable once the bar has ended.

**C2 — Position representation: fractional weights, unvalidated.** A weight is a
fraction of current portfolio NAV. Negative means short, zero means flat, and
absolute values above `1.0` mean leverage. There is no range validation and no
clamping. Weights are not required to sum to `1.0`; the unallocated remainder is
cash and earns nothing.

**C3 — Cost model.** Costs are incurred at rebalance bars only. At rebalance bar
`r`, for each instrument `j`, let `w_held_j` be the weight held coming into the
bar and `w_target_j` the target read from the signal frame, so
`Δw_j = w_target_j − w_held_j`. Let `nav` be the *pre-cost* NAV at bar `r`: the
NAV after marking the previously-held weights to market from bar `r − 1` to bar
`r`, and before any deduction. Then

```
cost_j     = (commission + slippage) * abs(Δw_j) * nav
total_cost = Σ_j cost_j
nav_after  = nav − total_cost
```

and only then do the held weights become the target weights. The ordering is
load-bearing: the new positions are funded from the post-cost NAV, not from the
pre-cost NAV.

Note the notional carefully. `abs(Δw_j)` is a dimensionless fraction of NAV and
`(commission + slippage)` is a dimensionless fraction of notional, so the
notional they multiply must be a money amount — turnover in NAV terms. It is
**not** the instrument's price level. Two strategies identical except that one
instrument is quoted at 400 and the other at 4 must incur identical costs.

**C4 — Mark-to-market.** For a step from bar `t − 1` to bar `t`, with `w_j` the
weight held across that step:

```
nav_t = nav_{t-1} * (1 + Σ_j w_j * (p_{t,j} / p_{t-1,j} − 1))
```

This is the weighted-return form. It coincides with the "sum of weighted price
relatives" form when `Σ_j w_j = 1`, and unlike that form it stays positive for a
short position and is well defined when the portfolio is not fully invested —
both of which this input distribution produces constantly. A zero-weight
portfolio drifts trivially: `nav_t = nav_{t-1}`.

**C5 — Equity curve.** One value per price-frame bar, `N` values for `N` bars,
carrying `prices_df.index` as its index.

- `equity[0] = 1.0`. NAV is normalised; there is no notion of currency.
- At a bar that is not a rebalance bar, the value is the mark-to-market result
  from C4 using the currently held weights.
- At a rebalance bar `r`, the value is `nav_after` from C3 — that is, marked to
  market from `r − 1` to `r` with the *previously* held weights, then reduced by
  the total cost. It is post-cost and pre-*next*-mark-to-market.
- The new target weights take effect at bar `r` and govern the step from `r` to
  `r + 1`.
- After the last rebalance the held weights stay fixed and positions continue to
  be marked to market through the end of the frame. This is intended behaviour,
  not a truncation bug.

**C6 — Held weights.** The weight in effect for instrument `j` at bar `t` is the
target weight of the most recent rebalance bar `≤ t`, and is the weight that
governs the mark-to-market step from bar `t` to bar `t + 1`. Bars before the
first rebalance carry `0.0`. These are *held* weights — what the portfolio
actually owns — not target weights forward-filled from the signal frame; the two
differ in general. You do not return this panel (see §4), but you need it
internally and it is the quantity C4 consumes.

**C7 — Returns.** The per-bar arithmetic return of the equity curve — the
percentage change from the previous bar. Same index, same length as the equity
curve. The first value is `NaN`: there is no prior bar to divide by.

Encode that as the contract rather than relying on a library default to coincide
with it. No fill, no interpolation, no dropping: the leading `NaN` is a sentinel
that survives into the fixture as an empty cell and is compared as `NaN` on the
other side. If the percentage-change helper you reach for forward-fills or
back-fills by default, override it explicitly, even though on this input the
equity curve is finite and strictly positive throughout so the default would
happen to agree.

**C8 — Trade accounting: round trip, per instrument.** One trade record spans one
inception → resolution of a held position in one instrument.

- *Inception*: the held weight goes from zero to non-zero, or a sign flip opens
  the new side (a flip is a close and an open at the same execution bar).
- *Resolution*: the target goes to zero, or a sign flip closes the prior holding,
  or the backtest ends with the position still open (force-close, C11).
- *Same-direction size adjustment*: held and target are both non-zero, share a
  sign, and differ in magnitude. This updates the in-flight record and does
  **not** produce a new record and does **not** resolve the existing one. A
  strategy that adjusts a position's size fifty times before closing it produces
  one trade, not fifty-one.

A cost is charged at every rebalance with a non-zero `Δw_j` regardless of which
of those the event is; how it is attributed to trades is C10.

**C9 — Trade fields.** For each record:

- `entry_timestamp` — the timestamp of the inception's execution bar, i.e. bar
  `r_inception + 1`. Same-direction size adjustments do not change it.
- `exit_timestamp` — the timestamp of the resolution's execution bar, i.e. bar
  `r_resolution + 1`; or, for a force-close, the timestamp of the last bar.
- `instrument` — the column name in the price frame.
- `entry_price` — the price at `entry_timestamp`. Same-direction size adjustments
  do not change it; it stays the *original* inception price. An adjustment's own
  execution price affects `pnl` and nothing else.
- `exit_price` — the price at `exit_timestamp`.
- `pnl` — realised P&L for the whole span, net of every cost attributed to it
  (C10).
- `holding_period_bars` — `exit_bar_index − entry_bar_index`, as an integer row
  distance in the price frame. Same-direction adjustments do not subdivide it.

**C10 — `pnl`.** For a record with inception at execution bar `i_0`, `K ≥ 0`
same-direction size adjustments at execution bars `i_1 < … < i_K`, and resolution
at execution bar `i_R` (define `i_{K+1} := i_R`), the specification states the
P&L as a sum over constant-weight segments:

```
Σ_k  w_k * NAV_at_segment_entry_k * (p_{i_{k+1}} / p_{i_k} − 1)
```

minus every cost attributed to this instrument at the inception, at each
adjustment, and — for resolutions that are not force-closes — at the resolution.

Cost attribution: at an inception, an adjustment, or a plain resolution, the
whole per-instrument cost `cost_j` belongs to the one trade affected. At a *sign
flip*, the single per-instrument cost is split between the trade being resolved
and the trade being incepted in proportion to each side's contribution to the
turnover:

```
closing_share = cost_j * abs(w_old) / (abs(w_old) + abs(w_new))
opening_share = cost_j * abs(w_new) / (abs(w_old) + abs(w_new))
```

For a sign flip `abs(Δw) = abs(w_old) + abs(w_new)`, so the two shares sum to
`cost_j` exactly and nothing is lost or double-counted. A symmetric flip splits
50/50; an asymmetric flip does not.

Note that the segment expression above and the bar-by-bar accrual implied by C4
and C5 are the same number only under particular conditions — for instance a
fully-invested single instrument, or a segment spanning a single
mark-to-market step. Away from those, "the NAV at segment entry" and "the running
NAV at each step" are different quantities. The specification also pins the
identity in §5's post-conditions, which ties the sum of all `pnl` to the equity
curve. Decide which reading satisfies the specification as a whole, implement
that one, and state your reasoning in a comment. Do not leave the tension
unremarked, and do not resolve it by consulting anything outside this prompt.

**C11 — End-of-backtest force-close.** At the end of the run, every instrument
still holding a non-zero weight produces a final record. Its `exit_timestamp` is
the last bar of the price frame, its `exit_price` is the *close* of that last bar
— which, per §2, is that bar's single price — and **no cost is charged**, because
costs are incurred at rebalance bars only and the end of the data is not a
rebalance. Using the last bar's close rather than a synthesised next open is what
keeps the sum of realised P&L consistent with where the equity curve ended.

## 4. Output contract

Return exactly three objects, in this order:

1. `equity_curve : pd.Series` — indexed by `prices_df.index`, `N` float values,
   per C5.
2. `returns : pd.Series` — indexed by `prices_df.index`, `N` float values, first
   value `NaN`, per C7.
3. `trades : list[_Trade]` — see the ordering rule below.

Do **not** return the held-weights panel. It is not part of this contract and no
fixture consumes it.

**Trade ordering is part of the contract**, because the comparison is positional:
row `i` of the emitted trade fixture is compared against record `i` of the OCaml
engine's trade list. The specification pins the accumulator as append-on-
resolution, so:

- records appear in order of the bar at which they *resolve*;
- within a single resolution bar, in price-frame column order;
- all force-closed records come last, in price-frame column order, since they
  resolve at the end of the run.

Note the consequence for sign flips: the closing record resolves at the flip bar
and the record it incepts resolves later, so they are not adjacent in general.

**Post-conditions.** The fixture-generation path asserts these on your output
before writing anything, and aborts the whole generation run if any fails:

```python
assert equity_curve.iloc[0] == 1.0
assert (equity_curve > 0.0).all()
# 1 + Σ pnl equals the final equity value, to max(len(trades), 1) * 1e-12
```

These are specification consequences, not extra requirements: the initial state
is a zero-weight portfolio at NAV `1.0`; the weighted-return form keeps NAV
positive; and the force-close convention exists precisely so that realised P&L
reconciles with the equity curve.

**Fixture shape**, for context — the writers already exist and you are not
writing them, but your returned objects must be drop-in for them, and this is the
shape the OCaml comparison binary parses:

| File | Header | Cells |
|------|--------|-------|
| `backtest_equity_curve.csv` | `timestamp,value` | timestamp `%Y-%m-%d`, value `%.17g` |
| `backtest_returns.csv` | `timestamp,value` | as above; `NaN` is written as an empty cell |
| `backtest_trades.csv` | `entry_timestamp,exit_timestamp,instrument,entry_price,exit_price,pnl,holding_period_bars` | timestamps `%Y-%m-%dT%H:%M:%SZ`, floats `%.17g`, `holding_period_bars` as a plain integer |

The trade column order matches the `_Trade` field declaration order and is
pinned; the comparison binary asserts the header string before parsing. The
comparison tolerance is `1e-10` absolute on floats, with exact equality on
integers, timestamps and instrument names — which is why float formatting is
`%.17g` throughout, so no precision is lost through the CSV round trip.

## 5. How to write it

**The equity, returns and held-weights path must be vectorised** — a
forward-filled weight matrix against a matrix of per-bar price relatives, with
costs applied at the rebalance rows. No bar-by-bar Python loop. This is not a
performance requirement; the fixture is fifty bars. It is an *independence*
requirement: a reference whose control flow mirrors the implementation's cannot
disagree with it structurally, and a vectorised formulation reaches the same
numbers by a different route.

**The trade scan is sequential**, and must be labelled as such in the code. The
trade state machine — in-flight records updated by same-direction adjustments,
flips resolving and incepting at one bar, segment-summed P&L, force-close at the
end — is inherently sequential, and a vectorised version of it would itself need
validating. Be honest about what that costs: the equity path can claim
independence *structurally*, the trade scan can only claim it *by provenance* —
it was written from this specification, with the implementation unavailable.
Write the comment so it says that, and do not let it imply more.

Use `numpy`/`pandas` idioms freely. Prefer expressing a clause the way the
specification states it over the way that would be shortest.

## 6. Provenance comment block

End with a `PROVENANCE:` comment block placed above the function, stating:

- that the reference is derived from the specification with the implementation
  unavailable to the author, and what was excluded;
- the contract clauses it encodes, restated **substantively** — a reader of this
  repository cannot open the specification documents, so a bare citation such as
  "per OC-3" is useless to them. State the rule, not its address. Do not cite any
  `docs/` or `planning/` path; those directories are not committed.
- the independence claim, scoped as §5 requires: structural for the equity path,
  provenance-only for the trade scan;
- explicitly, that the cost notional is turnover as a fraction of NAV and is
  independent of the price level, so nobody "fixes" it back toward a
  price-dimensioned formula;
- any place where you found the specification silent or self-tensioned, and how
  you resolved it.

Do not describe the reference as a transliteration or a port of anything. If it
is one, this task has failed.
````

## 6. Independence demonstration — mutation table

Each row is a deliberate modeling defect seeded into the engine, and the harness
that caught it. A mutation that nothing catches is a finding in its own right and
stays in the table with an empty "caught by" cell rather than being dropped.
Every mutation is reverted before the work closes; a diff under `lib/` at that
point is a defect.

Each row records which layers caught it — the interesting rows are the ones where
only one did — and what **kind** of evidence the catch is, because the
reference's independence is not uniform across fields:

- **Independent.** The reference derived this from the contract clauses alone and
  agreement is evidence about the model. Equity, returns, cost notional, cost
  attribution, and trade structure (counts, entry/exit fields, holding periods,
  ordering) are all in this class. Rows touching only these carry no caveat.
- **Partly spec-forced.** `pnl` is not fully in the class above. The generation
  prompt supplied the reconciliation post-condition (`1 + Σ pnl` equals the final
  equity value) because the fixture path asserts it and a replacement violating it
  could not run at all, and the reference's own commentary records that identity
  as what drove its resolution of the segment-versus-accrual question. A seeded
  `pnl`-semantics defect is therefore caught partly *because the identity forces
  the answer*, not solely because two authors derived the same model
  independently. A row whose only catch is a `pnl` mismatch must say so rather
  than read as unqualified independent confirmation.

The distinction is not hypothetical. The specification's own `pnl` clause and the
identity it is paired with are not satisfiable together: the clause's literal
segment form does not reproduce the identity on the cross-validation fixture under
either reading of its segment endpoints. So "the reference agrees with the engine
on `pnl`" and "the reference independently derived the engine's `pnl` semantics"
are different claims, and only the first is established.

Run 2026-07-25. Each mutation applied alone to `lib/cairos_engine/cairos_engine.ml`,
both layers run, then reverted from a pre-mutation copy and verified by `md5sum`
(`e4129bab6ad108bac1d24ec93c97a97f`). "Layer 1" is the hand-derived analytic
scenarios, "Layer 2" the specification-derived reference comparison, "Layer 3" the
QCheck properties (not a deliverable of this work, but recorded where it fired
because it changes what the other two prove).

| # | Mutation | Caught by | Symptom | Evidence class |
|---|----------|-----------|---------|----------------|
| M1 | Cost notional restored to the superseded price-dimensioned form — `(c + s) * abs(Δw) * execution_price` instead of `* nav` | Layer 1 (6/7), Layer 2, and the NaN-guard suite | L1: every cost-bearing scenario. L2: `equity_curve` row 2, expected 0.99772772502656981 got 0.99771777803790429 (diff 9.95e-06). Also reddened `validate_inputs` 14 — an unheld instrument's NaN price reaches the cost line and poisons NAV, so this defect silently breaks the zero-weight NaN guard too. | **Independent.** Cost notional; derived from the contract clause, not from the identity. This is the defect that shipped: the previous transliterated reference contained it identically and the comparison stayed green. |
| M2 | Mark-to-market restored to the superseded literal form — `nav_{t-1} * Σ_j (w_j * p_t / p_{t-1})`, applied where a position is held | Layer 1 (4/7), Layer 2, Layer 3 (4 properties) | L1: `equity_curve [3]` on the short and partial-investment scenarios. L2: `equity_curve` row 3, expected 0.99944342587055468 got **−0.35501206620187609**. L3: `equity_curve_strictly_positive`, the pnl-sum identity, `commission_monotonic_in_final_nav`, `zero_cost_equals_frictionless_recursion`. | **Independent.** Equity path. Note *which* Layer 1 tests survived: the three fully-invested ones, where the two forms coincide algebraically. Only the short and partial-investment scenarios discriminate — the coverage FR-4 added. |
| M3 | Sign-flip cost split allocating 100% to the closing leg instead of proportionally | Layer 1 (1/7), Layer 2 | L1: `sign_flip_costs_charged_on_both_legs`, on "closing leg's share of the flip cost". L2: `trades` row 0 `pnl`, expected 0.0020517153685333678 got 0.0016377799059327093 (diff 4.14e-04). **Layer 3 stayed fully green (10/10).** | **Independent**, despite surfacing in `pnl`. The catch comes from the proportional-split contract clause, not from the identity — and Layer 3's green proves the identity cannot see it, since every allocation exhausting the cost preserves the sum. This is the row that shows "caught via `pnl`" and "caught by the identity" are different things. |
| M4 | Trade `pnl` accrued against a frozen NAV (the initial 1.0) instead of the running NAV — the error class the superseded segment-sum P&L clause exemplifies | Layer 1 (7/7), Layer 2, Layer 3 (1 property) | L1: `pnl` in every scenario; equity assertions all stayed green, since the frozen NAV feeds only the trade accumulator. L2: `trades` row 0 `pnl`, expected 0.0020517153685333678 got 0.0020649399726369246 (diff 1.32e-05). L3: `trade_pnl_sum_plus_one_equals_final_equity` — **the identity itself**. | **Partly spec-forced.** This is the caveat's own case. The mutation changes `pnl` semantics, and the identity is what detects it: Layer 3's identity property fires directly, and the reference's agreement on the correct form was itself reached via that identity (see the attestation's follow-up). Layer 1 carries this row independently — its scenarios are hand-derived — but the Layer 2 catch alone would not establish independent derivation of `pnl` semantics. |

**What the table establishes, stated at its actual strength.** M1 and M2 are the two
behaviours a narrower specification source set would have reproduced, and both are
caught by Layer 1 and Layer 2 independently — the FR-3 claim holds for the cost
notional and the equity path without qualification. M3 extends that to cost
attribution and is the sharpest row: it is invisible to the identity, so its catch
is pure contract derivation. M4 is deliberately the weak case and is recorded as
such rather than counted as a fourth independent confirmation.

**A mutation nothing caught: none.** All four were caught by at least two layers.

**Coverage note.** Layer 1 caught every mutation, but not with every scenario, and
the pattern is the argument for FR-4's additions: M2 is invisible to all three
originally-shipped fully-invested scenarios and visible only to the short and
partial-investment ones; M3 is visible to exactly one scenario, the sign-flip case
added for it.

---

## 7. Related — the third-party oracle layer, and what its agreement claims

Scope note: sections 1–6 above concern the clean-room `backtest_reference` only.
The two third-party oracles are a separate evidence layer with its own record,
kept alongside those artefacts rather than here: the module docstring of
`oracle_scenarios.py` carries their production-conditions record — which
translation decisions were derived from the documented model difference before
any comparison ran, which were established empirically, and which were reached
from an observed deviation — together with the coverage boundary; and
`KNOWN_DISCREPANCIES.md` indexes the measured convention differences, each with
its own self-contained investigation document under `discrepancies/`. This
section exists so a reader auditing independence claims in this repository finds
the second layer from the first.

Note what that record is *about*. The oracles' own independence needs no
attestation — vectorbt and `nautilus_trader` are third-party code. What was
authored here, by someone who could see the engine's outputs, is the
**translation**, and a translation tuned until agreement appeared would make the
whole layer agree for the wrong reason. That is the claim the oracle record
addresses, and it is weaker than sections 1–6's: the clean-room reference's
author could not reach the implementation at all, whereas the translation's
author could and is relied upon to have recorded when a decision came from a
deviation rather than from the model difference.

Its agreement claims are not uniform either, and for a reason distinct from the
`pnl` caveat above. Two different things are done to make an order-driven
backtester comparable to this engine:

- **Model translation — a claim about equivalence, and testable.** Each oracle
  re-issues the held target weight as an order at every bar, because the engine
  models a continuously rebalanced constant-weight book while an order-driven
  system holds constant shares. The oracle still computes its own share counts,
  cash, fees and value path; the translation selects which book it simulates. If
  it were wrong the fixtures would disagree, so the agreement is evidence.

- **Scenario arrangement — not a claim about equivalence.** Two further
  differences — the engine funding targets out of post-cost NAV where an
  order-driven system sizes against pre-cost value, and the engine measuring
  turnover against the nominal held weight where an order-driven system uses the
  drifted actual weight — cannot be bridged by any configuration. The scenarios
  instead hold prices flat around every rebalance bar so neither difference has
  anything to act on. Agreement here is not evidence that the two systems model
  cost basis the same way; it is evidence that the scenarios never ask.

The consequence, stated so it is not read as broader than it is: the oracle
layer confirms execution timing, sign handling, short mark-to-market, the total
turnover a sign flip is charged on, multi-instrument aggregation, and the cost
notional's dimension and magnitude.

It does not confirm two things, both named limitations of the layer rather than
omissions. The first is the cost basis under weight drift, which no scenario
measures. The second is the **split** of a sign flip's cost between the closing
and opening legs: the oracle fixtures carry equity paths and nothing else, and an
equity path cannot observe an allocation — every split that exhausts the total
produces a bit-identical curve. §6's mutation M3 is the direct evidence, since a
100%-to-the-closing-leg split leaves the whole equity-based layer green. The
split is pinned instead by this reference's trade log and by
`sign_flip_costs_charged_on_both_legs`, which recovers each leg's share from the
engine's own `pnl` and asserts the two shares and their sum separately.

One item on that list is worth a pointer, because it is the one an oracle can
most easily get right for the wrong reason. "Execution timing" is not assumed
from either oracle's documentation: `nautilus_oracle.py` establishes it by probe
— five bars at distinct prices and one order submitted from a known bar, so the
fill price alone identifies the filling bar — and quotes the recorded output as
the grounds for its alignment. The probe ships runnable (`--probe`) rather than
merely described, and the fact it establishes is re-asserted on every fill in
every scenario, so a future version of that library which queued orders to the
next bar would redden rather than shift the fixtures by a bar.
