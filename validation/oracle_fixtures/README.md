# Oracle fixtures

Committed outputs of the third-party backtest oracles, plus the shared inputs
every system runs. Held here rather than under `validation/fixtures/` because
that directory is gitignored in full: these are committed on purpose, so that
the comparison still has something to check during the long stretches when
neither oracle's heavyweight dependency set is installed.

| file pattern | written by | contents |
|---|---|---|
| `oracle_manifest.csv` | `oracle_scenarios.py` | the scenario roster, one id per row |
| `oracle_<scenario>_prices.csv` | `oracle_scenarios.py` | price frame, one column per instrument |
| `oracle_<scenario>_signals.csv` | `oracle_scenarios.py` | signal frame: target weights on rebalance bars, zeros elsewhere |
| `oracle_<scenario>_params.csv` | `oracle_scenarios.py` | `commission`, `slippage`, and `rebalance_bars` as a `;`-separated bar list |
| `vectorbt_<scenario>_equity.csv` | `vectorbt_oracle.py` | per-bar portfolio value |
| `nautilus_<scenario>_equity.csv` | `nautilus_oracle.py` | per-bar portfolio value |

The manifest and the params files exist so that **no** input is restated in the
comparison binary. An earlier revision hard-coded the roster, the schedule and
the two cost parameters in OCaml, and a seeded mutation showed what that costs:
changing its private copy of `long_short_flip`'s schedule from `[1; 5]` to
`[1; 4]` made it report the engine as the odd system out — a confident,
well-formatted accusation with the fault entirely in the comparison. A binary
whose loudest output names a culprit cannot hold a private copy of an input.

Shape, uniform across every file: `%.17g` floats, NaN as an empty cell, and
full RFC 3339 timestamps with an explicit `Z` — the full form rather than the
date-only one so a comparison can *verify* row alignment between the two oracle
families instead of assuming it. `%.17g` is why `slippage` reads
`0.00050000000000000001`: the fixture carries the exact double the scenario
defines, not a rounded rendering of it.

Regenerate with `uv run validation/oracle_scenarios.py` followed by each oracle
script — or just `just validate-oracle`, which does both and then compares.
Output is deterministic; a diff after regeneration means an input, a
translation, or a pinned dependency version changed. `just validate-oracle`
reports such a diff rather than letting the comparison quietly run on the
regenerated numbers, because a fixture that has drifted still compares green
against itself.

**Consumer:** `test/unit/cairos_engine/cross_validate_oracles.ml`, the three-way
comparison binary. It reads both equity families and the shared input frames,
runs the Cairos engine in-process on the same inputs, and adjudicates the three
systems bar by bar. Nothing else reads these files; verify with a grep for
`oracle_fixtures` under `test/`.

The comparison needs neither oracle dependency, which is the point of committing
the outputs — it runs on its own during the long stretches when neither is
installed:

    opam exec -- dune exec test/unit/cairos_engine/cross_validate_oracles.exe

`just validate-oracle` is what regenerates these files first and then compares.

Measured by that binary, across all three scenarios: the engine, vectorbt and
Nautilus agree to a worst **per-bar** deviation of 4.441e-16, against a
tolerance of 1e-10, with timestamps asserted equal rather than row alignment
assumed. Per scenario the worst deviations are 1.110e-16
(`single_instrument_long`), 4.441e-16 (`long_short_flip`) and 2.220e-16
(`two_instruments_one_rebalance`).

The per-bar check is load-bearing, not a refinement of the final-NAV check: a
pre-trade equity read in the Nautilus oracle offsets every rebalance bar by that
bar's cost while leaving the final NAV untouched, and no guard inside that
oracle detects it.
