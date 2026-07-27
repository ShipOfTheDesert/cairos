# Contributing to Cairos

## Development Setup

```bash
opam install . --deps-only --with-test
dune build
dune test
```

## Running Tests

### All at once

```bash
just                  # build + unit tests + fmt + lint (run before every commit)
```

### Unit tests (Alcotest)

```bash
just test             # run all unit tests via dune
```

Unit tests live under `test/unit/` organised per package:

```
test/unit/cairos/          # Index, Series, Align, Window, Resample, Frame, Nonempty
test/unit/cairos_io/       # CSV loading: of_csv, frame_of_csv, error surface
test/unit/cairos_finance/  # returns, drawdown, vol, sharpe — verified against known values
test/unit/cairos_plot/     # SVG output, chart rendering
```

### Property tests (QCheck)

Property tests live in dedicated `test_<module>_props.ml` files alongside
the Alcotest suites under `test/unit/cairos/` and `test/unit/cairos_finance/`.
Shared generators (e.g. `daily_float_series_arb`, `paired_overlapping_daily_arb`)
live in `test/unit/support/qcheck_gen.{ml,mli}` and are consumed via the
`qcheck_gen` library; per-test arbitraries that compose those generators
(e.g. `series_with_shift_arb`) stay in the property file that uses them.
Register a property in the Alcotest suite with `QCheck_alcotest.to_alcotest
my_property` in the `tests` list. Runtime deps are `qcheck-core` and
`qcheck-alcotest`, declared `:with-test` on `cairos` and `cairos_finance`.

Property suites pin the QCheck seed via `Qcheck_gen.pin_seed_from_env ()`
at the top of `let () = Alcotest.run ...` so CI is deterministic; the
default seed is `0xC41A05`. To reproduce a given run locally, override
the seed via the environment: `QCHECK_SEED=12345 just test`.

### Benchmarks (Bechamel)

Benchmarks live under `bench/` — a top-level directory parallel to `lib/` and
`test/`, **not** under `test/`. Each benchmark is a private `(executable ...)`
stanza (never `(test ...)`) that links against `bechamel`, `bechamel-notty`,
and `notty.unix`. The `bench/dune` stanza intentionally omits `(package cairos)`:
dune rejects `(package ...)` on an executable without `(public_name ...)`, and
adding `(public_name ...)` would leak the benchmark binary into
`opam install cairos` and violate the `cairos` package's runtime-closure
minimalism. `bechamel` / `bechamel-notty` are declared `:with-test` on `cairos`
at the `dune-project` level, so the switch has them whenever
`opam install --deps-only --with-test .` runs.

Three opt-in justfile recipes drive the bench suite. None run as part of
`just`, `dune runtest`, or any CI job:

- `just bench` — runs every executable under `bench/` in Bechamel's Notty mode
  and prints OLS tables to the terminal. Human-readable, no comparison, no
  exit-code gate. The right recipe while iterating on a single bench.
- `just bench-compare` — runs every executable in JSON mode, diffs against
  `bench/baseline.json`, and exits non-zero on any monotonic-clock cell over
  the 20% regression threshold or any baseline cell missing from the current
  run. The regression gate.
- `just bench-record` — runs every executable in JSON mode and rewrites
  `bench/baseline.json` with the current numbers. Refresh after an
  intentionally perf-affecting change. Follow `bench/README.md`'s recording
  procedure (clean build, no other heavy load on the workstation).

Each bench `.ml` selects between the two output shapes by reading the
`CAIROS_BENCH_OUTPUT` environment variable: the `bench-compare` and
`bench-record` recipes set `CAIROS_BENCH_OUTPUT=json` before invoking each
executable; `bench` leaves it unset. The contract is exact-string match on
`"json"` — any other value (or unset) falls back to Notty rendering. To
manually invoke a single bench in JSON mode:

```bash
CAIROS_BENCH_OUTPUT=json opam exec -- dune exec bench/bench_series_map.exe
```

See `bench/README.md` for the JSON schema (versioned at
`cairos-bench-baseline-v1`) and the baseline-recording procedure.

Benchmarks are slow (statistical sampling) and produce machine-readable
JSON or human-readable Notty tables on stdout — never pass/fail output for
a unit-test runner. The regression gate (`bench-compare`) is the exception:
exit `0` means no regression, exit `1` means regression or missing cell,
exit `2` means tooling-level failure.

Error-handling policy for `bench/*.ml` mirrors the notebook exemption (see
Coding Principles §IX): setup failures `failwith` with a context label rather
than propagating `result`. The `result`-everywhere rule applies to the core
library, not to executable harnesses. The `bench/bench_emit.ml` helper
library is library-shaped code, not a harness, and follows the
`result`-everywhere rule in full — with one exception, stated rather than
implied: its four fallible signatures still return `(_, string) result` rather
than a closed `err` variant. §V's structured-error rule and the
`lint-string-errors` gate that enforces it are both scoped to `lib/`;
converting `bench_emit` is deliberate future scope, tracked in the planning
doc's Open Items.

When writing a new benchmark, copy `bench/bench_series_map.ml` as the
reference template. One load-bearing detail: **input construction
(`make_input ()`) must be hoisted out of the `Staged.stage` thunk**, so the
staged thunk contains only the operation under measurement. Placing setup
inside the staged thunk causes every measured iteration to rebuild the input,
polluting `time/run` and the allocation columns with setup cost.

### Cross-validation harnesses

Each package's Layer 2 comparison lives in an `(executable)`, never a `(test)`,
so `dune runtest` never depends on a fixture-generating toolchain. They run from
`just validate-check` (via `just validate`, which is part of `just`) and are:

```
test/unit/cairos_finance/cross_validate.ml    metrics vs Pandas
test/unit/cairos/cross_validate_frame.ml      Frame rank / zscore vs Pandas
test/unit/cairos/cross_validate_resample.ml   daily -> monthly vs Pandas
test/unit/cairos_engine/cross_validate.ml     engine vs the clean-room reference
test/unit/cairos_engine/cross_validate_oracles.ml   engine vs two third-party oracles
```

The last one reads **committed** fixtures rather than regenerated ones, so it
needs no Python and runs in the gate like the rest; only the scripts that write
those fixtures are opt-in. It always prints all three systems' equity paths bar
by bar after its verdict — Markdown, with the columns
`validation/discrepancies/TEMPLATE.md` expects, so a scenario that ever
disagrees is written up by pasting rather than retyping. The tables are not
behind a flag on purpose: the point of maintaining three implementations is
being able to *see* that they agree, and a line asserting agreement is the kind
of claim this whole layer exists to distrust.

`default` therefore runs `validate` **last**, after `notebooks`, so those tables
are the final thing on screen rather than buried under several hundred lines of
Jupyter chatter. While iterating, `just validate` on its own is the fast path —
it skips the notebook run entirely.

Shared conventions, and the reason each exists:

- **Exit codes.** `0` agreement, `1` mismatch, `2` tooling failure. The split is
  load-bearing, not cosmetic: `1` is read as a finding against the system under
  test, so a missing or malformed fixture must never take that path. A blank
  cell, a `nan` literal or an unreadable file is `2`.
- **Error handling.** These are harnesses, so the `result`-everywhere rule of
  Coding Principles §V does not apply — same exemption as `bench/*.ml` and
  `notebooks/*.ml` (§IX). They report via the exit codes above and terminate.
  Anything library-shaped that they call does follow §V in full.
- **Tolerance.** Absolute `1e-10`, with a NaN-aware comparator that branches on
  both operands *before* subtracting: `Float.abs (nan -. x) <= tol` is `false`
  for every `x` including `nan`, so a naive comparator reports both-NaN as a
  mismatch and a regression-to-NaN as an ordinary one.
- **Fixture headers** are asserted before parsing, so a format change fails by
  name rather than as an arithmetic disagreement.
- **Shared helpers** live in `test/unit/support/validate_support.{ml,mli}` —
  the exit-code helpers, the line reader, the comparator, and the two fixture
  directory constants — pinned by `test_validate_support.ml`. Consume them
  rather than re-copying; the copies had already drifted before the library
  existed. `test/unit/cairos_engine/cross_validate.ml` is the one holdout, and
  its head comment records why: feature 0063 makes that file's being unmodified
  the evidence for its own green run.

### Third-party oracles (regeneration is opt-in; the comparison is not)

`just validate-oracle` runs two third-party backtesters over three shared
scenarios and rewrites the committed fixtures the gate compares against:

```bash
just validate-oracle    # regenerate oracle fixtures, then compare all three systems
```

Read the split carefully, because it is the whole design: **comparing** against
the oracles happens on every change, as the last line of `just validate-check`;
**running** them does not. The comparison binary reads committed CSVs and needs
neither dependency, so keeping it out of the gate bought nothing and cost the
one thing the fixtures exist for — an engine change drifting from two
third-party references with nobody noticing until someone ran the opt-in recipe.
The two scripts below are what stay opt-in, and only they are unreachable from
`just`, `just validate`, or any CI job. The two
dependencies are the heaviest this repository takes on — `vectorbt` resolves
~59 packages including numba and LLVM, `nautilus_trader` 14 — and unrelated
upstream breakage in either must not be able to redden a per-PR gate for a
one-shot cross-check. Each is pinned in its own PEP 723 `# /// script` header
rather than in `validation/pyproject.toml`, which `uv run` ignores for scripts
carrying inline metadata; the per-script isolation is also what lets the two
oracles and `reference.py` hold mutually incompatible dependency sets. Neither
is a library dependency: nothing under `lib/` or in any `.opam` file refers to
them. This paragraph is the §VII record for both.

Their outputs are **committed** under `validation/oracle_fixtures/`, unlike
every other fixture family, which is gitignored and regenerated by
`just validate-generate`. That divergence is deliberate and rests on a class
distinction: cheap Pandas-generated fixtures inside the gate can be regenerated
on demand, whereas these are outputs of heavyweight runtimes kept outside it,
and committing them is what lets the comparison binary run — and act as a
regression anchor — with neither dependency installed. They live in a separate
directory rather than under `validation/fixtures/` because that whole directory
is gitignored and un-ignoring a subtree needs a `dir/*` + `!dir/sub/` rewrite;
the separate path also makes committed-versus-generated visible from the path
alone.

A disagreement where two of the three systems agree against the third is a
finding on the odd system out and is investigated. A three-way split, or a case
where pairwise agreement is not transitive at the tolerance, identifies no
culprit; those are recorded in `validation/KNOWN_DISCREPANCIES.md`, each with a
self-contained investigation document under `validation/discrepancies/` written
to be pasted whole into a session holding no prior context. Start from
`validation/discrepancies/TEMPLATE.md`.

## Coding Principles

Listed in priority order. All are enforced — none are guidelines.

### I. Library-First / Package Boundaries

Cairos is three opam packages. Each has its own `dune` file, its own
`(library ...)` stanza, and its own opam package definition. The build system
enforces boundaries — not convention.

**Package dependency order (strictly downward — no exceptions):**

```
cairos           ← ptime, nx — the core, no other cairos deps
cairos_finance   ← cairos
cairos_plot      ← cairos, nopal_scene, nopal_draw, nopal_charts
```

A package cannot import from another unless it is explicitly declared as a
dependency in its `dune` file. If the build succeeds with an undeclared import,
that is a dune misconfiguration — fix it, do not work around it.

No circular dependencies. The dependency graph must be a DAG. If you find
yourself wanting a circular import, the abstraction boundary is wrong — flag
it as an architectural question before touching any code.

### II. Spec-Driven Development

No implementation begins without an approved written specification. Every
feature carries a requirements document — problem statement, functional and
non-functional requirements, explicit scope boundaries — and an implementation
plan — module breakdown, key types and interfaces, implementation sequence,
test plan. Both are agreed before any `.ml` file is touched, and the
requirements half is frozen once approved.

Planning artefacts are maintained outside this repository; what lands here is
the code, the tests the specification called for, and the ADRs recording
decisions that outlive a single feature. Commit messages and PR descriptions
are where implementation rationale belongs — not inline comments citing
documents a reader of this repository cannot open.

Architectural questions that arise mid-implementation are escalated and
resolved in the specification, not settled unilaterally at the keyboard. If
something is not covered by the approved plan, stop and raise it.

### III. Test-First

All implementation follows strict TDD:

1. Write the test defining the intended behaviour
2. Confirm it fails (`dune test` output required as evidence)
3. Write the minimum implementation to make it pass
4. Refactor under green

No `.ml` implementation file is merged without a corresponding test that was
written first and initially failed.

Known-good reference values for financial function tests must be computed
independently — by hand or against a trusted Python/Pandas reference — and
encoded as regression tests. Do not derive expected values from the
implementation being tested.

**Oracle independence.** A reference implementation is only an oracle if it
is derived from the specification, not the code. Any cross-validation
reference (engine, WFO folds, future metrics without a library primitive)
is authored from the specification alone — the implementation must not be in
the author's context. Record provenance alongside the reference: state the
contract clauses it encodes, in full, so the oracle is self-contained and
auditable without reaching for an external document. A reference written by
reading the implementation proves only that the code equals its own port; it
cannot catch modeling errors, and this failure mode has shipped a real defect
in this project.

Enforcement is structural, not prompted. The clean-room author must run in an
environment where the excluded artefacts are unreachable, not one where it is
merely instructed not to open them. This requires the generation prompt to be
fully self-contained, including any output contract the replacement must
satisfy. The attestation records which mechanism enforced the exclusion.

Process artefacts carry implementation context. Reflections, task notes, and
decision logs that state how the implementation behaves are implementation
context for clean-room purposes. Any workflow file the clean-room step loads by
construction must not contain them — keep them in a sibling document that step
does not open.

The task decomposition that makes this enforceable is
`docs/adrs/0064-clean-room-task-decomposition.md`.

**A note on the ADR and feature-doc paths cited in this file.** `docs/` is not
committed, so those citations are breadcrumbs for the maintainer, not links a
cloner can follow — which is why every clause they point at is also stated
substantively here. Files under `validation/` are held to the stricter rule:
they may not cite uncommitted paths at all, because they are read by an
auditor or a fresh
investigation session with nothing but this repository, and a dangling reference
there is a dead end rather than a footnote.

### IV. Make Invalid States Unrepresentable

The type system is the primary correctness mechanism. Prefer designs where
wrong usage is a compile error over designs where it is a runtime error.

Specific invariants that are structurally enforced:

- Frequency mismatches between series are compile errors via phantom types.
  They are never runtime checks.
- Misaligned binary operations are impossible by construction. `map2` and all
  binary operations require a `Cairos.Align.aligned` value — the only way to
  produce one is through `Cairos.Align.align`.
- `Cairos.Align.aligned` is an **abstract type** in the public signature —
  external callers cannot construct it, destructure it, or project its
  fields. This is load-bearing — never remove it. No automated regression
  test guards this invariant; the OCaml compiler does (any external
  destructuring fails to compile). Reviewers: any PR that modifies
  `lib/align.mli`'s `aligned` declaration must preserve full abstractness
  — reject widenings to `private { ... }`, to a concrete record, or to any
  manifest RHS. If in doubt, attempt to compile `let _probe (a : (_, _, _)
  Cairos.Align.aligned) = a.left` as a throwaway — it must fail with
  "this expression has an abstract type".
- GADTs are preferred over variants + runtime checks when the invariant is
  statically knowable. `Freq.t` is the canonical example.

New type parameters or structural constraints must be raised as an
architectural question before being added.

### V. No Exceptions, Minimal Result

Two error categories, strictly separated:

**Programmer errors** are prevented structurally — wrong frequency, misaligned
series, invalid construction. These are compile errors or impossible by
construction. They never produce a `result` or an exception.

**Runtime conditions** — empty index after alignment, unparseable date string —
return `result`. These are genuine environmental conditions the caller must
handle.

`result` appears only at genuine runtime boundaries. The sanctioned sites are:

- `Cairos.Index` smart constructors (`daily`, `minute`, `hourly`, `weekly`,
  `monthly`, `of_unix_floats`) — parse failures and monotonicity violations.
- `Cairos.Series.make` — index/values length mismatch and 0-dimensional
  values.
- `Cairos.Align.align` — may produce an empty index.
- `Cairos.Resample.resample` — target frequency must be lower than source.
- `Cairos.Frame.of_series` — duplicate column names and index mismatch.
- `Cairos_engine.Backtest.run` — entrypoint validation of caller-supplied
  frames and rebalance calendar.
- The entire `cairos_io` public surface — CSV parsing is a runtime boundary.

Every one of these returns a **structured variant** on its error side, never a
`string`: one variant per failure mode, payloads carrying the offending values,
an aggregate whose non-emptiness is invariant typed `Cairos.Nonempty.t` rather
than `list`, and a sibling `err_to_string` holding all the prose. Message text
is not part of the contract — assert on variants, never on substrings. See
`docs/adrs/0061-structured-error-types-at-library-boundaries.md`. A `string` on
the error side of a public `.mli` is rejected mechanically rather than by
convention — see the structured-error gate under §X.

Chain with `let*`. Do not unwrap with `Result.get_ok` outside tests.

Never `raise`. Never `failwith`. Never `assert false` as an error path. If you
find yourself wanting to raise, the design is wrong.

In `lib/cairos_engine/` this is enforced mechanically rather than by
convention — see the engine-assert gate under §X.

A `result` whose failure the caller can prove impossible is still handled, never
unwrapped. The variant is constructed at its site, rendered by the module's
`err_to_string`, documented as unreachable with the argument for why, and tested
by direct construction once the type is public. Public constructors that no
caller will match are the expected consequence, not an oversight. The worked
instances are `Resample.err`'s two `Ptime` failures and `Cairos_io.err`'s
`Series_error` / `Frame_error`; see
`docs/features/0066-structured-error-migration.md` Decision 4.

- For functions whose precondition is "input list must be non-empty", use
  `Cairos.Nonempty.t` at the function signature rather than a runtime `result`
  rejection. Empty-list is a programmer error; lift the check to the type
  system. `Frame.of_series` is the canonical example.

### VI. .mli Files

Add `.mli` files only when they add genuine value:

- The module is public API consumed by third parties outside Cairos
- Implementation details must be hidden from other modules within Cairos
  (e.g. the abstract `Align.aligned` type)

Internal modules used only within Cairos development do not need `.mli` files
by default. Do not add them as a matter of habit.

### VII. Dependencies

Dependencies are welcome if they are worth their weight. When a dependency
would meaningfully reduce implementation effort or improve correctness, flag
it and discuss — do not assume it is forbidden and build from scratch instead.
Do not add a dependency silently — always surface the tradeoff first.

A developer-only dependency that no gate and no CI job installs is held to the
same rule, not a relaxed one: the tradeoff is surfaced and the record is written
down. `vectorbt` and `nautilus_trader` are the current instances — see Running
Tests, "Third-party oracles".

### VIII. Documentation

Public functions — those consumed by third parties — need doc comments covering
what the function does, what the `result` error cases are if applicable, and a
brief example for non-obvious functions.

Internal functions do not need doc comments by default. Prefer clear naming
and narrow scope over compensatory documentation.

### IX. Notebook Code Is Not Library Code

Jupyter notebooks (`notebooks/*.ml`) are sequential demo scripts, not library
modules. They follow different error-handling rules than the core library:

- **Crash early with a clear message.** Unwrap `result` values immediately
  (e.g., `Result.get_ok` or `match ... | Error e -> failwith e`). Do not
  propagate `Option` or `Result` through subsequent cells.
- **No defensive wrapping.** Downstream cells should receive plain values, not
  `option`s. If a cell fails, the notebook stops — that is the correct
  behaviour.
- **Solutions library exemptions.** `result-let-binding`, `avoid-result-get-ok`,
  and `exhaustive-variant-matching` do not apply to notebook code. These
  conventions exist to protect library consumers; notebooks have no consumers.

The rationale: notebooks are sequential. If cell 3 fails, there is no point
running cell 7 with `None`. A crash with a clear error is better UX than
`match x with Some ... | None -> print "unavailable"` boilerplate on every
downstream binding.

When a notebook cell uses `Printf.printf`, end the format string with `%!`
(or otherwise flush `stdout`): `ocaml-jupyter` does not flush between cells,
and output without `%!` can be lost or appear in the wrong cell. This was
diagnosed during the PoC Task 1 walkthrough.

### X. Quality Gate

Every commit must pass:

```bash
just    # build + test + fmt + lint
```

No PR is opened without this passing locally first.

#### The engine-assert gate

`just lint-asserts` — folded into `just lint`, so `just` runs it — fails if any
file under `lib/cairos_engine/` contains an `assert` token outside a comment or
a string literal. §V forbids `assert false` as an error path everywhere; the
engine is the one place where that rule has a machine checking it, because two
code reviews found the unreachability comments justifying nine such sites had
gone stale while the sites themselves stayed put.

```bash
just lint-asserts                          # self-test, then scan the engine
scripts/lint-asserts.sh path/to/file.ml    # scan arbitrary files
```

A bare `grep` is not sufficient in either direction: it fires on the comment
that merely names the token — the `parse_rows` doc comment in
`lib/cairos_io/cairos_io.ml` does exactly that today — and it is blind to a
token hidden inside a string. So
`scripts/lint-asserts.awk` lexes just enough OCaml to remove non-code text
first: nested `(* (* *) *)` comments, string literals (including ocamlformat's
backslash continuations), string literals *inside* comments, `{|quoted|}` and
`{tag|quoted|tag}` literals, and char literals — `'"'` would otherwise open a
string that swallows the rest of the file.

**Exit codes**, following the `bench-compare` convention:

| Code | Meaning |
|------|---------|
| `0` | clean |
| `1` | at least one `assert` token in engine code |
| `2` | tooling failure — bad arguments, or the lexer desynchronised |

Exit 2 is not a nuisance code, it is the one that keeps the gate honest. If the
lexer runs off the end of a file it has stopped tracking (an unterminated
comment or string), everything after that point is stripped as comment text and
a dirty tree reports clean. The awk script checks its own end-of-file state and
exits 2 rather than reporting a green it cannot justify.

**Self-test.** `just lint-asserts` runs three fixtures under
`test/lint/fixtures/` through the real script before it scans the real tree, and
aborts if any of them behaves differently from its contract:

| Fixture | Must exit |
|---------|-----------|
| `dirty_engine.ml.fixture` | `1` — real `assert false` in code |
| `comment_engine.ml.fixture` | `0` — the token only in comments, strings and identifiers |
| `truncated_engine.ml.fixture` | `2` — unterminated comment, lexer out of sync |

The fixtures are `.fixture` rather than `.ml` to keep them out of dune's sight.
They exercise the same `scripts/lint-asserts.sh` entrypoint as the real scan —
a self-test that runs different code from the gate proves nothing about the
gate.

**Portability.** The awk is POSIX-only (no `gensub`, no regex-dialect
dependencies). `ubuntu-latest` ships mawk as `/usr/bin/awk`, so mawk, not gawk,
is what runs this in CI; identical exit codes are verified under gawk 5.4.0,
`gawk --posix`, `gawk --traditional`, mawk 1.3.4 and busybox awk.

**In CI** the gate runs as the `engine-assert-gate` job in both
`.github/workflows/pr.yaml` and `main.yaml`. It is in both — rather than in
`lint.yaml` — because `lint.yaml` triggers only on `pull_request`, so a direct
push to `main` would go ungated. The job invokes `just lint-asserts` rather than
re-listing its steps, so the self-test cannot drift out of CI. It installs only
`just`: the gate is a text scan and needs no opam switch, so a violation reports
in seconds instead of behind a full solve.

Scope is `lib/cairos_engine/*.ml` and nothing else. `lib/cairos_io/cairos_io.ml`
(a comment) and `test/unit/cairos_engine/cross_validate_oracles.ml` (real code)
carry the token today and are deliberately unguarded.

#### The structured-error gate

`just lint-string-errors` — folded into `just lint`, so `just` runs it — fails
if any public `.mli` under `lib/` puts `string` on the error side of a `result`.
§V requires every fallible function *in `lib/`* to return a closed `err`
variant with a sibling `err_to_string`; this is the machine that keeps it
true. (`bench/bench_emit.mli` is outside both the rule and the gate — see
§Benchmarks.) Convention
alone was not enough: two exceptions were recorded and left standing while eight
signatures still returned prose, and the next feature that adds a fallible
function re-breaks the property for free.

```bash
just lint-string-errors                          # self-test, then scan lib/
scripts/lint-string-errors.sh path/to/file.mli   # scan arbitrary files
```

A bare `grep` is not sufficient in **three** directions here. The first two are
the engine-assert gate's: it fires on the doc comment that merely explains why
a surface stopped returning `(_, string) result`, and it is blind to the shape
hidden inside a string literal. So `scripts/lint-string-errors.awk` carries the
same OCaml lexer and strips non-code text first.

The third direction is specific to this gate. Every `.mli` it scans is
ocamlformat output, and ocamlformat splits a `result` whose success side is wide
across three lines:

```ocaml
val frame_of_csv_with :
  path:string ->
  ( ('freq, (float, Bigarray.float64_elt) Nx.t, [ `Column_major ]) t,
    string )
  result
```

No line there contains `string) result`, so a line-at-a-time scan reports the
file clean — a false negative that arrives silently, the day a signature grows.
The gate therefore matches over the whole stripped file rather than line by
line, and reports against the line the offending `string` sits on.

**Exit codes**, the same three-valued contract as `lint-asserts`:

| Code | Meaning |
|------|---------|
| `0` | clean |
| `1` | at least one `string` error side in a public signature |
| `2` | tooling failure — bad arguments, or the lexer desynchronised |

**Self-test.** `just lint-string-errors` runs five fixtures under
`test/lint/fixtures/` through the real script before it scans the real tree, and
aborts if any of them behaves differently from its contract:

| Fixture | Must exit |
|---------|-----------|
| `string_error_violation.mli.fixture` | `1` — a `(_, string) result` on one line |
| `string_error_wrapped.mli.fixture` | `1` — the same violation, split by ocamlformat across three lines |
| `string_error_qualified.mli.fixture` | `1` — the same violation spelled `Stdlib.result` and `Result.t`, flat and wrapped |
| `string_error_clean.mli.fixture` | `0` — the shape only in comments, strings, and near-miss types (`(string, err) result`, `('freq, string) Series.t`, `result_summary`, `(int, err_string) result`) |
| `string_error_truncated.mli.fixture` | `2` — unterminated comment, lexer out of sync |

The fixtures are `.fixture` rather than `.mli` for a stronger reason than the
assert gate's: `dune fmt` reflows every `.ml` and `.mli` dune can see, anywhere
in the tree, and the wrapped fixture is pinned to an exact line break.

**Portability.** POSIX awk only. Verified to give identical exit codes and
byte-identical output on all five fixtures and the real tree under gawk 5.4.0,
`gawk --posix`, `gawk --traditional`, mawk 1.3.4 and busybox awk — mawk being
what `ubuntu-latest` actually runs.

**In CI** the gate runs as the `string-error-gate` job in both
`.github/workflows/pr.yaml` and `main.yaml`, for the same reasons the
`engine-assert-gate` job is in both, and invoking the `just` recipe directly so
the self-test cannot drift out of CI.

Scope is the public `.mli` files under `lib/` — `lib/*.mli` and `lib/*/*.mli`,
thirteen files today. Implementation files are not scanned: the criterion is
about the surface a caller sees, and a `string` error inside an `.ml` is
invisible once the `.mli` constrains it. The gate recognises `result`,
`Stdlib.result` and `Result.t`. The qualified spellings are not hypothetical:
`lib/cairos_engine/cairos_engine.mli` declares `type 'freq result = private
{...}`, which shadows `Stdlib.result` for the rest of that signature, so
`Backtest.run` *cannot* spell its return type bare. A gate matching only the
bare form would be permanently blind to the engine.

Two things it cannot see, stated rather than left inferred:

- **A type alias.** `type err = string` followed by `val f : ... -> (int, err)
  result` launders a prose error side past any text-level scan, because the
  offending token is not in the signature. Catching it needs type information,
  which means `ocaml-lsp`, not awk.
- **`bench/bench_emit.mli`.** The paragraph in §Benchmarks calling `bench_emit`
  library-shaped code predates this gate, and its four fallible signatures still
  return `(_, string) result`. §V's rule is scoped to `lib/`, and so is the
  gate; converting `bench_emit` is a deliberate future change, tracked in the
  planning doc's Open Items, not an omission the gate is hiding.

## Commit Style

Conventional Commits: `type(scope): description`

Types: `feat`, `fix`, `docs`, `test`, `refactor`, `chore`, `perf`

Scopes match package names or internal modules: `core`, `finance`, `plot`,
`index`, `series`, `align`, `window`, `resample`, `frame`

Examples:
- `feat(series): add shift operation with positive and negative lag`
- `feat(align): implement asof forward and backward strategies`
- `test(finance): add drawdown regression against pandas reference values`
- `fix(window): correct nan handling at series head for rolling mean`
- `refactor(index): extract Ptime parsing into Index.Parse submodule`
