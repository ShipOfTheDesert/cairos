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
test/unit/cairos/          # Index, Series, Align, Window, Resample, Frame
test/unit/cairos_finance/  # returns, drawdown, vol, sharpe — verified against known values
test/unit/cairos_plot/     # SVG output, chart rendering
```

### Property tests (QCheck)

Property tests live alongside Alcotest tests in the same `test_series.ml`-style
files. Generators (e.g. `daily_float_series_arb`) are defined inline in the
test file that consumes them — promote to a shared helper only when a second
consumer lands. Register a property in the Alcotest suite with
`QCheck_alcotest.to_alcotest my_property` in the `tests` list. Runtime deps
are `qcheck-core` and `qcheck-alcotest`, declared `:with-test` on `cairos`
only.

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

Run a benchmark with:

```bash
opam exec -- dune exec bench/bench_series_map.exe
```

Benchmarks are **not** run by `just`, `dune runtest`, or any CI job. They are
opt-in, slow (statistical sampling), and produce a Bechamel OLS table on
stdout rather than pass/fail output. Regression thresholds and baseline storage
are out of scope until a concrete need surfaces.

Error-handling policy for `bench/*.ml` mirrors the notebook exemption (see
Coding Principles §IX): setup failures `failwith` with a context label rather
than propagating `result`. The `result`-everywhere rule applies to the core
library, not to executable harnesses.

When writing a new benchmark, copy `bench/bench_series_map.ml` as the
reference template. One load-bearing detail: **input construction
(`make_input ()`) must be hoisted out of the `Staged.stage` thunk**, so the
staged thunk contains only the operation under measurement. Placing setup
inside the staged thunk causes every measured iteration to rebuild the input,
polluting `time/run` and the allocation columns with setup cost.

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

All planning happens outside CC, in Claude.ai. CC receives finished planning
documents and implements from them. The workflow is:

```
docs/00_project_overview.md  -- phases (Claude.ai)
docs/01_poc_planning.md      -- epics for current phase (Claude.ai)
docs/epics/<n>.md            -- features for one epic (CC)
docs/prds/<n>.md             -- one PRD per feature (CC)
docs/rfcs/<n>.md             -- one RFC per feature (CC)
docs/tasks/<n>.md            -- tasks derived from RFC (CC)
```

Per epic: generate epic doc → per feature: PRD → RFC → tasks → implement.

Architectural questions that arise during implementation are surfaced back to
Claude.ai. CC does not make architectural decisions unilaterally.

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

New type parameters or structural constraints must be flagged to Claude.ai
before being added.

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
  `of_unix_floats`) — parse failures and monotonicity violations.
- `Cairos.Series.make` — index/values length mismatch.
- `Cairos.Align.align` — may produce an empty index.
- `Cairos.Resample.resample` — target frequency must be lower than source.
- `Cairos.Frame.of_series` — duplicate column names and index mismatch.
- The entire `cairos_io` public surface — CSV parsing is a runtime boundary.

Chain with `let*`. Do not unwrap with `Result.get_ok` outside tests.

Never `raise`. Never `failwith`. Never `assert false` as an error path. If you
find yourself wanting to raise, the design is wrong.

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
