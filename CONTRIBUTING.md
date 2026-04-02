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
- `Cairos.Align.aligned` uses `private` to prevent construction outside
  `Cairos.Align`. This is load-bearing — never remove it.
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

`result` appears in exactly one place in the core library: `Cairos.Align.align`.
Chain with `let*`. Do not unwrap with `Result.get_ok` outside tests.

Never `raise`. Never `failwith`. Never `assert false` as an error path. If you
find yourself wanting to raise, the design is wrong.

### VI. .mli Files

Add `.mli` files only when they add genuine value:

- The module is public API consumed by third parties outside Cairos
- Implementation details must be hidden from other modules within Cairos
  (e.g. the `private` constraint on `Align.aligned`)

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

### IX. Quality Gate

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
