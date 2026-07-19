# Cairos benchmarks

Bechamel-powered measurement suite. Manual, opt-in — never part of the
default `just` gate.

## Recording the baseline

Under the standard recording conditions — clean `_build/`, no competing load,
single run:

```bash
opam exec -- dune clean
just bench-record
```

`bench/baseline.json` is rewritten via `Bench_emit.write_consolidated`
(sorted cells, `%.6g` floats) — diff-friendly when rebaselining.

## Three recipes

- `just bench` — runs every bench in Notty mode. No baseline interaction.
- `just bench-record` — runs every bench in JSON mode and rewrites
  `bench/baseline.json`.
- `just bench-compare` — diffs current run against baseline. Exits `0`
  on no regression, `1` on >20% wall-clock regression or
  missing-in-current, `2` on tooling-level failure.

## JSON schema

Schema-versioned at `cairos-bench-baseline-v1`. See `bench/bench_emit.mli`
for the full contract.
