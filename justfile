default: pin build test fmt lint validate notebooks

pin:
    #!/usr/bin/env bash
    set -euo pipefail
    for row in $(jq -c '.[]' pins.json); do
        url=$(jq -r '.url' <<< "$row")
        commit=$(jq -r '.commit' <<< "$row")
        for pkg in $(jq -r '.packages[]' <<< "$row"); do
            opam pin add -n "$pkg" "${url}#${commit}"
        done
    done

deps: pin
    opam install --deps-only -y .

build:
    opam exec -- dune build

test:
    opam exec -- dune runtest

# Opt-in benchmark runner. Intentionally NOT part of the default gate or
# `dune runtest` (per RFC 0032): benchmarks are manual, time-sensitive, and
# their output is noise for CI. Three recipes share the same per-bench loop:
# `bench` renders Notty for human inspection; `bench-record` writes
# bench/baseline.json under FR-7 conditions; `bench-compare` diffs the
# current run against the committed baseline and exits non-zero on
# >20% wall-clock regression or any missing-in-current cell.
bench:
    #!/usr/bin/env bash
    set -euo pipefail
    shopt -s nullglob
    for ml in bench/bench_*.ml; do
        name=$(basename "$ml" .ml)
        case "$name" in bench_compare|bench_record|bench_emit) continue;; esac
        opam exec -- dune exec "bench/$name.exe"
    done

# Refresh bench/baseline.json. Run under FR-7 conditions: clean build, no
# competing load. Each bench writes its JSON output to its own tempfile in
# a tempdir; bench_record.exe reads the tempdir and rewrites
# bench/baseline.json via Bench_emit.write_consolidated for diff stability.
# The OCaml driver assembles the consolidated wrapper — no shell-side JSON
# string-concat — per ~/.claude/solutions/ocaml/yojson-over-manual-json.md.
bench-record:
    #!/usr/bin/env bash
    set -euo pipefail
    shopt -s nullglob
    tmpdir=$(mktemp -d)
    trap 'rm -rf "$tmpdir"' EXIT
    for ml in bench/bench_*.ml; do
        name=$(basename "$ml" .ml)
        case "$name" in bench_compare|bench_record|bench_emit) continue;; esac
        if ! CAIROS_BENCH_OUTPUT=json opam exec -- dune exec "bench/$name.exe" \
                > "$tmpdir/$name.json"; then
            echo "bench-record: $name failed" >&2
            exit 1
        fi
    done
    opam exec -- dune exec bench/bench_record.exe -- --bench-dir "$tmpdir"

# Run every bench in JSON mode, diff against bench/baseline.json. Exits 0
# on no regressions, 1 on >20% wall-clock regression or missing-in-current,
# 2 on tooling-level failure (malformed JSON, missing baseline).
bench-compare:
    #!/usr/bin/env bash
    set -euo pipefail
    shopt -s nullglob
    tmpdir=$(mktemp -d)
    trap 'rm -rf "$tmpdir"' EXIT
    for ml in bench/bench_*.ml; do
        name=$(basename "$ml" .ml)
        case "$name" in bench_compare|bench_record|bench_emit) continue;; esac
        if ! CAIROS_BENCH_OUTPUT=json opam exec -- dune exec "bench/$name.exe" \
                > "$tmpdir/$name.json"; then
            echo "bench-compare: $name failed" >&2
            exit 1
        fi
    done
    opam exec -- dune exec bench/bench_compare.exe -- \
        --baseline bench/baseline.json --bench-dir "$tmpdir"

fmt:
    opam exec -- dune fmt

lint-doc:
    opam exec -- dune build @doc --force

lint-fmt:
    opam exec -- dune build @fmt

lint-opam:
    opam exec -- opam-dune-lint

lint: lint-doc lint-fmt lint-opam

validate-generate:
    uv run validation/reference.py

validate-check:
    opam exec -- dune exec test/unit/cairos_finance/cross_validate.exe

validate:
    #!/usr/bin/env bash
    set -euo pipefail
    command -v uv >/dev/null || { echo "uv not installed, skipping validation"; exit 0; }
    just validate-generate
    just validate-check

# Soft-skips when jupytext/papermill are absent so `just` passes on machines
# without the Jupyter toolchain. CI must install both to enforce notebook execution.
#
# `dune install` is required before papermill runs: the ocaml-jupyter kernel
# resolves `#require "cairos_jupyter"` (and friends) through findlib in the
# opam switch prefix, not from the local _build tree. Without an install step,
# API changes in cairos_plot / cairos_jupyter show up as "Unbound value" errors
# in notebook cells even though `just build` passes.
notebooks:
    #!/usr/bin/env bash
    set -euo pipefail
    command -v jupytext >/dev/null || { echo "jupytext not installed, skipping notebooks"; exit 0; }
    command -v papermill >/dev/null || { echo "papermill not installed, skipping notebooks"; exit 0; }
    opam exec -- dune install cairos cairos_io cairos_finance cairos_plot cairos_jupyter
    mkdir -p _build/notebooks
    for nb in notebooks/[0-9]*.ml; do
        base=$(basename "$nb" .ml)
        tmp=$(mktemp --suffix=.ipynb)
        jupytext --to notebook "$nb" -o "$tmp"
        papermill "$tmp" "_build/notebooks/${base}.ipynb" \
            --kernel ocaml-jupyter \
            --cwd notebooks \
            --execution-timeout 60
        rm -f "$tmp"
    done
