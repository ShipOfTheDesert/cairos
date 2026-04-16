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
# their output is noise for CI. Run explicitly with `just bench`.
bench:
    opam exec -- dune exec bench/bench_series_map.exe

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
    opam exec -- dune install cairos cairos_finance cairos_plot cairos_jupyter
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
