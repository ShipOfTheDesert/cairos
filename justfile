default: build test fmt lint validate notebooks

build:
    opam exec -- dune build

test:
    opam exec -- dune runtest

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
notebooks:
    #!/usr/bin/env bash
    set -euo pipefail
    command -v jupytext >/dev/null || { echo "jupytext not installed, skipping notebooks"; exit 0; }
    command -v papermill >/dev/null || { echo "papermill not installed, skipping notebooks"; exit 0; }
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
