default: build test fmt lint notebooks

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
