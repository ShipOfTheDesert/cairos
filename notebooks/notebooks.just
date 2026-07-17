# Notebook execution, split into focused recipes. Imported by the root
# justfile; `notebooks` runs as part of the default gate. Recipes execute
# from the repo root (the importing justfile's directory), so notebook and
# --project paths are repo-root-relative.

# uv provides jupytext + papermill (notebooks/pyproject.toml); the
# ocaml-jupyter kernel comes from the opam switch. Machines without uv
# soft-skip, matching `validate`. CI must install uv to enforce execution.
notebooks:
    #!/usr/bin/env bash
    set -euo pipefail
    command -v uv >/dev/null || { echo "uv not installed, skipping notebooks"; exit 0; }
    just notebooks-install
    just notebooks-kernel
    just notebooks-run

# Install libs into the switch prefix so the kernel resolves
# `#require "cairos_jupyter"` (and friends) through findlib, not _build.
notebooks-install:
    opam exec -- dune install cairos cairos_io cairos_finance cairos_plot cairos_jupyter

# Register the ocaml-jupyter kernelspec into the notebooks venv. genspec
# hardcodes `-init $HOME/.ocamlinit` (which need not exist); repoint it at the
# committed notebooks/kernel.ocamlinit that loads topfind, so `#require` works
# hermetically without depending on a global ~/.ocamlinit.
notebooks-kernel:
    #!/usr/bin/env bash
    set -euo pipefail
    prefix="$(opam var prefix)"
    opam exec -- ocaml-jupyter-opam-genspec
    init="$(pwd)/notebooks/kernel.ocamlinit"
    tmp="$(mktemp)"
    jq --arg init "$init" '(.argv | index("-init")) as $i | .argv[$i+1] = $init' \
        "$prefix/share/jupyter/kernel.json" > "$tmp"
    mv "$tmp" "$prefix/share/jupyter/kernel.json"
    uv run --project notebooks jupyter kernelspec install --sys-prefix \
        --name ocaml-jupyter "$prefix/share/jupyter"

# Convert each jupytext .ml to a notebook and execute it through papermill on
# the ocaml-jupyter kernel. The kernel.json embeds the switch path, so the
# venv's papermill discovers and drives it via --sys-prefix (notebooks-kernel).
notebooks-run:
    #!/usr/bin/env bash
    set -euo pipefail
    mkdir -p _build/notebooks
    for nb in notebooks/[0-9]*.ml; do
        base=$(basename "$nb" .ml)
        tmp=$(mktemp --suffix=.ipynb)
        uv run --project notebooks jupytext --to notebook "$nb" -o "$tmp"
        uv run --project notebooks papermill "$tmp" "_build/notebooks/${base}.ipynb" \
            --kernel ocaml-jupyter --cwd notebooks --execution-timeout 60
        rm -f "$tmp"
    done
