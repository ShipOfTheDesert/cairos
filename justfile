default: build test fmt lint

build:
    opam exec -- dune build

run:
    opam exec -- dune exec bookmarkapi

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
