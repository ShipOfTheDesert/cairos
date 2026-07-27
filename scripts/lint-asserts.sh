#!/usr/bin/env bash
# Engine-assert gate. Fails if any OCaml file given as an argument contains an
# `assert` token outside comments and string literals.
# The lexing lives in lint-asserts.awk next to this file.
#
# The files are arguments rather than a hardcoded lib/cairos_engine/*.ml so that
# the justfile's two self-test fixtures exercise this exact code path — a gate
# whose self-test runs different code from the real scan proves nothing about
# the real scan.
#
# Exit: 0 clean, 1 violation, 2 tooling failure.
set -euo pipefail

if [ "$#" -eq 0 ]; then
    echo "usage: ${0##*/} <file.ml> [file.ml ...]" >&2
    exit 2
fi

for f in "$@"; do
    if [ ! -f "$f" ]; then
        echo "lint-asserts: not a readable file: $f" >&2
        exit 2
    fi
done

here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

rc=0
awk -f "$here/lint-asserts.awk" "$@" || rc=$?

case "$rc" in
0)
    echo "lint-asserts: OK — $# file(s) scanned, no assert token outside comments and string literals."
    ;;
1)
    echo "lint-asserts: FAIL — carry the invariant in a type, or in a result that stays internal to the engine. See CONTRIBUTING.md section V." >&2
    ;;
*)
    echo "lint-asserts: tooling failure (awk exit $rc)." >&2
    ;;
esac

exit "$rc"
