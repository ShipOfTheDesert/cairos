#!/usr/bin/env bash
# Structured-error gate. Fails if any OCaml signature file given as an argument
# puts `string` on the error side of a `result`, outside comments and string
# literals. The lexing and the matching live in lint-string-errors.awk next to
# this file.
#
# The files are arguments rather than a hardcoded lib/*.mli so that the
# justfile's four self-test fixtures exercise this exact code path — a gate
# whose self-test runs different code from the real scan proves nothing about
# the real scan.
#
# Exit: 0 clean, 1 violation, 2 tooling failure.
set -euo pipefail

if [ "$#" -eq 0 ]; then
    echo "usage: ${0##*/} <file.mli> [file.mli ...]" >&2
    exit 2
fi

for f in "$@"; do
    if [ ! -f "$f" ]; then
        echo "lint-string-errors: not a readable file: $f" >&2
        exit 2
    fi
done

here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

rc=0
awk -f "$here/lint-string-errors.awk" "$@" || rc=$?

case "$rc" in
0)
    echo "lint-string-errors: OK — $# file(s) scanned, no string error side."
    ;;
1)
    echo "lint-string-errors: FAIL — give the module a closed err variant and a sibling err_to_string, and return that instead. See CONTRIBUTING.md section V." >&2
    ;;
*)
    echo "lint-string-errors: tooling failure (awk exit $rc)." >&2
    ;;
esac

exit "$rc"
