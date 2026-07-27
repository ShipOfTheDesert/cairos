import 'notebooks/notebooks.just'

# `validate` runs last so the three-way oracle comparison tables are the final
# thing on screen. `notebooks` emits several hundred lines of Jupyter chatter
# and four "Killed by signal" kernel-teardown tracebacks, so anything printed
# before it is effectively invisible. The cost is that a validation failure now
# surfaces after the notebook run rather than before it; `just validate` on its
# own is the fast path while iterating.
default: build test fmt lint notebooks validate

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
    opam install --deps-only --with-test --with-dev-setup -y .

build:
    opam exec -- dune build

test:
    opam exec -- dune runtest

# Opt-in benchmark runner. Intentionally NOT part of the default gate or
# `dune runtest`: benchmarks are manual, time-sensitive, and
# their output is noise for CI. Three recipes share the same per-bench loop:
# `bench` renders Notty for human inspection; `bench-record` writes
# bench/baseline.json under the recording conditions documented in
# bench/README.md; `bench-compare` diffs the
# current run against the committed baseline and exits non-zero on
# >20% wall-clock regression or any missing-in-current cell.
bench:
    #!/usr/bin/env bash
    set -euo pipefail
    shopt -s nullglob
    for ml in bench/bench_*.ml; do
        name=$(basename "$ml" .ml)
        case "$name" in bench_compare|bench_record|bench_emit|bench_fixtures) continue;; esac
        opam exec -- dune exec "bench/$name.exe"
    done

# Refresh bench/baseline.json. Run under the documented recording
# conditions: clean build, no competing load. Each bench writes its JSON output to its own tempfile in
# a tempdir; bench_record.exe reads the tempdir and rewrites
# bench/baseline.json via Bench_emit.write_consolidated for diff stability.
# The OCaml driver assembles the consolidated wrapper so that JSON is built
# by a real serialiser rather than shell-side string concatenation.
bench-record:
    #!/usr/bin/env bash
    set -euo pipefail
    shopt -s nullglob
    tmpdir=$(mktemp -d)
    trap 'rm -rf "$tmpdir"' EXIT
    for ml in bench/bench_*.ml; do
        name=$(basename "$ml" .ml)
        case "$name" in bench_compare|bench_record|bench_emit|bench_fixtures) continue;; esac
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
        case "$name" in bench_compare|bench_record|bench_emit|bench_fixtures) continue;; esac
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

# `--force` re-runs the dune *rules*, but odoc's own outputs stay cached under
# _build/default/_doc, so a broken {!Reference} that warned on the first run
# passes silently on every run after it — the recipe reports cache state, not
# documentation state (feature 0062 Step 2; the dune-odoc-force-flag handbook).
# Clearing _doc first is what makes the invocation discriminate, and odoc warns
# rather than failing, so the grep is what makes it a gate. The tree is
# warning-free, so this needs no allowlist and no baseline count.
lint-doc:
    #!/usr/bin/env bash
    set -euo pipefail
    rm -rf _build/default/_doc
    rc=0
    out=$(opam exec -- dune build @doc --force 2>&1) || rc=$?
    printf '%s' "$out" | grep -v '^$' || true
    if [ "$rc" -ne 0 ]; then
        echo "lint-doc: FAIL — the doc build itself failed (exit $rc)." >&2
        exit "$rc"
    fi
    if printf '%s' "$out" | grep -q 'Warning:'; then
        echo "lint-doc: FAIL — odoc emitted the warnings above. Resolve the reference," \
             "or demote it to a code span if it names a module odoc does not build." >&2
        exit 1
    fi

lint-fmt:
    opam exec -- dune build @fmt

lint-opam:
    opam exec -- opam-dune-lint

# Zero `assert` tokens in engine source outside comments and string literals.
# The engine's stated invariant is that no exception is ever raised, and
# `assert false` breaks it; two code reviews found the unreachability comments
# justifying the nine deleted sites had gone stale, which is why the invariant
# needs a gate rather than a convention.
#
# Self-tests in all three directions against the committed fixtures before
# scanning the real tree. A gate proven once by hand is a gate that rots, and
# the comment direction is not hypothetical — lib/cairos_io/cairos_io.ml:82
# names the token in prose today. The exit-2 arm is the one that matters most:
# if the lexer loses sync it strips the rest of the file as comment text, so a
# fail-open scan would report a clean tree.
#
# Scope is lib/cairos_engine/*.ml and nothing else, stated here rather than left
# to be inferred from the glob: lib/cairos_io/cairos_io.ml:82 (a comment) and
# test/unit/cairos_engine/cross_validate_oracles.ml:525 (real code) carry the
# token today and are deliberately unguarded.
#
# Exit codes follow `bench-compare`: 0 clean, 1 violation, 2 tooling failure —
# which includes a self-test that did not behave as specified.
lint-asserts:
    #!/usr/bin/env bash
    set -euo pipefail
    fixtures=test/lint/fixtures
    rc=0
    scripts/lint-asserts.sh "$fixtures/dirty_engine.ml.fixture" >/dev/null 2>&1 || rc=$?
    if [ "$rc" -ne 1 ]; then
        echo "lint-asserts: SELF-TEST FAILED — $fixtures/dirty_engine.ml.fixture" \
             "must exit 1, got $rc. The gate does not detect a real assert." >&2
        exit 2
    fi
    rc=0
    scripts/lint-asserts.sh "$fixtures/comment_engine.ml.fixture" >/dev/null || rc=$?
    if [ "$rc" -ne 0 ]; then
        echo "lint-asserts: SELF-TEST FAILED — $fixtures/comment_engine.ml.fixture" \
             "must exit 0, got $rc. The gate matches the token in comments or strings." >&2
        exit 2
    fi
    rc=0
    scripts/lint-asserts.sh "$fixtures/truncated_engine.ml.fixture" >/dev/null 2>&1 || rc=$?
    if [ "$rc" -ne 2 ]; then
        echo "lint-asserts: SELF-TEST FAILED — $fixtures/truncated_engine.ml.fixture" \
             "must exit 2, got $rc. The gate does not detect that the lexer lost sync," \
             "so it can strip a dirty file as comment text and report it clean." >&2
        exit 2
    fi
    scripts/lint-asserts.sh lib/cairos_engine/*.ml

lint: lint-doc lint-fmt lint-opam lint-asserts

validate-generate:
    uv run validation/reference.py

validate-check:
    opam exec -- dune exec test/unit/cairos_finance/cross_validate.exe
    opam exec -- dune exec test/unit/cairos/cross_validate_frame.exe
    opam exec -- dune exec test/unit/cairos/cross_validate_resample.exe
    opam exec -- dune exec test/unit/cairos_engine/cross_validate.exe
    # Reads the COMMITTED fixtures under validation/oracle_fixtures/ and runs the
    # engine in-process. Needs no Python, no vectorbt and no nautilus_trader —
    # which is why it belongs here while the three uv scripts that WRITE those
    # fixtures stay in the opt-in validate-oracle. Without this line nothing in
    # the gate ever checks the engine against the two third-party references,
    # and an engine change would drift from them silently until someone
    # remembered to run the opt-in recipe. Costs ~0.03s.
    opam exec -- dune exec test/unit/cairos_engine/cross_validate_oracles.exe

validate:
    #!/usr/bin/env bash
    set -euo pipefail
    command -v uv >/dev/null || { echo "uv not installed, skipping validation"; exit 0; }
    just validate-generate
    just validate-check

# Opt-in REGENERATION of the third-party oracle fixtures. What is opt-in is
# running the two oracles, not comparing against them: the comparison binary is
# the last line of `validate-check` and therefore runs in `default` and in CI on
# every change. This recipe is what rewrites the committed fixtures that
# comparison reads.
#
# The two oracles pull heavyweight third-party runtimes (vectorbt resolves ~59
# packages including numba and LLVM) and stay out of the gate for that reason —
# unrelated upstream breakage in either must not be able to redden a per-PR run.
# Nothing else about them is expensive: the comparison itself is ~0.03s and needs
# neither dependency, since the fixtures are committed.
#
# Unlike `validate`, a missing `uv` fails rather than skipping: this recipe is
# only ever run on purpose, so silently doing nothing would be the wrong answer.
validate-oracle:
    #!/usr/bin/env bash
    set -euo pipefail
    command -v uv >/dev/null || {
        echo "validate-oracle: uv is required to run the oracles. Install it with:" >&2
        echo "    curl -LsSf https://astral.sh/uv/install.sh | sh" >&2
        exit 2
    }
    uv run validation/oracle_scenarios.py
    uv run validation/vectorbt_oracle.py
    uv run validation/nautilus_oracle.py
    # The fixtures under validation/oracle_fixtures/ are committed, and the three
    # scripts above have just overwritten them in place. If a regenerated fixture
    # differs from the committed one, the comparison that follows would run on
    # the new numbers and pass, leaving the drift invisible until someone
    # noticed the dirty worktree. Report it here instead. Not a hard failure:
    # regenerating after a deliberate scenario change is exactly how these
    # fixtures get updated, and the operator is the one who knows which it is.
    if ! git diff --quiet -- validation/oracle_fixtures/; then
        echo "validate-oracle: regenerated fixtures differ from the committed ones:" >&2
        git diff --stat -- validation/oracle_fixtures/ >&2
        echo "validate-oracle: intended? commit them. Not intended? git checkout -- validation/oracle_fixtures/" >&2
    fi
    opam exec -- dune exec test/unit/cairos_engine/cross_validate_oracles.exe
