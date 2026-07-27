# Reports `string` on the error side of a `result` type in OCaml signatures,
# outside comments and string literals.
#
# Text-level lints are blind to syntax in both directions: a bare grep for
# `(_, string) result` also fires on the doc comment that explains why a
# surface stopped returning one, which is prose these .mli files carry. So this
# lexes just enough OCaml to remove the non-code text before matching. The
# lexer below (is_ident_char, is_tag_char, char_literal_len, qstring_open_len,
# strip) is the same one lint-asserts.awk carries and is deliberately a second
# copy rather than a shared file: the two gates match different things — one a
# single token on one line, this one a type expression across several — and the
# extraction is worth doing when a third consumer appears. Fix them together.
#
# The line-spanning part is what distinguishes this gate from the assert one.
# Every .mli scanned is ocamlformat output, and ocamlformat breaks a result
# type whose success side is wide:
#
#   ( ('freq, (float, Bigarray.float64_elt) Nx.t, [ `Column_major ]) t,
#     string )
#   result
#
# No line there contains `string) result`, so a line-at-a-time scan reports the
# file clean. Matching therefore runs over the whole file: each line is
# stripped, its whitespace collapsed, and the result appended to one buffer,
# with the buffer position of each line recorded so a match can be reported
# against the line the offending `string` sits on.
#
# What positions the match as the *error* side is that `string` is followed by
# the closing parenthesis and then by `result`: `(string, err) result` puts a
# comma there instead, and `('freq, string) Series.t` names another type. Both
# are in the clean fixture, and removing either half of that requirement fails
# it. The leading comma is load-bearing too, for the symmetric reason: it is the
# identifier boundary on that side. Without it `(int, err_string) result` — a
# structured error side whose type name merely ends in `string` — matches on its
# `string) result` tail and becomes a false positive. Pinned by the clean
# fixture's `err_string`, as the trailing boundary is by its `result_summary`.
#
# Scope covers `result`, `Stdlib.result` and `Result.t`. The qualified spellings
# are not hypothetical: lib/cairos_engine/cairos_engine.mli declares
# `type 'freq result = private {...}`, which shadows Stdlib.result inside that
# signature, so Backtest.run *cannot* spell its return type bare — it reads
# `('freq result, err) Stdlib.result`. A gate matching only the bare form is
# permanently blind to the one package e05 and e06 extend with new fallible
# surface. All three spellings are in string_error_qualified.mli.fixture.
#
# One evasion remains and no pattern can close it: a type alias. `type err =
# string` followed by `val f : ... -> (int, err) result` launders a prose error
# side past any text-level scan, because the offending token is not in the
# signature. Catching it needs type information, which means ocaml-lsp, not awk.
# CONTRIBUTING section X states this rather than leaving it inferred.
#
# POSIX awk only (no gensub, no regex-dialect dependencies): the CI runner's
# awk is not the same implementation as the developer's. ubuntu-latest ships
# mawk as /usr/bin/awk, so mawk — not gawk — is what actually runs this in CI.
# `split("", arr)` clears an array rather than `delete arr`, for the same
# reason. Verified to give identical exit codes on all five fixtures (1, 0, 1,
# 2, 1) and on the thirteen real .mli files (0), with byte-identical output, under
# gawk 5.4.0, gawk --posix, gawk --traditional, mawk 1.3.4 20250131 (in
# debian:stable-slim) and busybox awk (in busybox:latest). As in
# lint-asserts.awk, the lookaheads rely on POSIX substr returning "" past the
# end of the string.
#
# Exit: 0 clean, 1 at least one violation, 2 lexer state desynchronised.

BEGIN {
    started = 0
    violations = 0
    state_error = 0
}

function is_ident_char(c) {
    return c != "" && \
        index("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'", c) > 0
}

function is_tag_char(c) {
    return c != "" && index("abcdefghijklmnopqrstuvwxyz_", c) > 0
}

# Length of the char literal starting at position i, or 0 if the quote there is
# an identifier prime (x') or a type variable ('freq) instead.
function char_literal_len(line, i,    c2, c3, body) {
    c2 = substr(line, i + 1, 1)
    if (c2 == "" || c2 == "'") return 0
    if (c2 != "\\") {
        if (substr(line, i + 2, 1) == "'") return 3
        return 0
    }
    c3 = substr(line, i + 2, 1)
    if (c3 == "") return 0
    if (index("\\\"'ntbr ", c3) > 0) body = 1
    else if (index("0123456789", c3) > 0) body = 3
    else if (c3 == "x") body = 3
    else if (c3 == "o") body = 4
    else return 0
    if (substr(line, i + 2 + body, 1) == "'") return body + 3
    return 0
}

# Length of the "{tag|" opener of a quoted string literal starting at i, or 0
# if the brace opens a record instead.
function qstring_open_len(line, i,    j, ch) {
    j = i + 1
    while (1) {
        ch = substr(line, j, 1)
        if (ch == "|") return j - i + 1
        if (!is_tag_char(ch)) return 0
        j++
    }
}

# Returns the line with comments and string-literal contents removed. Lexer
# state (depth, in_str, in_comment_str, in_qstr, qtag) persists across lines,
# because all three constructs may span them.
function strip(line,    out, i, n, c, two, k, closer, p) {
    out = ""
    i = 1
    n = length(line)
    while (i <= n) {
        c = substr(line, i, 1)
        two = (i < n) ? substr(line, i, 2) : ""

        if (in_qstr) {
            closer = "|" qtag "}"
            p = index(substr(line, i), closer)
            if (p == 0) {
                i = n + 1
            } else {
                i = i + p - 1 + length(closer)
                in_qstr = 0
                qtag = ""
            }
            continue
        }

        if (in_str) {
            if (c == "\\") { i += 2; continue }
            if (c == "\"") { in_str = 0; i += 1; continue }
            i += 1
            continue
        }

        if (depth > 0) {
            if (in_comment_str) {
                if (c == "\\") { i += 2; continue }
                if (c == "\"") { in_comment_str = 0; i += 1; continue }
                i += 1
                continue
            }
            if (two == "(*") { depth += 1; i += 2; continue }
            if (two == "*)") { depth -= 1; i += 2; continue }
            if (c == "\"") { in_comment_str = 1; i += 1; continue }
            i += 1
            continue
        }

        if (two == "(*") { depth = 1; i += 2; continue }
        if (c == "\"") { in_str = 1; i += 1; continue }
        if (c == "'") {
            k = char_literal_len(line, i)
            if (k > 0) { i += k; continue }
            out = out c
            i += 1
            continue
        }
        if (c == "{") {
            k = qstring_open_len(line, i)
            if (k > 0) {
                qtag = (k > 2) ? substr(line, i + 1, k - 2) : ""
                in_qstr = 1
                i += k
                continue
            }
            out = out c
            i += 1
            continue
        }
        out = out c
        i += 1
    }
    return out
}

# The source line whose stripped text contains buffer position pos.
function line_of(pos,    i) {
    for (i = 1; i <= nchunk; i++)
        if (pos <= chunk_end[i]) return chunk_line[i]
    return chunk_line[nchunk]
}

# Scans the accumulated buffer and reports every match. Called once per file,
# after its last line.
function scan_buffer(    off, p, len, after, spos, ln) {
    off = 0
    while (1) {
        if (match(substr(buf, off + 1), \
                  /, ?string ?\) ?((Stdlib\.)?result|Result\.t)/) == 0) return
        p = off + RSTART
        len = RLENGTH
        after = substr(buf, p + len, 1)
        if (!is_ident_char(after)) {
            spos = (substr(buf, p + 1, 1) == "s") ? p + 1 : p + 2
            ln = line_of(spos)
            printf "%s:%d: string on the error side of a result type:%s%s\n", \
                   curfile, ln, "  ", srcline[ln]
            violations += 1
        }
        off = p
    }
}

function check_eof_state() {
    if (depth > 0 || in_str || in_qstr) {
        printf "lint-string-errors: unterminated comment or string literal at end of %s" \
               " — the lexer is out of sync with the source, so this scan proves nothing.\n", \
               curfile > "/dev/stderr"
        state_error = 1
    }
}

function finish_file() {
    check_eof_state()
    scan_buffer()
}

FNR == 1 {
    if (started) finish_file()
    started = 1
    curfile = FILENAME
    depth = 0
    in_str = 0
    in_comment_str = 0
    in_qstr = 0
    qtag = ""
    buf = ""
    nchunk = 0
    split("", srcline)
    split("", chunk_end)
    split("", chunk_line)
}

{
    srcline[FNR] = $0
    code = strip($0)
    # \r is in the class deliberately. A CRLF file leaves the carriage return at
    # the end of every stripped line, and joining two lines then produces
    # "string)\r result", which the pattern misses — silently, and only for the
    # multi-line shape the wrapped fixture exists to pin. The tree is LF and
    # ocamlformat writes LF, so this is prophylactic, but a false negative that
    # depends on line endings is exactly the failure mode this gate is for.
    gsub(/[ \t\r]+/, " ", code)
    sub(/^ /, "", code)
    sub(/ $/, "", code)
    if (code != "") {
        nchunk += 1
        buf = (buf == "") ? code : buf " " code
        chunk_end[nchunk] = length(buf)
        chunk_line[nchunk] = FNR
    }
}

END {
    if (started) finish_file()
    if (state_error) exit 2
    if (violations > 0) exit 1
    exit 0
}
