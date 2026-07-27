# Reports `assert` tokens in OCaml source outside comments and string literals.
#
# Text-level lints are blind to syntax in both directions: a bare grep for a
# banned token also fires on the comment or docstring that merely describes it,
# and lib/cairos_io/cairos_io.ml:82 does exactly that today. So this lexes just
# enough OCaml to remove the non-code text before matching:
#
#   - nested (* (* *) *) comments;
#   - string literals, including the backslash continuations ocamlformat
#     introduces when it reflows a long literal across lines;
#   - string literals *inside* comments, which OCaml lexes — so a comment may
#     contain "*)" without closing;
#   - {|quoted|} and {tag|quoted|tag} string literals;
#   - char literals, because '"' would otherwise open a string literal that
#     swallows the rest of the file — a false negative in the guard itself.
#
# POSIX awk only (no gensub, no regex-dialect dependencies): the CI runner's awk
# is not the same implementation as the developer's. ubuntu-latest ships mawk as
# /usr/bin/awk, so mawk — not gawk — is what actually runs this in CI. Verified
# to give identical exit codes (1 on the dirty fixture, 0 on the comment fixture
# and on the real engine source) under gawk 5.4.0, gawk --posix,
# gawk --traditional, mawk 1.3.4, and busybox awk. The lookaheads deliberately
# rely on POSIX substr returning "" past the end of the string, so `gawk --lint`
# reports "start index past end of string" on ordinary input; that is the
# contract, not a defect.
#
# The quoted-string tag class below is lowercase letters and underscore only.
# That is not an oversight: the OCaml manual's grammar for a quoted-string id is
# { a-z | _ }, and the compiler rejects {sql1|..|sql1}, {Sql|..|Sql} and
# {ab'c|..|ab'c} — it reads the brace as a record instead. Widening the class
# would desynchronise this lexer from the real one.
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

function has_assert(code,    off, p, pos, before, after) {
    if (code == "") return 0
    off = 0
    while (1) {
        p = index(substr(code, off + 1), "assert")
        if (p == 0) return 0
        pos = off + p
        before = (pos == 1) ? "" : substr(code, pos - 1, 1)
        after = substr(code, pos + 6, 1)
        if (!is_ident_char(before) && !is_ident_char(after)) return 1
        off = pos
    }
}

function check_eof_state() {
    if (depth > 0 || in_str || in_qstr) {
        printf "lint-asserts: unterminated comment or string literal at end of %s" \
               " — the lexer is out of sync with the source, so this scan proves nothing.\n", \
               curfile > "/dev/stderr"
        state_error = 1
    }
}

FNR == 1 {
    if (started) check_eof_state()
    started = 1
    curfile = FILENAME
    depth = 0
    in_str = 0
    in_comment_str = 0
    in_qstr = 0
    qtag = ""
}

{
    if (has_assert(strip($0))) {
        printf "%s:%d: assert token outside comments and string literals:%s%s\n", \
               FILENAME, FNR, "  ", $0
        violations += 1
    }
}

END {
    if (started) check_eof_state()
    if (state_error) exit 2
    if (violations > 0) exit 1
    exit 0
}
