(** Cairos bench JSON I/O and regression diff.

    Used by [bench/bench_*.ml] in JSON-emit mode and by [bench/bench_compare.ml]
    / [bench/bench_record.ml] to load the committed [bench/baseline.json] (or a
    directory of per-bench tempfiles produced by the [just bench-record] /
    [just bench-compare] shell loops) and emit a diff-stable rewrite. Schema is
    pinned at {!schema_version}; documents written under one version are not
    read by a later version (the loader rejects them with [Error _], which
    surfaces as an explicit "rebaseline required" message at the executable
    boundary). *)

(** {1 Schema} *)

val schema_version : string
(** ["cairos-bench-baseline-v1"]. Stored under the top-level ["$schema"] key of
    every consolidated document and every per-bench document. *)

val monotonic_clock_label : string
(** ["monotonic-clock"]. The Bechamel instance label the regression gate checks;
    allocation labels ([minor-allocated], [major-allocated]) are recorded in the
    baseline but never gated. Hoisted as a constant so a typo at a call site is
    a compile error. The unit-test suite asserts that
    [Bechamel.Toolkit.Instance.monotonic_clock |> Bechamel.Measure.label] equals
    this constant, so a future Bechamel-side label rename surfaces in CI rather
    than silently disabling the gate. *)

(** {1 Cell type} *)

type cell = {
  bench : string;
      (** Bench file name without [bench_] prefix or [.ml] suffix, e.g.
          ["window_rolling"]. *)
  name : string;  (** [Bechamel.Test.t]'s name, e.g. ["rolling/n=10"]. *)
  instance : string;
      (** One of ["monotonic-clock"], ["minor-allocated"], ["major-allocated"].
          Stable strings; the diff gates only on ["monotonic-clock"]. *)
  estimate : float;
      (** OLS-fitted estimate of the per-iteration value in the instance's
          native unit (ns for monotonic-clock, words for allocation instances).
      *)
  r_square : float;
      (** OLS goodness-of-fit, retained for diagnostic value but not used by
          {!regress}. *)
}

(** {1 Output mode} *)

val output_mode : unit -> [ `Notty | `Json ]
(** Reads the [CAIROS_BENCH_OUTPUT] environment variable. Returns [`Json] iff
    the value is exactly ["json"] (lowercase, exact match); any other value
    (including unset, ["JSON"], ["yes"]) returns [`Notty]. Bench [.ml] files
    dispatch on this at the top of [let () =]: Notty branch unchanged from the
    pre-PRD pattern, JSON branch calls {!to_channel}. *)

(** {1 JSON I/O} *)

val round_6g : float -> float
(** [round_6g f] rounds [f] to [%.6g] precision (~6 significant digits) so the
    emitted JSON byte output is stable across rebaselines: a 0.1% wall-clock
    drift produces a one-character diff, not a re-formatted line. Non-finite
    values ([Float.nan], [Float.infinity], [Float.neg_infinity]) pass through
    unchanged — defensively, a well-formed bench run never produces them.
    Exposed for tests. *)

val bench_doc_of_cells : bench:string -> cell list -> Yojson.Basic.t
(** [bench_doc_of_cells ~bench cells] builds the per-bench JSON document
    {!to_channel} writes. Cells are sorted by [(name, instance)] before emission
    so the byte output is stable. Exposed so tests and ad-hoc tooling can
    inspect or construct a per-bench document without running a benchmark; the
    contract is identical to {!to_channel}'s. *)

val to_channel :
  out_channel ->
  bench:string ->
  (string, (string, Bechamel.Analyze.OLS.t) Hashtbl.t) Hashtbl.t ->
  Bechamel.Measure.witness list ->
  unit
(** [to_channel oc ~bench results instances] writes a single JSON document
    describing the bench run to [oc]. Document shape:

    {v
    { "$schema": "cairos-bench-baseline-v1",
      "bench": "window_rolling",
      "cells": [
        { "name": "rolling/n=10",
          "instance": "monotonic-clock",
          "estimate": 1234.5,
          "r_square": 0.999 },
        ... ] }
    v}

    The outer [results] table is keyed by Bechamel instance label; the inner
    table is keyed by test name. Cells are emitted sorted by [(name, instance)]
    so the byte output is stable. Floats are emitted via [%.6g] (~6 significant
    digits) — sufficient for the 20% gate, narrow enough that wall-clock noise
    produces single-digit diffs. No trailing newline is emitted. Each bench
    writes its own per-bench document to a tempfile under the
    [just bench-record] / [just bench-compare] tempdir; {!read_bench_dir}
    consumes the tempdir and {!write_consolidated} produces the diff-stable
    [bench/baseline.json].

    A Bechamel result with no OLS estimate (e.g. zero-iteration sample) emits
    [Float.nan] for that cell's [estimate]. [Yojson.Basic.t] cannot represent
    NaN as a JSON number, so {!Yojson.Basic.to_channel} will emit a
    non-conforming token and downstream {!parse_consolidated} or
    {!read_bench_dir} will reject the document — surfacing the empty-sample
    failure at the comparator boundary rather than silently passing the gate. *)

val parse_consolidated : Yojson.Basic.t -> (cell list, string) result
(** Parse a consolidated JSON document — top-level object with ["$schema"] and
    ["benches"] keys, where ["benches"] is an array of per-bench documents (each
    shaped as {!to_channel} emits). Returns [Error msg] on schema-version
    mismatch, missing keys, type errors, or an empty ["benches"] array (the last
    guards against a CI miswiring or upstream loop bug producing a
    syntactically-valid-but-semantically-empty document that would silently pass
    the gate). Does not raise. *)

val load_baseline : path:string -> (cell list, string) result
(** [load_baseline ~path] reads [path] via [Yojson.Basic.from_file] and parses
    via {!parse_consolidated}. Used by the comparator to load the committed
    [bench/baseline.json]. *)

val read_bench_dir : path:string -> (cell list, string) result
(** [read_bench_dir ~path] enumerates every [*.json] file under [path]
    (non-recursive), parses each as a per-bench document (as {!to_channel}
    emits), and returns the merged cell list. Used by [bench_compare.exe] and
    [bench_record.exe] to consolidate the per-bench tempfiles produced by the
    [just bench-record] / [just bench-compare] shell loops, replacing the
    earlier shell-side string-concat wrapper.

    Returns [Error msg] on any IO failure, parse failure, schema mismatch, or if
    the directory contains zero [*.json] files (same empty-input motivation as
    {!parse_consolidated}). Does not raise. *)

val write_consolidated : path:string -> cell list -> (unit, string) result
(** [write_consolidated ~path cells] writes a consolidated document to [path],
    grouped by bench, each group sorted by [(name, instance)] and bench groups
    sorted alphabetically by [bench]. Pretty-printed at indent 2 with sorted
    object keys. Stable byte output for stable input — diff-friendly when
    rebaselining.

    Returns [Error msg] if [path] cannot be opened for writing. Does not raise.
*)

(** {1 Regression diff} *)

type regression = {
  cell : cell;  (** The current-run cell that regressed. *)
  baseline : cell;
      (** The baseline cell with matching [(bench, name, instance)]. *)
  ratio : float;  (** [cell.estimate /. baseline.estimate]. *)
}

type validated_pair
(** A baseline / current pair where every [(bench, name)] in the
    [monotonic-clock] subset of [baseline] is present in the [monotonic-clock]
    subset of [current]. Constructed only by {!validate_coverage}; the only
    thing you can do with one is pass it to {!regress}.

    The abstract type makes the precedence "missing coverage trumps regression
    measurement" structural: you cannot ask whether a cell regressed without
    first establishing that the cell is present in both runs, because {!regress}
    cannot be called without a [validated_pair]. A run with missing-in-current
    cells terminates at {!validate_coverage}'s [Error] arm and never reaches
    {!regress}. *)

val validate_coverage :
  baseline:cell list ->
  current:cell list ->
  (validated_pair, (string * string) list) result
(** [validate_coverage ~baseline ~current] checks whether every [(bench, name)]
    [monotonic-clock] cell in [baseline] is present in [current].

    Returns [Ok pair] when coverage is complete; [pair] can then be passed to
    {!regress}.

    Returns [Error missing] otherwise, where [missing] is the sorted list of
    [(bench, name)] pairs absent from [current]. A deleted bench (or a current
    run that crashed before emitting that cell) is a structurally broken
    comparison; the user must rebaseline ([just bench-record]) before any
    regression measurement is meaningful.

    Only [monotonic-clock] cells participate in coverage and regression — the
    allocation instances are recorded in the baseline for diagnostic value but
    are not gated. Output is sorted by [(bench, name)] for deterministic line
    order in downstream prints. *)

type diff_outcome = Ok_no_regression | Regressions of regression list

val regress : threshold:float -> validated_pair -> diff_outcome
(** [regress ~threshold pair] checks whether any [monotonic-clock] cell in
    [pair]'s current run regresses by more than [threshold] against the matching
    baseline cell.

    For each [(bench, name)] in [pair], if
    [current.estimate /. baseline.estimate > 1.0 +. threshold] the cell is
    reported as a [regression]. Returns [Ok_no_regression] when no cell
    regresses; [Regressions rs] otherwise.

    Cells in [current] without a matching baseline cell — i.e. benches added in
    this run — are silently permitted; they are not present in
    [validated_pair]'s baseline-keyed view, so they cannot show up as either a
    regression or a missing entry.

    Output ordering is deterministic and stable across runs: the [Regressions]
    list is sorted ascending by [(bench, name)] before being returned, matching
    the ordering [validate_coverage] uses for [Error _]. Downstream consumers
    that print the results get the same line order PR-to-PR. *)
