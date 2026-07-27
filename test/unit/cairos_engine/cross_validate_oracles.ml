(* Three-way comparison of the shared oracle scenarios.

   Each scenario exists in three implementations: the Cairos engine (run
   in-process here), the vectorbt oracle, and the Nautilus oracle. The two
   oracles are third-party order engines whose outputs are committed under
   [validation/oracle_fixtures/], so this binary needs neither Python nor
   either heavyweight dependency to run — it reads the committed fixtures and
   runs the engine against the same committed inputs.

   This is a different question from [cross_validate.ml]. That one asks whether
   the engine conforms to the specification, using a clean-room reference
   derived from the specification alone. This one asks whether the
   specification itself agrees with how independently authored backtesters
   model the same book, which no spec-derived reference can answer.

   Pattern mirrors [cross_validate.ml], [test/unit/cairos/cross_validate_frame.ml]
   and [test/unit/cairos_finance/cross_validate.ml]: an [executable], not a
   [test], so [dune runtest] never depends on a fixture-generating toolchain.
   It runs as the last line of [just validate-check], and therefore in the
   default gate and in CI: the fixtures are committed, so this binary needs
   neither heavyweight dependency. What stays opt-in is [just validate-oracle],
   which regenerates those fixtures by running the two oracles.

   OUTPUT, in this order: one line per scenario, then the verdict, then every
   scenario's three equity paths bar by bar. The tables come last and always —
   not behind a flag — because the point of running three implementations is
   being able to see that they agree, and a summary line asserting agreement is
   the same thing this feature exists to distrust. Grouping them after the
   verdict keeps the one-liners contiguous.

   A disagreement prints the failing scenario's table immediately instead, since
   the run stops there and which bar a divergence starts at is most of the
   diagnosis.

   NOTHING ABOUT A SCENARIO IS RESTATED HERE. The scenario set, each scenario's
   rebalance schedule, and the two cost parameters all arrive as committed
   fixtures written by [validation/oracle_scenarios.py], alongside the prices
   and signals. An earlier revision hard-coded the schedule and the costs, and a
   seeded mutation showed why that is not survivable: changing this binary's own
   [long_short_flip] schedule from [1; 5] to [1; 4] made it report "two systems
   agree against cairos, so cairos is the finding" — a confident, well-formatted
   accusation against the engine, when the fault was entirely in this file. A
   binary whose loudest output names a culprit must not hold a private copy of
   any input.

   Tolerance is absolute [1e-10] on floats, with a NaN-aware comparator that
   branches on both operands before subtracting. Comparator and exit-code
   helpers come from [Validate_support], shared with the other harnesses; its
   NaN branches are pinned by [test/unit/support/test_validate_support.ml],
   because nothing here reaches them — [read_equity_fixture] rejects a
   non-finite fixture cell and [check_engine_finite] rejects a non-finite engine
   value, both before any comparison happens.

   EXIT CODES. [2] means this binary could not do its job: a missing or
   malformed fixture, a header that is not what the writer promised, a
   non-finite value in a committed fixture, a parked entry without its
   evidence. [1] means it ran correctly and the systems disagree. Keeping a
   broken fixture out of the [1] bucket matters more here than in the other
   harnesses: a [1] from this binary is read as an accusation against a named
   system, and a corrupt CSV is not evidence about anybody's arithmetic.

   ADJUDICATION. A disagreement where two systems agree against one identifies
   the odd system out, and is reported as such. A three-way split identifies no
   culprit; so does the case where closeness fails to be transitive at this
   tolerance. Both are reported with all three values and the classification,
   and both exit [1]: the binary does not decide that a disagreement is
   acceptable. A disagreement judged a convention difference rather than a
   defect is parked by a human, by adding the scenario to [parked] below with
   the investigation document that records the evidence — which is what keeps a
   parked scenario from leaving this recipe permanently red without also
   letting it vanish silently.

   The per-bar check is load-bearing, not merely a refinement of the final-NAV
   check. A pre-trade equity read in the Nautilus oracle offsets every
   rebalance bar by that bar's cost while leaving the final NAV untouched, and
   nothing inside that oracle's own guards detects it. *)

let binary = "cross_validate_oracles"
let fixture_dir = Validate_support.oracle_fixture_dir
let tolerance = Validate_support.default_tolerance
let die_tooling fmt = Validate_support.die_tooling ~binary fmt
let die_mismatch fmt = Validate_support.die_mismatch ~binary fmt
let read_lines path = Validate_support.read_lines ~binary path
let float_close a b = Validate_support.float_close ~tolerance a b
let abs_diff = Validate_support.abs_diff

(* Scenarios whose three-way disagreement has been investigated and recorded as
   something other than a defect. Each names the discrepancy record that holds
   the evidence, and each is skipped with a notice rather than compared, so a
   parked entry cannot leave this recipe permanently red. Empty means every
   shared scenario is expected to agree three ways.

   Both fields are checked before any scenario runs — see [check_parked]. *)
type parked_scenario = {
  parked_id : string;
  discrepancy_id : string;
  document : string;
}

let parked : parked_scenario list = []

(* --- fixture readers ------------------------------------------------------ *)

let read_frame_fixture path =
  match Cairos_io.frame_of_csv ~freq:Cairos.Freq.Day path with
  | Ok f -> f
  | Error e -> die_tooling "%s: %s" path (Cairos_io.err_to_string e)

let split_header_and_rows ~expected path =
  match read_lines path with
  | [] -> die_tooling "%s: empty file" path
  | header :: rest ->
      if String.trim header <> expected then
        die_tooling "%s: expected header %S, got %S" path expected
          (String.trim header);
      rest

(* The scenario set itself is a fixture, so adding a fourth scenario to
   [oracle_scenarios.py] cannot leave this binary silently comparing three. *)
let read_manifest () =
  let path = Filename.concat fixture_dir "oracle_manifest.csv" in
  match split_header_and_rows ~expected:"scenario_id" path with
  | [] -> die_tooling "%s: no scenarios listed" path
  | rows -> List.map String.trim rows

let parse_float ~path ~what s =
  match Float.of_string_opt (String.trim s) with
  | Some f -> f
  | None -> die_tooling "%s: cannot parse %s %S as float" path what s

let parse_int ~path ~what s =
  match int_of_string_opt (String.trim s) with
  | Some i -> i
  | None -> die_tooling "%s: cannot parse %s %S as int" path what s

type params = {
  commission : float;
  slippage : float;
  rebalance_bars : int array;
}

(* Every input the engine run needs beyond the price and signal frames. Emitted
   by [oracle_scenarios.write_input_fixtures] from the same definitions the two
   oracles import, so all three systems provably run one schedule and one pair
   of cost parameters. *)
let read_params ~scenario_id path =
  let rows = split_header_and_rows ~expected:"key,value" path in
  let table =
    List.map
      (fun line ->
        match String.split_on_char ',' line with
        | [ k; v ] -> (String.trim k, String.trim v)
        | _ -> die_tooling "%s: expected 2 columns, got %S" path line)
      rows
  in
  let get key =
    match List.assoc_opt key table with
    | Some v -> v
    | None -> die_tooling "%s: missing key %S" path key
  in
  let bars =
    match String.split_on_char ';' (get "rebalance_bars") with
    | [ "" ]
    | [] ->
        die_tooling "%s: %s has no rebalance bars" path scenario_id
    | parts ->
        Array.of_list
          (List.map (fun s -> parse_int ~path ~what:"rebalance bar" s) parts)
  in
  {
    commission = parse_float ~path ~what:"commission" (get "commission");
    slippage = parse_float ~path ~what:"slippage" (get "slippage");
    rebalance_bars = bars;
  }

(* Reads an oracle equity fixture. Timestamps are full RFC 3339 with an
   explicit [Z] — the shape both oracles emit precisely so that row alignment
   between the three systems can be verified rather than assumed, which is what
   [check_timestamps] below does with them.

   A blank cell, a [nan] literal, or an infinity in a committed fixture is a
   malformed fixture and exits [2] here, at the point of parsing. It is
   deliberately not allowed to travel onward as a value and surface as a
   disagreement: no scenario in this set can legitimately produce a non-finite
   equity value, so such a cell says the file is broken, not that the systems
   are in conflict. *)
let read_equity_fixture path : (Ptime.t * float) array =
  let rows = split_header_and_rows ~expected:"timestamp,value" path in
  List.mapi
    (fun i line ->
      let line_num = i + 2 in
      match String.split_on_char ',' line with
      | [ ts; v ] ->
          let ptime =
            match Ptime.of_rfc3339 (String.trim ts) with
            | Ok (t, _, _) -> t
            | Error _ ->
                die_tooling "%s line %d: cannot parse %S as RFC 3339" path
                  line_num ts
          in
          let trimmed = String.trim v in
          if trimmed = "" then
            die_tooling "%s line %d: empty equity cell" path line_num;
          let value =
            match Float.of_string_opt trimmed with
            | Some f -> f
            | None ->
                die_tooling "%s line %d: cannot parse %S as float" path line_num
                  trimmed
          in
          if not (Float.is_finite value) then
            die_tooling
              "%s line %d: equity is %.17g — no scenario here can legitimately \
               produce a non-finite equity value, so this fixture is malformed"
              path line_num value;
          (ptime, value)
      | _ ->
          die_tooling "%s line %d: expected 2 columns, got %S" path line_num
            line)
    rows
  |> Array.of_list

(* The engine's own output, unlike a fixture, is a system under comparison, so a
   non-finite value here is a finding about the engine rather than a broken
   file — [1], not [2]. The oracles' values never reach this: they are rejected
   as malformed at parse time above. That asymmetry is the whole point of
   splitting the two checks. *)
let check_engine_finite ~scenario_id ~bar ~value =
  if not (Float.is_finite value) then
    die_mismatch
      "%s: cairos bar %d equity is %.17g — no scenario here can legitimately \
       produce a non-finite equity value"
      scenario_id bar value

(* --- engine run ----------------------------------------------------------- *)

let frame_index frame =
  let name = Cairos.Nonempty.hd (Cairos.Frame.columns frame) in
  match Cairos.Frame.get name frame with
  | Some s -> Cairos.Series.index s
  | None -> die_tooling "frame missing column %S" name

let build_rebalance_index ~scenario_id ~price_idx ~bars =
  let price_ts = Cairos.Index.timestamps price_idx in
  let n = Array.length price_ts in
  let floats =
    Array.map
      (fun bar ->
        if bar < 0 || bar >= n then
          die_tooling
            "%s: rebalance bar %d out of price-fixture range [0, %d) — the \
             params fixture and the price fixture disagree"
            scenario_id bar n;
        Ptime.to_float_s price_ts.(bar))
      bars
  in
  match Cairos.Index.of_unix_floats Cairos.Freq.Day floats with
  | Ok idx -> idx
  | Error e ->
      die_tooling "%s: rebalance index: %s" scenario_id
        (Cairos.Index.err_to_string e)

let run_engine ~scenario_id ~prices_path ~signals_path ~params =
  let price_frame = read_frame_fixture prices_path in
  let signal_frame = read_frame_fixture signals_path in
  (* Both frames take their column names from their own CSV header, so a typo in
     one of the two headers is a fixture-format failure rather than anything
     about the engine. Checked here so it exits 2 with the two column lists
     named, instead of reaching [Backtest.run] and surfacing as a validation
     error about the book. *)
  let price_cols = Cairos.Nonempty.to_list (Cairos.Frame.columns price_frame)
  and signal_cols =
    Cairos.Nonempty.to_list (Cairos.Frame.columns signal_frame)
  in
  if price_cols <> signal_cols then
    die_tooling "%s: price columns [%s] but signal columns [%s]" scenario_id
      (String.concat "; " price_cols)
      (String.concat "; " signal_cols);
  let price_idx = frame_index price_frame in
  let rebalance_index =
    build_rebalance_index ~scenario_id ~price_idx ~bars:params.rebalance_bars
  in
  match
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission:params.commission ~slippage:params.slippage
  with
  | Ok r ->
      ( Cairos.Index.timestamps (Cairos.Series.index r.equity_curve),
        Nx.to_array (Cairos.Series.values r.equity_curve) )
  | Error e ->
      die_tooling "%s: Backtest.run: %s" scenario_id
        (Cairos_engine.Backtest.err_to_string e)

(* The three timestamp vectors must be equal element-wise, not merely the same
   length. Both oracles emit full RFC 3339 for this reason: comparing values at
   row i across systems is only meaningful once row i is known to be the same
   bar everywhere. *)
let check_timestamps ~scenario_id ~engine_ts ~vectorbt ~nautilus =
  let n = Array.length engine_ts in
  let check name (rows : (Ptime.t * float) array) =
    let m = Array.length rows in
    if m <> n then
      die_mismatch "%s: %s has %d bars, engine has %d" scenario_id name m n;
    Array.iteri
      (fun i (ts, _) ->
        if not (Ptime.equal ts engine_ts.(i)) then
          die_mismatch "%s: %s bar %d timestamp %s, engine has %s" scenario_id
            name i (Ptime.to_rfc3339 ts)
            (Ptime.to_rfc3339 engine_ts.(i)))
      rows
  in
  check "vectorbt" vectorbt;
  check "nautilus" nautilus

(* --- adjudication --------------------------------------------------------- *)

(* Two-of-three adjudication at one bar. [cv], [cn] and [vn] are the pairwise
   closeness verdicts; the odd system out is the one both others exclude. The
   final branch covers two cases that a defect report must not conflate with a
   clean split: no pair agrees at all, or closeness holds for some pairs but
   not transitively, which happens only when the deviations straddle the
   tolerance and means the tolerance itself is the wrong instrument here. *)
let classify ~cairos ~vectorbt ~nautilus =
  let cv = float_close cairos vectorbt in
  let cn = float_close cairos nautilus in
  let vn = float_close vectorbt nautilus in
  match (cv, cn, vn) with
  | true, true, true -> `Agree
  | false, false, true -> `Odd_one_out "cairos"
  | false, true, false -> `Odd_one_out "vectorbt"
  | true, false, false -> `Odd_one_out "nautilus"
  | false, false, false -> `Three_way_split
  (* All three remaining shapes have two pairs close and one pair not, which
     is only reachable when the deviations straddle the tolerance. Enumerated
     rather than caught by [_] so the set is visible and stays checked. *)
  | true, true, false
  | true, false, true
  | false, true, true ->
      `Non_transitive

(* --- measuring a disagreement --------------------------------------------- *)

(* The gap between [x] and the next representable double. *)
let ulp_gap x =
  let a = Float.abs x in
  Float.succ a -. a

(* A deviation expressed in units in the last place, which is the only form of
   this number a reader can evaluate without arithmetic. [4.441e-16] means
   nothing on its own; "2 ULP" says the two systems differ in the last two bits
   of a double, which is as close as two independent implementations get without
   being bit-identical. For scale, the [1e-10] tolerance is ~4.5e5 ULP near 1.0,
   so agreement at 1-2 ULP is five orders of magnitude inside it.

   Scaled against the larger operand, so the unit is the representable gap where
   the values actually live. Only meaningful while both operands share a
   magnitude — true throughout here, since every value is an equity path near
   1.0 — which is why the absolute deviation is reported alongside rather than
   replaced. *)
let deviation_ulps a b =
  let d = abs_diff a b in
  if d = 0.0 then 0.0
  else
    let scale = Float.max (Float.abs a) (Float.abs b) in
    if scale = 0.0 then 0.0 else d /. ulp_gap scale

(* [0] is reserved for bit-identical and must never be a rounding of something
   nonzero, so anything below 10 keeps a decimal. Above 1000 the ULP framing has
   stopped being informative — that is a real disagreement, and the absolute
   column beside it is the number to read. *)
let format_ulps u =
  if u = 0.0 then "0"
  else if u < 10.0 then Printf.sprintf "%.1f" u
  else if u < 1000.0 then Printf.sprintf "%.0f" u
  else Printf.sprintf "%.1e" u

type bar_verdict = {
  bar : int;
  cairos : float;
  vectorbt : float;
  nautilus : float;
  dev : float;
  ulps : float;
  classification :
    [ `Agree | `Odd_one_out of string | `Three_way_split | `Non_transitive ];
}

type scenario_result = {
  id : string;
  ts : Ptime.t array;
  verdicts : bar_verdict array;
  n_ok : int;
  worst_dev : float;
  worst_ulps : float;
  failures : bar_verdict list;
}

(* Compares one scenario across all three systems bar by bar.

   EVERY bar is evaluated before anything is reported. An earlier revision died
   at the first disagreement, which cannot answer the question a reader actually
   has: one bad bar out of eight is a single event — a rebalance, a cost — while
   eight out of eight is a drift or a misalignment, and the two want completely
   different investigations. Reporting "7/8 bars OK" distinguishes them for free;
   aborting at bar 2 does not.

   Checking every bar rather than only the last is load-bearing for a second
   reason: the final NAV is blind to an offset that a rebalance bar's cost
   introduces and a later bar removes. *)
let compare_scenario ~scenario_id ~engine_ts ~engine_v ~vectorbt ~nautilus =
  let n = Array.length engine_v in
  let verdicts =
    Array.init n (fun i ->
        let cairos = engine_v.(i) in
        (* [check_timestamps] has already established that all three arrays have
           length [n] and agree bar for bar, which is what makes these two
           indexed reads safe and what makes comparing row [i] across systems
           mean anything. *)
        let _, v = vectorbt.(i) in
        let _, na = nautilus.(i) in
        check_engine_finite ~scenario_id ~bar:i ~value:cairos;
        let dev =
          Float.max (abs_diff cairos v)
            (Float.max (abs_diff cairos na) (abs_diff v na))
        in
        let ulps =
          Float.max (deviation_ulps cairos v)
            (Float.max (deviation_ulps cairos na) (deviation_ulps v na))
        in
        {
          bar = i;
          cairos;
          vectorbt = v;
          nautilus = na;
          dev;
          ulps;
          classification = classify ~cairos ~vectorbt:v ~nautilus:na;
        })
  in
  let failures =
    List.filter (fun b -> b.classification <> `Agree) (Array.to_list verdicts)
  in
  {
    id = scenario_id;
    ts = engine_ts;
    verdicts;
    n_ok = n - List.length failures;
    worst_dev = Array.fold_left (fun acc b -> Float.max acc b.dev) 0.0 verdicts;
    worst_ulps =
      Array.fold_left (fun acc b -> Float.max acc b.ulps) 0.0 verdicts;
    failures;
  }

(* --- reporting ------------------------------------------------------------ *)

(* "N/M bars OK" first, because it is the only part most runs need read, and it
   separates the two failure shapes at a glance. The margin follows in the two
   forms that make a bare deviation interpretable: ULP, and how far inside the
   tolerance it sits. *)
let scenario_headline r =
  let n = Array.length r.verdicts in
  let margin =
    if r.worst_dev = 0.0 then "bit-identical on every bar"
    else if r.worst_dev <= tolerance then
      Printf.sprintf "worst %s ULP (%.3e), %.1gx inside tolerance %g"
        (format_ulps r.worst_ulps) r.worst_dev (tolerance /. r.worst_dev)
        tolerance
    else
      Printf.sprintf "worst %.3e, OUTSIDE tolerance %g by %.1gx" r.worst_dev
        tolerance (r.worst_dev /. tolerance)
  in
  match r.failures with
  | [] -> Printf.sprintf "%d/%d bars OK — %s" n n margin
  | fs ->
      Printf.sprintf "%d/%d bars OK, %d OUTSIDE TOLERANCE (bars %s) — %s" r.n_ok
        n (List.length fs)
        (String.concat ", " (List.map (fun b -> string_of_int b.bar) fs))
        margin

(* Renders one scenario's three equity paths bar by bar.

   Markdown, with columns matching [validation/discrepancies/TEMPLATE.md]'s
   "All three outputs" stub exactly, so the output pastes into an investigation
   document without reformatting. FR-8 requires every discrepancy brief to carry
   all three systems' outputs and to say how each figure is reproduced; emitting
   them in the target shape is what makes "run this and paste the table" a
   complete answer to both.

   Values at [%.17g], the fixture convention, not a display rounding. A brief
   whose numbers do not round-trip is a brief whose disagreement cannot be
   re-derived, and these tables exist precisely for the cases where the last
   couple of digits are the whole story. The ULP column is what makes them
   readable anyway: [0] is bit-identical, a small integer is last-bits
   agreement, and anything else is worth looking at. *)
let render_table r =
  Printf.printf "\n### %s — %s\n\n" r.id (scenario_headline r);
  print_string
    "| bar | timestamp | cairos | vectorbt | nautilus | max dev | ULP |\n";
  print_string
    "|-----|-----------|--------|----------|----------|---------|-----|\n";
  Array.iter
    (fun b ->
      (* [~tz_offset_s:0] renders UTC as [Z]. The default is [-00:00], which in
         RFC 3339 means "offset unknown" — misleading in a table whose whole
         purpose is letting a reader line rows up against a fixture. *)
      Printf.printf "| %d | %s | %.17g | %.17g | %.17g | %.3e | %s |\n" b.bar
        (Ptime.to_rfc3339 ~tz_offset_s:0 r.ts.(b.bar))
        b.cairos b.vectorbt b.nautilus b.dev (format_ulps b.ulps))
    r.verdicts;
  print_newline ()

let describe_failure r b =
  let values =
    Printf.sprintf
      "cairos %.17g, vectorbt %.17g, nautilus %.17g (|c-v| %.17g, |c-n| %.17g, \
       |v-n| %.17g, tol %g)"
      b.cairos b.vectorbt b.nautilus
      (abs_diff b.cairos b.vectorbt)
      (abs_diff b.cairos b.nautilus)
      (abs_diff b.vectorbt b.nautilus)
      tolerance
  in
  let timestamp = Ptime.to_rfc3339 ~tz_offset_s:0 r.ts.(b.bar) in
  match b.classification with
  | `Agree -> assert false
  | `Odd_one_out system ->
      Printf.sprintf
        "%s: bar %d (%s) — two systems agree against %s, so %s is the finding: \
         %s"
        r.id b.bar timestamp system system values
  | `Three_way_split ->
      Printf.sprintf
        "%s: bar %d (%s) — three-way split, no two systems agree, so nothing \
         here identifies a culprit: %s. Investigate, and if this is a \
         convention difference rather than a defect, record it and add the \
         scenario to this binary's parked list."
        r.id b.bar timestamp values
  | `Non_transitive ->
      Printf.sprintf
        "%s: bar %d (%s) — deviations straddle the tolerance, so pairwise \
         agreement is not transitive and no two-of-three verdict is available: \
         %s"
        r.id b.bar timestamp values

(* --- parked-list integrity ------------------------------------------------ *)

(* Run before any scenario, so a broken skip list is reported instead of green
   pass lines followed by a failure — and so the documents that say this binary
   refuses to start on a bad parked entry are describing what it does.

   Two ways a skip list rots, both of which turn "parked" into "silently not
   compared": the evidence document is named but never committed, and the entry
   names a scenario that does not exist, in which case the scenario it was meant
   to skip is compared anyway while the summary still counts it as parked. *)
let check_parked ~scenario_ids =
  List.iter
    (fun p ->
      if not (List.mem p.parked_id scenario_ids) then
        die_tooling
          "parked entry %s names scenario %S, which is not in the manifest — \
           the scenario it was meant to skip is being compared, and the \
           summary counts a park that is not happening"
          p.discrepancy_id p.parked_id;
      if not (Sys.file_exists p.document) then
        die_tooling
          "%s is parked as %s but its investigation document %s does not exist \
           — a parked scenario without its evidence is a scenario silently \
           dropped from the comparison"
          p.parked_id p.discrepancy_id p.document)
    parked

(* --- main ----------------------------------------------------------------- *)

let () =
  let path name = Filename.concat fixture_dir (name ^ ".csv") in
  let scenario_ids = read_manifest () in
  check_parked ~scenario_ids;
  let results =
    List.filter_map
      (fun id ->
        match List.find_opt (fun p -> p.parked_id = id) parked with
        | Some p ->
            Printf.printf
              "cross_validate_oracles: %s SKIPPED — parked as %s; the evidence \
               and the open question are in %s\n"
              id p.discrepancy_id p.document;
            None
        | None ->
            let params =
              read_params ~scenario_id:id (path ("oracle_" ^ id ^ "_params"))
            in
            let engine_ts, engine_v =
              run_engine ~scenario_id:id
                ~prices_path:(path ("oracle_" ^ id ^ "_prices"))
                ~signals_path:(path ("oracle_" ^ id ^ "_signals"))
                ~params
            in
            let vectorbt =
              read_equity_fixture (path ("vectorbt_" ^ id ^ "_equity"))
            and nautilus =
              read_equity_fixture (path ("nautilus_" ^ id ^ "_equity"))
            in
            check_timestamps ~scenario_id:id ~engine_ts ~vectorbt ~nautilus;
            let r =
              compare_scenario ~scenario_id:id ~engine_ts ~engine_v ~vectorbt
                ~nautilus
            in
            Printf.printf "cross_validate_oracles: %s — %s, final NAV %.17g\n"
              id (scenario_headline r)
              engine_v.(Array.length engine_v - 1);
            Some r)
      scenario_ids
  in
  let total_bars =
    List.fold_left (fun acc r -> acc + Array.length r.verdicts) 0 results
  in
  let ok_bars = List.fold_left (fun acc r -> acc + r.n_ok) 0 results in
  let worst_dev =
    List.fold_left (fun acc r -> Float.max acc r.worst_dev) 0.0 results
  in
  let worst_ulps =
    List.fold_left (fun acc r -> Float.max acc r.worst_ulps) 0.0 results
  in
  let failures =
    List.concat_map (fun r -> List.map (fun b -> (r, b)) r.failures) results
  in
  if failures = [] then
    Printf.printf
      "\n\
       Cross-validate oracles: OK — %d/%d bars agree across %d scenario(s), %d \
       parked.\n\
      \  Worst disagreement anywhere: %s ULP (%.3e), %.1gx inside the %g \
       tolerance.\n"
      ok_bars total_bars (List.length results) (List.length parked)
      (format_ulps worst_ulps) worst_dev
      (if worst_dev = 0.0 then Float.infinity else tolerance /. worst_dev)
      tolerance
  else
    Printf.printf
      "\n\
       Cross-validate oracles: FAILED — %d/%d bars agree across %d \
       scenario(s), %d outside tolerance %g.\n\
      \  Worst disagreement anywhere: %.3e.\n"
      ok_bars total_bars (List.length results) (List.length failures) tolerance
      worst_dev;
  print_string
    "\n\
     Three-way comparison, bar by bar. Markdown on purpose: these are the\n\
     columns validation/discrepancies/TEMPLATE.md asks for, so a scenario that\n\
     ever disagrees can be written up by pasting rather than retyping.\n";
  List.iter render_table results;
  if failures <> [] then
    die_mismatch "%s"
      (String.concat "\n"
         (List.map (fun (r, b) -> describe_failure r b) failures));
  exit 0
