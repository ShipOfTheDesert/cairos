(* Layer 1 — entrypoint validation negative-path tests for
   [Cairos_engine.Backtest.run].

   Each test constructs inputs that violate exactly one of the nine
   preconditions enumerated in the entrypoint validation order
   (mirrored in [lib/cairos_engine/cairos_engine.ml] [validate_inputs])
   and asserts [Backtest.run] returns the [Backtest.err] variant naming
   the violated precondition. *)

let make_daily_index dates =
  match Cairos.Index.daily dates with
  | Ok idx -> idx
  | Error e -> Alcotest.fail (Cairos.Index.err_to_string e)

let make_daily_series dates values =
  let idx = make_daily_index dates in
  let nx = Nx.create Nx.float64 [| Array.length values |] values in
  match Cairos.Series.make idx nx with
  | Ok s -> s
  | Error msg -> Alcotest.fail msg

let make_frame named_series =
  match named_series with
  | [] -> Alcotest.fail "make_frame: at least one column required"
  | (name, s) :: rest -> (
      let nonempty = Cairos.Nonempty.make (name, s) rest in
      match Cairos.Frame.of_series nonempty with
      | Ok f -> f
      | Error msg -> Alcotest.fail msg)

let ptime_of_date s =
  match Ptime.of_rfc3339 (s ^ "T00:00:00Z") with
  | Ok (t, _, _) -> t
  | Error _ -> Alcotest.fail (Printf.sprintf "ptime_of_date: %s" s)

let assert_error ~expect result =
  match result with
  | Ok _ -> Alcotest.fail "expected Error, got Ok"
  | Error e ->
      if not (expect e) then
        Alcotest.fail
          (Printf.sprintf "unexpected error variant: %s"
             (Cairos_engine.Backtest.err_to_string e))

let commission = 0.001
let slippage = 0.0005

(* Step 1 — price and signal frames have different indices. *)
let mismatched_indices () =
  let price_dates = [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] in
  let signal_dates = [| "2024-01-04"; "2024-01-05"; "2024-01-06" |] in
  let prices = [| 1.0; 1.0; 1.0 |] in
  let signals = [| 0.0; 1.0; 0.0 |] in
  let price_frame =
    make_frame [ ("A", make_daily_series price_dates prices) ]
  in
  let signal_frame =
    make_frame [ ("A", make_daily_series signal_dates signals) ]
  in
  let rebalance_index = make_daily_index [| "2024-01-02" |] in
  let result =
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  in
  assert_error result ~expect:(function
    | Cairos_engine.Backtest.Index_mismatch -> true
    | _ -> false)

(* Step 2 — price and signal frames have different columns. *)
let mismatched_columns () =
  let dates = [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] in
  let prices = [| 1.0; 1.0; 1.0 |] in
  let signals = [| 0.0; 1.0; 0.0 |] in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame = make_frame [ ("B", make_daily_series dates signals) ] in
  let rebalance_index = make_daily_index [| "2024-01-02" |] in
  let result =
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  in
  assert_error result ~expect:(function
    | Cairos_engine.Backtest.Column_mismatch { price; signal } ->
        price = [ "A" ] && signal = [ "B" ]
    | _ -> false)

(* Step 3 — rebalance index is empty. *)
let empty_rebalance_index () =
  let dates = [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] in
  let prices = [| 1.0; 1.0; 1.0 |] in
  let signals = [| 0.0; 1.0; 0.0 |] in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame = make_frame [ ("A", make_daily_series dates signals) ] in
  let rebalance_index = make_daily_index [||] in
  let result =
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  in
  assert_error result ~expect:(function
    | Cairos_engine.Backtest.Empty_rebalance_index -> true
    | _ -> false)

(* Step 4 — rebalance date precedes price frame's first bar. *)
let rebalance_before_first_bar () =
  let dates = [| "2024-01-02"; "2024-01-03"; "2024-01-04" |] in
  let prices = [| 1.0; 1.0; 1.0 |] in
  let signals = [| 0.0; 1.0; 0.0 |] in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame = make_frame [ ("A", make_daily_series dates signals) ] in
  let rebalance_index = make_daily_index [| "2024-01-01" |] in
  let result =
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  in
  assert_error result ~expect:(function
    | Cairos_engine.Backtest.Calendar_violations
        { head = Precedes_first_bar { timestamp }; tail = [] } ->
        Ptime.equal timestamp (ptime_of_date "2024-01-01")
    | _ -> false)

(* Step 5 — rebalance date does not match any price-frame row. *)
let rebalance_date_not_in_price_frame () =
  let dates = [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] in
  let prices = [| 1.0; 1.0; 1.0 |] in
  let signals = [| 0.0; 1.0; 0.0 |] in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame = make_frame [ ("A", make_daily_series dates signals) ] in
  let rebalance_index = make_daily_index [| "2024-01-04" |] in
  let result =
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  in
  assert_error result ~expect:(function
    | Cairos_engine.Backtest.Calendar_violations
        { head = No_matching_row { timestamp }; tail = [] } ->
        Ptime.equal timestamp (ptime_of_date "2024-01-04")
    | _ -> false)

(* Step 6 — rebalance date is the last bar (no T+1 open available). *)
let rebalance_on_last_bar () =
  let dates = [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] in
  let prices = [| 1.0; 1.0; 1.0 |] in
  let signals = [| 0.0; 0.0; 1.0 |] in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame = make_frame [ ("A", make_daily_series dates signals) ] in
  let rebalance_index = make_daily_index [| "2024-01-03" |] in
  let result =
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  in
  assert_error result ~expect:(function
    | Cairos_engine.Backtest.Calendar_violations
        { head = Last_bar_no_next_open { timestamp }; tail = [] } ->
        Ptime.equal timestamp (ptime_of_date "2024-01-03")
    | _ -> false)

(* The calendar tier aggregates every offending date rather than failing on
   the first — two violations of different kinds are both reported, in
   rebalance-index order. *)
let calendar_violations_are_aggregated () =
  let dates = [| "2024-01-02"; "2024-01-03"; "2024-01-04" |] in
  let prices = [| 1.0; 1.0; 1.0 |] in
  let signals = [| 0.0; 1.0; 0.0 |] in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame = make_frame [ ("A", make_daily_series dates signals) ] in
  let rebalance_index = make_daily_index [| "2024-01-01"; "2024-01-05" |] in
  let result =
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  in
  assert_error result ~expect:(function
    | Cairos_engine.Backtest.Calendar_violations
        {
          head = Precedes_first_bar { timestamp = t1 };
          tail = [ No_matching_row { timestamp = t2 } ];
        } ->
        Ptime.equal t1 (ptime_of_date "2024-01-01")
        && Ptime.equal t2 (ptime_of_date "2024-01-05")
    | _ -> false)

(* Step 8 — every rebalance has all-zero target weights. *)
let all_zero_target_weights () =
  let dates = [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] in
  let prices = [| 1.0; 1.0; 1.0 |] in
  let signals = [| 0.0; 0.0; 0.0 |] in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame = make_frame [ ("A", make_daily_series dates signals) ] in
  let rebalance_index = make_daily_index [| "2024-01-02" |] in
  let result =
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  in
  assert_error result ~expect:(function
    | Cairos_engine.Backtest.No_nonzero_target_weight -> true
    | _ -> false)

(* Step 7 — a NaN signal cell on a rebalance row.

   Two columns: A carries a valid non-zero target at the rebalance, so the
   all-zero-weights tier is satisfied and the row reaches the loop today,
   where the NaN in B silently poisons every downstream quantity. *)
let nan_signal_at_rebalance_date () =
  let dates = [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |] in
  let prices = [| 1.0; 1.0; 1.0; 1.0 |] in
  let price_frame =
    make_frame
      [
        ("A", make_daily_series dates prices);
        ("B", make_daily_series dates prices);
      ]
  in
  let signal_frame =
    make_frame
      [
        ("A", make_daily_series dates [| 0.0; 1.0; 0.0; 0.0 |]);
        ("B", make_daily_series dates [| 0.0; Float.nan; 0.0; 0.0 |]);
      ]
  in
  let rebalance_index = make_daily_index [| "2024-01-02" |] in
  let result =
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  in
  assert_error result ~expect:(function
    | Cairos_engine.Backtest.Nan_signal_at_rebalance
        { cells = { head = timestamp, instrument; tail = [] } } ->
        Ptime.equal timestamp (ptime_of_date "2024-01-02")
        && String.equal instrument "B"
    | _ -> false)

(* The check is scoped to rebalance rows. Same fixture as above with the NaN
   moved off the rebalance date onto a row the loop never reads — the pair
   pins the scoping, since a whole-frame scan would reject this too. *)
let nan_signal_away_from_rebalance_date () =
  let dates = [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |] in
  let prices = [| 1.0; 1.0; 1.0; 1.0 |] in
  let price_frame =
    make_frame
      [
        ("A", make_daily_series dates prices);
        ("B", make_daily_series dates prices);
      ]
  in
  let signal_frame =
    make_frame
      [
        ("A", make_daily_series dates [| 0.0; 1.0; 0.0; 0.0 |]);
        ("B", make_daily_series dates [| 0.0; 0.0; Float.nan; 0.0 |]);
      ]
  in
  let rebalance_index = make_daily_index [| "2024-01-02" |] in
  match
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  with
  | Ok _ -> ()
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "expected Ok, got Error: %s"
           (Cairos_engine.Backtest.err_to_string e))

(* Ordering: the NaN check precedes the all-zero-weights check. An all-NaN
   rebalance row has no non-zero weight either, and [any_nonzero] skipping
   NaN is the defect — reporting [No_nonzero_target_weight] here would name
   a consequence instead of the cause. *)
let nan_signal_reported_before_all_zero_weights () =
  let dates = [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |] in
  let prices = [| 1.0; 1.0; 1.0; 1.0 |] in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame =
    make_frame [ ("A", make_daily_series dates [| 0.0; Float.nan; 0.0; 0.0 |]) ]
  in
  let rebalance_index = make_daily_index [| "2024-01-02" |] in
  let result =
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  in
  assert_error result ~expect:(function
    | Cairos_engine.Backtest.Nan_signal_at_rebalance
        { cells = { head = timestamp, instrument; tail = [] } } ->
        Ptime.equal timestamp (ptime_of_date "2024-01-02")
        && String.equal instrument "A"
    | _ -> false)

(* Step 9 — a price cell that is not strictly positive and finite at a bar the
   loop reads at non-zero exposure. One predicate covers NaN, both infinities,
   zero and negatives, so each is exercised against the same fixture.

   [A] is held at weight 1.0 from the rebalance at 01-02 onward; the poisoned
   cell sits at 01-03, inside that holding. [B] is present but never traded. *)
let bad_price_values =
  [ Float.nan; Float.infinity; Float.neg_infinity; 0.0; -1.0 ]

let rejects_invalid_price_at_nonzero_weight () =
  let dates = [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |] in
  List.iter
    (fun bad ->
      let price_frame =
        make_frame
          [
            ("A", make_daily_series dates [| 1.0; 1.0; bad; 1.0 |]);
            ("B", make_daily_series dates [| 1.0; 1.0; 1.0; 1.0 |]);
          ]
      in
      let signal_frame =
        make_frame
          [
            ("A", make_daily_series dates [| 0.0; 1.0; 0.0; 0.0 |]);
            ("B", make_daily_series dates [| 0.0; 0.0; 0.0; 0.0 |]);
          ]
      in
      let rebalance_index = make_daily_index [| "2024-01-02" |] in
      let result =
        Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
          ~commission ~slippage
      in
      assert_error result ~expect:(function
        | Cairos_engine.Backtest.Invalid_price
            { cells = { head = timestamp, instrument, value; tail = [] } } ->
            Ptime.equal timestamp (ptime_of_date "2024-01-03")
            && String.equal instrument "A"
            &&
            if Float.is_nan bad then Float.is_nan value
            else Float.equal value bad
        | _ -> false))
    bad_price_values

(* Paired with the rejection above on the same fixture shape: the poison moves
   from [A] to [B], which is never traded and so carries zero held weight at
   the poisoned bar and its predecessor. Load-bearing as a pair — scoping
   without the mark-to-market zero-weight skip would leave the NAV poisoned
   anyway, and the skip without scoping would be unobservable. *)
let accepts_nan_price_at_zero_weight () =
  let dates = [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |] in
  let price_frame =
    make_frame
      [
        ("A", make_daily_series dates [| 1.0; 1.0; 1.0; 1.0 |]);
        ("B", make_daily_series dates [| 1.0; 1.0; Float.nan; 1.0 |]);
      ]
  in
  let signal_frame =
    make_frame
      [
        ("A", make_daily_series dates [| 0.0; 1.0; 0.0; 0.0 |]);
        ("B", make_daily_series dates [| 0.0; 0.0; 0.0; 0.0 |]);
      ]
  in
  let rebalance_index = make_daily_index [| "2024-01-02" |] in
  match
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  with
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "expected Ok, got Error: %s"
           (Cairos_engine.Backtest.err_to_string e))
  | Ok result ->
      let equity =
        Nx.to_array
          (Cairos.Series.values result.Cairos_engine.Backtest.equity_curve)
      in
      Array.iteri
        (fun t v ->
          if not (Float.is_finite v) then
            Alcotest.fail
              (Printf.sprintf
                 "unheld instrument's NaN price poisoned NAV at bar %d: %f" t v))
        equity

(* Mark-to-market at bar [t] applies [held.(t-1)] to both [price.(t)] and
   [price.(t-1)], so the bar an instrument exits on is read at full exposure
   while carrying zero held weight. [A] holds from 01-02 and exits at the
   rebalance on 01-04; the poison sits on 01-04 itself. The execution bars are
   01-03 and 01-05, so this cell is covered by the mark-to-market clause alone
   — a single-clause [held.(t) <> 0.0] scope would accept it and yield a NaN
   NAV. *)
let rejects_invalid_price_at_exit_bar () =
  let dates =
    [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
  in
  let price_frame =
    make_frame
      [ ("A", make_daily_series dates [| 1.0; 1.0; 1.0; Float.nan; 1.0 |]) ]
  in
  let signal_frame =
    make_frame [ ("A", make_daily_series dates [| 0.0; 1.0; 0.0; 0.0; 0.0 |]) ]
  in
  let rebalance_index = make_daily_index [| "2024-01-02"; "2024-01-04" |] in
  let result =
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  in
  assert_error result ~expect:(function
    | Cairos_engine.Backtest.Invalid_price
        { cells = { head = timestamp, instrument, value; tail = [] } } ->
        Ptime.equal timestamp (ptime_of_date "2024-01-04")
        && String.equal instrument "A"
        && Float.is_nan value
    | _ -> false)

(* The execution clause: at every rebalance date, the T+1 price of every
   instrument with a non-zero weight delta must be valid. [A] exits at the
   rebalance on 01-04, so its execution price on 01-05 is read even though
   held weight is zero on 01-05 and on 01-04. A bad value there lands in
   [Trade.exit_price] rather than NAV, which downstream trade metrics ingest. *)
let rejects_invalid_price_at_exit_execution_bar () =
  let dates =
    [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
  in
  let price_frame =
    make_frame
      [ ("A", make_daily_series dates [| 1.0; 1.0; 1.0; 1.0; Float.nan |]) ]
  in
  let signal_frame =
    make_frame [ ("A", make_daily_series dates [| 0.0; 1.0; 0.0; 0.0; 0.0 |]) ]
  in
  let rebalance_index = make_daily_index [| "2024-01-02"; "2024-01-04" |] in
  let result =
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  in
  assert_error result ~expect:(function
    | Cairos_engine.Backtest.Invalid_price
        { cells = { head = timestamp, instrument, value; tail = [] } } ->
        Ptime.equal timestamp (ptime_of_date "2024-01-05")
        && String.equal instrument "A"
        && Float.is_nan value
    | _ -> false)

(* Both new variants document an offender ordering — rebalance-index order
   then frame-column order for signals, bar order then frame-column order for
   prices — that a single-cell fixture cannot pin. Each of the next two tests
   poisons two cells spanning both axes: two instruments on one row, and one
   instrument on a later row. A scan that reported column-major, or that
   dropped its final reversal, would fail here and nowhere else. *)
let nan_signal_cells_are_ordered_bar_then_column () =
  let dates = [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |] in
  let prices = [| 1.0; 1.0; 1.0; 1.0 |] in
  let price_frame =
    make_frame
      [
        ("A", make_daily_series dates prices);
        ("B", make_daily_series dates prices);
      ]
  in
  let signal_frame =
    make_frame
      [
        ("A", make_daily_series dates [| 0.0; Float.nan; 1.0; 0.0 |]);
        ("B", make_daily_series dates [| 0.0; Float.nan; Float.nan; 0.0 |]);
      ]
  in
  let rebalance_index = make_daily_index [| "2024-01-02"; "2024-01-03" |] in
  let result =
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  in
  assert_error result ~expect:(function
    | Cairos_engine.Backtest.Nan_signal_at_rebalance
        { cells = { head = t1, i1; tail = [ (t2, i2); (t3, i3) ] } } ->
        (* 01-02/A and 01-02/B before 01-03/B: row axis outermost, column
           axis within the row. *)
        Ptime.equal t1 (ptime_of_date "2024-01-02")
        && String.equal i1 "A"
        && Ptime.equal t2 (ptime_of_date "2024-01-02")
        && String.equal i2 "B"
        && Ptime.equal t3 (ptime_of_date "2024-01-03")
        && String.equal i3 "B"
    | _ -> false)

let invalid_price_cells_are_ordered_bar_then_column () =
  let dates = [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |] in
  let price_frame =
    make_frame
      [
        ("A", make_daily_series dates [| 1.0; 1.0; 0.0; -1.0 |]);
        ("B", make_daily_series dates [| 1.0; 1.0; Float.nan; 1.0 |]);
      ]
  in
  (* Both instruments are held across the poisoned bars, so every cell below
     is inside the read set. *)
  let signal_frame =
    make_frame
      [
        ("A", make_daily_series dates [| 0.0; 1.0; 0.0; 0.0 |]);
        ("B", make_daily_series dates [| 0.0; 1.0; 0.0; 0.0 |]);
      ]
  in
  let rebalance_index = make_daily_index [| "2024-01-02" |] in
  let result =
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  in
  assert_error result ~expect:(function
    | Cairos_engine.Backtest.Invalid_price
        { cells = { head = t1, i1, v1; tail = [ (t2, i2, v2); (t3, i3, v3) ] } }
      ->
        (* 01-03/A and 01-03/B before 01-04/A. *)
        Ptime.equal t1 (ptime_of_date "2024-01-03")
        && String.equal i1 "A"
        && Float.equal v1 0.0
        && Ptime.equal t2 (ptime_of_date "2024-01-03")
        && String.equal i2 "B"
        && Float.is_nan v2
        && Ptime.equal t3 (ptime_of_date "2024-01-04")
        && String.equal i3 "A"
        && Float.equal v3 (-1.0)
    | _ -> false)

(* [err_to_string] is only ever called from failure arms, so a green suite
   would never execute it. Rendering one value of every variant keeps a
   malformed format string or an exception in the renderer from shipping.
   The assertions are deliberately weak — that the offender appears at all —
   because the variants, not the prose, are the contract. *)
let err_to_string_renders_every_variant () =
  let ts = ptime_of_date "2024-01-02" in
  let contains haystack needle =
    let nh = String.length haystack and nn = String.length needle in
    let rec loop i =
      if i + nn > nh then false
      else if String.equal (String.sub haystack i nn) needle then true
      else loop (i + 1)
    in
    loop 0
  in
  let ne x = Cairos.Nonempty.make x [] in
  let cases =
    [
      (Cairos_engine.Backtest.Index_mismatch, "indices");
      ( Cairos_engine.Backtest.Column_mismatch
          { price = [ "A" ]; signal = [ "B" ] },
        "A" );
      (Cairos_engine.Backtest.Empty_rebalance_index, "empty");
      ( Cairos_engine.Backtest.Calendar_violations
          (ne (Cairos_engine.Backtest.Precedes_first_bar { timestamp = ts })),
        "2024-01-02" );
      (Cairos_engine.Backtest.No_nonzero_target_weight, "non-zero");
      ( Cairos_engine.Backtest.Nan_signal_at_rebalance { cells = ne (ts, "A") },
        "2024-01-02" );
      ( Cairos_engine.Backtest.Invalid_price { cells = ne (ts, "A", 0.0) },
        "2024-01-02" );
    ]
  in
  List.iter
    (fun (e, needle) ->
      let rendered = Cairos_engine.Backtest.err_to_string e in
      Alcotest.(check bool)
        (Printf.sprintf "%S is non-empty" needle)
        true
        (String.length rendered > 0);
      Alcotest.(check bool)
        (Printf.sprintf "rendering mentions %S" needle)
        true (contains rendered needle))
    cases

(* Affirmative arm for the whole suite: the same fixture shape every
   rejection test perturbs is accepted when no precondition is violated, so
   no assertion above can pass because [run] rejects unconditionally. *)
let valid_inputs_are_accepted () =
  let dates = [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] in
  let prices = [| 1.0; 1.0; 1.0 |] in
  let signals = [| 0.0; 1.0; 0.0 |] in
  let price_frame = make_frame [ ("A", make_daily_series dates prices) ] in
  let signal_frame = make_frame [ ("A", make_daily_series dates signals) ] in
  let rebalance_index = make_daily_index [| "2024-01-02" |] in
  match
    Cairos_engine.Backtest.run ~price_frame ~signal_frame ~rebalance_index
      ~commission ~slippage
  with
  | Ok _ -> ()
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "expected Ok, got Error: %s"
           (Cairos_engine.Backtest.err_to_string e))

let () =
  Alcotest.run "cairos_engine.validation_errors"
    [
      ( "validate_inputs",
        [
          Alcotest.test_case "step 1 — mismatched indices" `Quick
            mismatched_indices;
          Alcotest.test_case "step 2 — mismatched columns" `Quick
            mismatched_columns;
          Alcotest.test_case "step 3 — empty rebalance index" `Quick
            empty_rebalance_index;
          Alcotest.test_case "step 4 — rebalance precedes first bar" `Quick
            rebalance_before_first_bar;
          Alcotest.test_case "step 5 — rebalance date not in price frame" `Quick
            rebalance_date_not_in_price_frame;
          Alcotest.test_case "step 6 — rebalance on last bar" `Quick
            rebalance_on_last_bar;
          Alcotest.test_case "calendar violations are aggregated" `Quick
            calendar_violations_are_aggregated;
          Alcotest.test_case "step 7 — NaN signal at rebalance date" `Quick
            nan_signal_at_rebalance_date;
          Alcotest.test_case "step 8 — all-zero target weights" `Quick
            all_zero_target_weights;
          Alcotest.test_case "NaN signal away from rebalance date is accepted"
            `Quick nan_signal_away_from_rebalance_date;
          Alcotest.test_case "NaN signal reported before all-zero weights"
            `Quick nan_signal_reported_before_all_zero_weights;
          Alcotest.test_case "step 9 — invalid price at non-zero weight" `Quick
            rejects_invalid_price_at_nonzero_weight;
          Alcotest.test_case "invalid price at exit bar (mark-to-market clause)"
            `Quick rejects_invalid_price_at_exit_bar;
          Alcotest.test_case "invalid price at exit execution bar" `Quick
            rejects_invalid_price_at_exit_execution_bar;
          Alcotest.test_case "NaN price at zero weight is accepted" `Quick
            accepts_nan_price_at_zero_weight;
          Alcotest.test_case "NaN signal cells are ordered bar-then-column"
            `Quick nan_signal_cells_are_ordered_bar_then_column;
          Alcotest.test_case "invalid price cells are ordered bar-then-column"
            `Quick invalid_price_cells_are_ordered_bar_then_column;
          Alcotest.test_case "err_to_string renders every variant" `Quick
            err_to_string_renders_every_variant;
          Alcotest.test_case "valid inputs are accepted" `Quick
            valid_inputs_are_accepted;
        ] );
    ]
