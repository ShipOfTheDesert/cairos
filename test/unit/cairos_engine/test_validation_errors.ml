(* Layer 1 — entrypoint validation negative-path tests for
   [Cairos_engine.Backtest.run].

   Each test constructs inputs that violate exactly one of the seven
   preconditions enumerated in the entrypoint validation order
   (mirrored in [lib/cairos_engine/cairos_engine.ml] [validate_inputs])
   and asserts [Backtest.run] returns [Error] with a message identifying
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

let contains_substring haystack needle =
  let nh = String.length haystack in
  let nn = String.length needle in
  if nn = 0 then true
  else
    let rec loop i =
      if i + nn > nh then false
      else if String.sub haystack i nn = needle then true
      else loop (i + 1)
    in
    loop 0

let assert_error_with_substring ~substr result =
  match result with
  | Ok _ -> Alcotest.fail "expected Error, got Ok"
  | Error msg ->
      if not (contains_substring msg substr) then
        Alcotest.fail
          (Printf.sprintf "error message %S does not contain %S" msg substr)

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
  assert_error_with_substring ~substr:"different indices" result

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
  assert_error_with_substring ~substr:"different columns" result

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
  assert_error_with_substring ~substr:"rebalance index is empty" result

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
  assert_error_with_substring ~substr:"precedes price frame's first bar" result

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
  assert_error_with_substring ~substr:"does not match any price-frame row"
    result

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
  assert_error_with_substring ~substr:"no T+1 open available" result

(* Step 7 — every rebalance has all-zero target weights. *)
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
  assert_error_with_substring
    ~substr:"no non-zero target weight at any rebalance date" result

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
          Alcotest.test_case "step 7 — all-zero target weights" `Quick
            all_zero_target_weights;
        ] );
    ]
