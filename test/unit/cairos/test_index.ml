let ptime_testable =
  Alcotest.testable
    (fun ppf t -> Fmt.pf ppf "%a" (Ptime.pp_rfc3339 ()) t)
    Ptime.equal

let daily_parses_valid_iso8601 () =
  let result = Cairos.Index.daily [| "2024-01-15"; "2024-01-16" |] in
  match result with
  | Error e -> Alcotest.fail e
  | Ok idx ->
      Alcotest.(check int) "length is 2" 2 (Cairos.Index.length idx);
      let ts = Cairos.Index.timestamps idx in
      let expected_0 =
        match Ptime.of_rfc3339 "2024-01-15T00:00:00Z" with
        | Ok (t, _, _) -> t
        | Error _ -> Alcotest.fail "test setup: bad date"
      in
      let expected_1 =
        match Ptime.of_rfc3339 "2024-01-16T00:00:00Z" with
        | Ok (t, _, _) -> t
        | Error _ -> Alcotest.fail "test setup: bad date"
      in
      Alcotest.(check ptime_testable) "first timestamp" expected_0 ts.(0);
      Alcotest.(check ptime_testable) "second timestamp" expected_1 ts.(1)

let smart_constructor_rejects_malformed_input () =
  let result = Cairos.Index.daily [| "2024-01-15"; "not-a-date" |] in
  match result with
  | Ok _ -> Alcotest.fail "expected Error for malformed input"
  | Error msg ->
      Alcotest.(check string)
        "error message" "invalid timestamp at position 1: not-a-date" msg

let all_four_constructors_accept_valid_input () =
  let ts = "2024-06-03T10:30:00Z" in
  let check_ok name result =
    match result with
    | Ok _ -> ()
    | Error e -> Alcotest.fail (name ^ ": " ^ e)
  in
  check_ok "daily" (Cairos.Index.daily [| ts |]);
  check_ok "minute" (Cairos.Index.minute [| ts |]);
  check_ok "hourly" (Cairos.Index.hourly [| ts |]);
  check_ok "weekly" (Cairos.Index.weekly [| ts |])

let of_unix_floats_produces_correct_timestamps () =
  let f1 = 1_700_000_000.0 in
  let f2 = 1_700_086_400.0 in
  match Cairos.Index.of_unix_floats Cairos.Freq.Day [| f1; f2 |] with
  | Error e -> Alcotest.fail e
  | Ok idx ->
      Alcotest.(check int) "length is 2" 2 (Cairos.Index.length idx);
      let ts = Cairos.Index.timestamps idx in
      let expected_0 =
        match Ptime.of_float_s f1 with
        | Some t -> t
        | None -> Alcotest.fail "test setup"
      in
      let expected_1 =
        match Ptime.of_float_s f2 with
        | Some t -> t
        | None -> Alcotest.fail "test setup"
      in
      Alcotest.(check ptime_testable) "first timestamp" expected_0 ts.(0);
      Alcotest.(check ptime_testable) "second timestamp" expected_1 ts.(1)

let slice_extracts_subrange () =
  match
    Cairos.Index.daily
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]
  with
  | Error e -> Alcotest.fail e
  | Ok idx ->
      let sliced = Cairos.Index.slice ~start:1 ~stop:3 idx in
      Alcotest.(check int) "sliced length" 2 (Cairos.Index.length sliced);
      let ts = Cairos.Index.timestamps sliced in
      let expected_1 =
        match Ptime.of_rfc3339 "2024-01-02T00:00:00Z" with
        | Ok (t, _, _) -> t
        | Error _ -> Alcotest.fail "test setup"
      in
      let expected_2 =
        match Ptime.of_rfc3339 "2024-01-03T00:00:00Z" with
        | Ok (t, _, _) -> t
        | Error _ -> Alcotest.fail "test setup"
      in
      Alcotest.(check ptime_testable) "position 0" expected_1 ts.(0);
      Alcotest.(check ptime_testable) "position 1" expected_2 ts.(1)

let slice_clamps_out_of_bounds () =
  match Cairos.Index.daily [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] with
  | Error e -> Alcotest.fail e
  | Ok idx ->
      let sliced = Cairos.Index.slice ~start:(-1) ~stop:100 idx in
      Alcotest.(check int) "clamped length" 3 (Cairos.Index.length sliced)

let empty_array_produces_empty_index () =
  match Cairos.Index.daily [||] with
  | Error e -> Alcotest.fail e
  | Ok idx -> Alcotest.(check int) "length is 0" 0 (Cairos.Index.length idx)

let of_unix_floats_rejects_invalid_values () =
  (match
     Cairos.Index.of_unix_floats Cairos.Freq.Day
       [| 1_700_000_000.0; Float.nan |]
   with
  | Ok _ -> Alcotest.fail "expected Error for NaN"
  | Error msg ->
      Alcotest.(check string)
        "rejects nan" "invalid unix timestamp at position 1: nan" msg);
  match Cairos.Index.of_unix_floats Cairos.Freq.Day [| Float.infinity |] with
  | Ok _ -> Alcotest.fail "expected Error for infinity"
  | Error msg ->
      Alcotest.(check string)
        "rejects infinity" "invalid unix timestamp at position 0: inf" msg

let slice_start_geq_stop_produces_empty () =
  match Cairos.Index.daily [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] with
  | Error e -> Alcotest.fail e
  | Ok idx ->
      let sliced = Cairos.Index.slice ~start:2 ~stop:1 idx in
      Alcotest.(check int)
        "empty when start >= stop" 0
        (Cairos.Index.length sliced)

let timestamps_returns_defensive_copy () =
  match Cairos.Index.daily [| "2024-01-01"; "2024-01-02" |] with
  | Error e -> Alcotest.fail e
  | Ok idx ->
      let ts = Cairos.Index.timestamps idx in
      ts.(0) <- Ptime.epoch;
      let ts2 = Cairos.Index.timestamps idx in
      let expected =
        match Ptime.of_rfc3339 "2024-01-01T00:00:00Z" with
        | Ok (t, _, _) -> t
        | Error _ -> Alcotest.fail "test setup"
      in
      Alcotest.(check ptime_testable)
        "mutation did not affect index" expected ts2.(0)

let rejects_non_monotonic_or_duplicate_timestamps () =
  (match Cairos.Index.daily [| "2024-01-03"; "2024-01-02"; "2024-01-01" |] with
  | Ok _ -> Alcotest.fail "expected Error for non-monotonic timestamps"
  | Error msg ->
      Alcotest.(check string)
        "descending" "timestamps not strictly monotonic at position 1" msg);
  match Cairos.Index.daily [| "2024-01-01"; "2024-01-01" |] with
  | Ok _ -> Alcotest.fail "expected Error for duplicate timestamps"
  | Error msg ->
      Alcotest.(check string)
        "duplicate" "timestamps not strictly monotonic at position 1" msg

let of_unix_floats_rejects_non_monotonic () =
  match
    Cairos.Index.of_unix_floats Cairos.Freq.Day
      [| 1_700_086_400.0; 1_700_000_000.0 |]
  with
  | Ok _ -> Alcotest.fail "expected Error for non-monotonic floats"
  | Error msg ->
      Alcotest.(check string)
        "error message" "timestamps not strictly monotonic at position 1" msg

let of_unix_floats_empty_array () =
  match Cairos.Index.of_unix_floats Cairos.Freq.Day [||] with
  | Error e -> Alcotest.fail e
  | Ok idx -> Alcotest.(check int) "length is 0" 0 (Cairos.Index.length idx)

let tests =
  [
    ("daily_parses_valid_iso8601", `Quick, daily_parses_valid_iso8601);
    ( "smart_constructor_rejects_malformed_input",
      `Quick,
      smart_constructor_rejects_malformed_input );
    ( "all_four_constructors_accept_valid_input",
      `Quick,
      all_four_constructors_accept_valid_input );
    ( "of_unix_floats_produces_correct_timestamps",
      `Quick,
      of_unix_floats_produces_correct_timestamps );
    ("slice_extracts_subrange", `Quick, slice_extracts_subrange);
    ("slice_clamps_out_of_bounds", `Quick, slice_clamps_out_of_bounds);
    ( "empty_array_produces_empty_index",
      `Quick,
      empty_array_produces_empty_index );
    ( "of_unix_floats_rejects_invalid_values",
      `Quick,
      of_unix_floats_rejects_invalid_values );
    ( "slice_start_geq_stop_produces_empty",
      `Quick,
      slice_start_geq_stop_produces_empty );
    ( "timestamps_returns_defensive_copy",
      `Quick,
      timestamps_returns_defensive_copy );
    ("of_unix_floats_empty_array", `Quick, of_unix_floats_empty_array);
    ( "rejects_non_monotonic_or_duplicate_timestamps",
      `Quick,
      rejects_non_monotonic_or_duplicate_timestamps );
    ( "of_unix_floats_rejects_non_monotonic",
      `Quick,
      of_unix_floats_rejects_non_monotonic );
  ]
