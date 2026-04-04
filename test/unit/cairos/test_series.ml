let make_succeeds_with_matching_lengths () =
  match Cairos.Index.daily [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] with
  | Error e -> Alcotest.fail e
  | Ok idx -> (
      let values = Nx.create Nx.float64 [| 3 |] [| 10.0; 20.0; 30.0 |] in
      match Cairos.Series.make idx values with
      | Error e -> Alcotest.fail e
      | Ok s -> Alcotest.(check int) "length is 3" 3 (Cairos.Series.length s))

let make_fails_with_mismatched_lengths () =
  match Cairos.Index.daily [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] with
  | Error e -> Alcotest.fail e
  | Ok idx -> (
      let values = Nx.create Nx.float64 [| 5 |] [| 1.0; 2.0; 3.0; 4.0; 5.0 |] in
      match Cairos.Series.make idx values with
      | Ok _ -> Alcotest.fail "expected Error for mismatched lengths"
      | Error _ -> ())

let slice_produces_correct_subseries () =
  match
    Cairos.Index.daily
      [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |]
  with
  | Error e -> Alcotest.fail e
  | Ok idx -> (
      let values = Nx.create Nx.float64 [| 4 |] [| 10.0; 20.0; 30.0; 40.0 |] in
      match Cairos.Series.make idx values with
      | Error e -> Alcotest.fail e
      | Ok s ->
          let sliced = Cairos.Series.slice ~start:1 ~stop:3 s in
          Alcotest.(check int) "sliced length" 2 (Cairos.Series.length sliced);
          let vs = Nx.to_array (Cairos.Series.values sliced) in
          Alcotest.(check (float 0.001)) "value at 0" 20.0 vs.(0);
          Alcotest.(check (float 0.001)) "value at 1" 30.0 vs.(1))

let map_transforms_values_and_preserves_index () =
  match Cairos.Index.daily [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] with
  | Error e -> Alcotest.fail e
  | Ok idx -> (
      let values = Nx.create Nx.float64 [| 3 |] [| 1.0; 2.0; 3.0 |] in
      match Cairos.Series.make idx values with
      | Error e -> Alcotest.fail e
      | Ok s ->
          let doubled = Cairos.Series.map (fun v -> Nx.mul_s v 2.0) s in
          let vs = Nx.to_array (Cairos.Series.values doubled) in
          Alcotest.(check (float 0.001)) "doubled 0" 2.0 vs.(0);
          Alcotest.(check (float 0.001)) "doubled 1" 4.0 vs.(1);
          Alcotest.(check (float 0.001)) "doubled 2" 6.0 vs.(2);
          let ptime_testable =
            Alcotest.testable
              (fun ppf t -> Fmt.pf ppf "%a" (Ptime.pp_rfc3339 ()) t)
              Ptime.equal
          in
          let orig_ts = Cairos.Index.timestamps (Cairos.Series.index s) in
          let mapped_ts =
            Cairos.Index.timestamps (Cairos.Series.index doubled)
          in
          Alcotest.(check ptime_testable) "ts 0" orig_ts.(0) mapped_ts.(0);
          Alcotest.(check ptime_testable) "ts 1" orig_ts.(1) mapped_ts.(1);
          Alcotest.(check ptime_testable) "ts 2" orig_ts.(2) mapped_ts.(2))

let empty_series_succeeds () =
  match Cairos.Index.daily [||] with
  | Error e -> Alcotest.fail e
  | Ok idx -> (
      let values = Nx.create Nx.float64 [| 0 |] [||] in
      match Cairos.Series.make idx values with
      | Error e -> Alcotest.fail e
      | Ok s -> Alcotest.(check int) "length is 0" 0 (Cairos.Series.length s))

let slice_empty_range_produces_empty_series () =
  match Cairos.Index.daily [| "2024-01-01"; "2024-01-02"; "2024-01-03" |] with
  | Error e -> Alcotest.fail e
  | Ok idx -> (
      let values = Nx.create Nx.float64 [| 3 |] [| 1.0; 2.0; 3.0 |] in
      match Cairos.Series.make idx values with
      | Error e -> Alcotest.fail e
      | Ok s ->
          let equal_bounds = Cairos.Series.slice ~start:1 ~stop:1 s in
          Alcotest.(check int)
            "start == stop" 0
            (Cairos.Series.length equal_bounds);
          Alcotest.(check int)
            "tensor shape is 0" 0
            (Nx.shape (Cairos.Series.values equal_bounds)).(0);
          let reversed = Cairos.Series.slice ~start:2 ~stop:1 s in
          Alcotest.(check int) "start > stop" 0 (Cairos.Series.length reversed))

let tests =
  [
    ( "make_succeeds_with_matching_lengths",
      `Quick,
      make_succeeds_with_matching_lengths );
    ( "make_fails_with_mismatched_lengths",
      `Quick,
      make_fails_with_mismatched_lengths );
    ( "slice_produces_correct_subseries",
      `Quick,
      slice_produces_correct_subseries );
    ( "map_transforms_values_and_preserves_index",
      `Quick,
      map_transforms_values_and_preserves_index );
    ("empty_series_succeeds", `Quick, empty_series_succeeds);
    ( "slice_empty_range_produces_empty_series",
      `Quick,
      slice_empty_range_produces_empty_series );
  ]
