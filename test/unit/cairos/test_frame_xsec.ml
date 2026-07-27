(* Tests for [Cairos.Frame] cross-sectional operations.

   Single file holding both Alcotest unit cases and
   QCheck properties (wired through [qcheck-alcotest]).

   Covers the [column_map], [rank], and [zscore] cases. *)

let dates_1 = [| "2024-01-01" |]
let dates_2 = [| "2024-01-01"; "2024-01-02" |]
let dates_4 = [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04" |]

let dates_5 =
  [| "2024-01-01"; "2024-01-02"; "2024-01-03"; "2024-01-04"; "2024-01-05" |]

(* NaN-aware tolerance testable. [Alcotest.float tol] returns false on
   NaN-vs-NaN per IEEE 754, which would silently mask all NaN-passthrough
   assertions in [rank]/[zscore]. Branch on [is_nan] for both operands
   first. *)
let nan_float tol =
  let pp ppf x =
    if Float.is_nan x then Format.fprintf ppf "NaN"
    else Format.fprintf ppf "%g" x
  in
  let equal a b =
    match (Float.is_nan a, Float.is_nan b) with
    | true, true -> true
    | true, false
    | false, true ->
        false
    | false, false -> Float.abs (a -. b) <= tol
  in
  Alcotest.testable pp equal

let frame_of_columns = function
  | [] -> Alcotest.fail "frame_of_columns: empty column list"
  | (n, s) :: tl -> (
      match Cairos.Frame.of_series (Cairos.Nonempty.make (n, s) tl) with
      | Error e -> Alcotest.fail e
      | Ok frame -> frame)

(* Read each column's values as a [float array] in [Frame.columns] order.
   Returns a [Nonempty.t] because [Frame.columns] is one, which spares every
   caller below a dead empty-frame branch. The [None] branch is unreachable:
   the name was just produced by [Frame.columns] on the same frame —
   terminate unreachable branches with [failwith] inside QCheck so the
   shrinker reports the true counter-example. *)
let columns_arrays frame =
  Cairos.Nonempty.map
    (fun name ->
      match Cairos.Frame.get name frame with
      | Some s -> Nx.to_array (Cairos.Series.values s)
      | None ->
          failwith
            "unreachable: column name was just read from Frame.columns on the \
             same frame")
    (Cairos.Frame.columns frame)

(* Output series length equals input row count. Catches a
   regression where [column_map] loses or duplicates a row. *)
let column_map_output_length_matches_input () =
  let a =
    Test_helpers.make_daily_series dates_5 [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
  in
  let b =
    Test_helpers.make_daily_series dates_5 [| 6.0; 7.0; 8.0; 9.0; 10.0 |]
  in
  let c =
    Test_helpers.make_daily_series dates_5 [| 11.0; 12.0; 13.0; 14.0; 15.0 |]
  in
  let frame = frame_of_columns [ ("a", a); ("b", b); ("c", c) ] in
  let out = Cairos.Frame.column_map ~f:(fun _ -> 0.0) frame in
  Alcotest.(check int) "output length" 5 (Cairos.Series.length out)

(* Per-column values reach [f] in [columns frame] order at
   each row. The reducer multiplies by the scratch index, so a column-order
   shuffle or off-by-one row indexing produces a different sum. *)
let column_map_passes_per_column_values_in_order () =
  let a = Test_helpers.make_daily_series dates_2 [| 1.0; 2.0 |] in
  let b = Test_helpers.make_daily_series dates_2 [| 10.0; 20.0 |] in
  let c = Test_helpers.make_daily_series dates_2 [| 100.0; 200.0 |] in
  let frame = frame_of_columns [ ("a", a); ("b", b); ("c", c) ] in
  let f xs = xs.(0) +. (xs.(1) *. 2.0) +. (xs.(2) *. 3.0) in
  let out = Cairos.Frame.column_map ~f frame in
  let vs = Nx.to_array (Cairos.Series.values out) in
  Alcotest.(check int) "length" 2 (Array.length vs);
  Alcotest.(check (float 1e-12)) "row 0 = 1 + 10*2 + 100*3" 321.0 vs.(0);
  Alcotest.(check (float 1e-12)) "row 1 = 2 + 20*2 + 200*3" 642.0 vs.(1)

(* Output index timestamps equal the input's element-wise
   under [Ptime.equal] for the series-output op. *)
let column_map_index_identical_to_input () =
  let a = Test_helpers.make_daily_series dates_4 [| 1.0; 2.0; 3.0; 4.0 |] in
  let frame = frame_of_columns [ ("a", a) ] in
  let out = Cairos.Frame.column_map ~f:(fun _ -> 0.0) frame in
  let expected = Cairos.Index.timestamps (Cairos.Series.index a) in
  let actual = Cairos.Index.timestamps (Cairos.Series.index out) in
  Alcotest.(check (array Test_helpers.ptime_testable))
    "timestamps" expected actual

(* Pins the buffer-reuse contract documented at frame.mli:99-100. The scratch
   buffer passed to [f] is the same physical array on every call; capturing
   references and reading them after [column_map] returns surfaces stale
   values from the last row, which is why the docstring warns the caller
   against retaining. Physical equality ([==]) is the minimal-fidelity check
   for this. If a future refactor switches to fresh-per-row allocation,
   update this test together with the
   .mli docstring — they move together. *)
let column_map_buffer_is_reused_across_calls () =
  let a = Test_helpers.make_daily_series dates_4 [| 1.0; 2.0; 3.0; 4.0 |] in
  let b = Test_helpers.make_daily_series dates_4 [| 10.0; 20.0; 30.0; 40.0 |] in
  let frame = frame_of_columns [ ("a", a); ("b", b) ] in
  let captured = ref [] in
  let _ =
    Cairos.Frame.column_map
      ~f:(fun buf ->
        captured := buf :: !captured;
        buf.(0))
      frame
  in
  match !captured with
  | b1 :: b2 :: _ ->
      Alcotest.(check bool) "buffer reused across calls" true (b1 == b2)
  | _ -> Alcotest.fail "expected at least 2 captured buffers"

(* Output series length always equals input row count. The row count is
   recovered from a member column's own data length rather than from
   [Cairos.Frame.index], deliberately: [column_map] sizes its output from
   that same index, so measuring against it would compare the
   implementation with itself. The [None] branch is unreachable — the
   column name was just read from [Frame.columns frame] on the same frame
   — and terminates with [failwith] inside the QCheck property so the
   shrinker reports the true counter-example. *)
let qcheck_column_map_output_length_equals_row_count =
  QCheck.Test.make ~count:200
    ~name:"qcheck_column_map_output_length_equals_row_count"
    Qcheck_gen.daily_frame_distinct_floats_arb (fun frame ->
      let out = Cairos.Frame.column_map ~f:(fun _ -> 0.0) frame in
      let n_in =
        match
          Cairos.Frame.get
            (Cairos.Nonempty.hd (Cairos.Frame.columns frame))
            frame
        with
        | None ->
            failwith
              "unreachable: column name was just read from Frame.columns on \
               the same frame"
        | Some s -> Cairos.Series.length s
      in
      Cairos.Series.length out = n_in)

(* Distinct row values get distinct integer ranks.
   Inputs 30, 10, 20 sort to [10; 20; 30]; the original positions therefore
   rank 3, 1, 2. *)
let rank_simple_distinct_values () =
  let a = Test_helpers.make_daily_series dates_1 [| 30.0 |] in
  let b = Test_helpers.make_daily_series dates_1 [| 10.0 |] in
  let c = Test_helpers.make_daily_series dates_1 [| 20.0 |] in
  let frame = frame_of_columns [ ("a", a); ("b", b); ("c", c) ] in
  let out = Cairos.Frame.rank frame in
  Alcotest.(check (list (array (nan_float 1e-12))))
    "ranks"
    [ [| 3.0 |]; [| 1.0 |]; [| 2.0 |] ]
    (Cairos.Nonempty.to_list (columns_arrays out))

(* Two equal cells share the average of the ranks they
   would otherwise occupy. 10, 20, 20 → 1, 2.5, 2.5 (the run of length 2
   at sorted position 1 spans output ranks 2 and 3; average is 2.5). *)
let rank_average_tie_breaking_two_way () =
  let a = Test_helpers.make_daily_series dates_1 [| 10.0 |] in
  let b = Test_helpers.make_daily_series dates_1 [| 20.0 |] in
  let c = Test_helpers.make_daily_series dates_1 [| 20.0 |] in
  let frame = frame_of_columns [ ("a", a); ("b", b); ("c", c) ] in
  let out = Cairos.Frame.rank frame in
  Alcotest.(check (list (array (nan_float 1e-12))))
    "ranks"
    [ [| 1.0 |]; [| 2.5 |]; [| 2.5 |] ]
    (Cairos.Nonempty.to_list (columns_arrays out))

(* Three equal cells share the average of ranks 1, 2,
   3 = 2.0; the fourth, larger cell takes rank 4. *)
let rank_average_tie_breaking_three_way () =
  let a = Test_helpers.make_daily_series dates_1 [| 5.0 |] in
  let b = Test_helpers.make_daily_series dates_1 [| 5.0 |] in
  let c = Test_helpers.make_daily_series dates_1 [| 5.0 |] in
  let d = Test_helpers.make_daily_series dates_1 [| 10.0 |] in
  let frame = frame_of_columns [ ("a", a); ("b", b); ("c", c); ("d", d) ] in
  let out = Cairos.Frame.rank frame in
  Alcotest.(check (list (array (nan_float 1e-12))))
    "ranks"
    [ [| 2.0 |]; [| 2.0 |]; [| 2.0 |]; [| 4.0 |] ]
    (Cairos.Nonempty.to_list (columns_arrays out))

(* NaN cells stay NaN and are excluded from N. With
   N=3 here, 10, NaN, 20, 30 → 1, NaN, 2, 3. *)
let rank_nan_passthrough_and_excluded_from_n () =
  let a = Test_helpers.make_daily_series dates_1 [| 10.0 |] in
  let b = Test_helpers.make_daily_series dates_1 [| Float.nan |] in
  let c = Test_helpers.make_daily_series dates_1 [| 20.0 |] in
  let d = Test_helpers.make_daily_series dates_1 [| 30.0 |] in
  let frame = frame_of_columns [ ("a", a); ("b", b); ("c", c); ("d", d) ] in
  let out = Cairos.Frame.rank frame in
  Alcotest.(check (list (array (nan_float 1e-12))))
    "ranks"
    [ [| 1.0 |]; [| Float.nan |]; [| 2.0 |]; [| 3.0 |] ]
    (Cairos.Nonempty.to_list (columns_arrays out))

(* Constant row of 4 cells: every cell is part of one
   tie spanning ranks 1..4, so each gets (1+4)/2 = 2.5. *)
let rank_constant_row_uniform_average () =
  let a = Test_helpers.make_daily_series dates_1 [| 5.0 |] in
  let b = Test_helpers.make_daily_series dates_1 [| 5.0 |] in
  let c = Test_helpers.make_daily_series dates_1 [| 5.0 |] in
  let d = Test_helpers.make_daily_series dates_1 [| 5.0 |] in
  let frame = frame_of_columns [ ("a", a); ("b", b); ("c", c); ("d", d) ] in
  let out = Cairos.Frame.rank frame in
  Alcotest.(check (list (array (nan_float 1e-12))))
    "ranks"
    [ [| 2.5 |]; [| 2.5 |]; [| 2.5 |]; [| 2.5 |] ]
    (Cairos.Nonempty.to_list (columns_arrays out))

(* Single-column frame: every non-NaN cell is the only
   member of its row and ranks 1.0; NaN cells stay NaN. *)
let rank_single_column_frame () =
  let a = Test_helpers.make_daily_series dates_2 [| 3.5; Float.nan |] in
  let frame = frame_of_columns [ ("a", a) ] in
  let out = Cairos.Frame.rank frame in
  Alcotest.(check (list (array (nan_float 1e-12))))
    "ranks"
    [ [| 1.0; Float.nan |] ]
    (Cairos.Nonempty.to_list (columns_arrays out))

(* All-NaN row produces an all-NaN output row. *)
let rank_all_nan_row_stays_all_nan () =
  let a = Test_helpers.make_daily_series dates_1 [| Float.nan |] in
  let b = Test_helpers.make_daily_series dates_1 [| Float.nan |] in
  let c = Test_helpers.make_daily_series dates_1 [| Float.nan |] in
  let frame = frame_of_columns [ ("a", a); ("b", b); ("c", c) ] in
  let out = Cairos.Frame.rank frame in
  Alcotest.(check (list (array (nan_float 1e-12))))
    "ranks"
    [ [| Float.nan |]; [| Float.nan |]; [| Float.nan |] ]
    (Cairos.Nonempty.to_list (columns_arrays out))

(* Output frame's index timestamps and column names
   match the input's. Rank-only here; a parallel _zscore case
   covers [zscore]. *)
let output_frame_index_and_columns_identical_to_input () =
  let a = Test_helpers.make_daily_series dates_4 [| 1.0; 2.0; 3.0; 4.0 |] in
  let b = Test_helpers.make_daily_series dates_4 [| 4.0; 3.0; 2.0; 1.0 |] in
  let frame = frame_of_columns [ ("a", a); ("b", b) ] in
  let out = Cairos.Frame.rank frame in
  Alcotest.(check (list string))
    "columns" [ "a"; "b" ]
    (Cairos.Nonempty.to_list (Cairos.Frame.columns out));
  let timestamps_of f name =
    match Cairos.Frame.get name f with
    | None -> Alcotest.fail "unreachable: column was just looked up"
    | Some s -> Cairos.Index.timestamps (Cairos.Series.index s)
  in
  Alcotest.(check (array Test_helpers.ptime_testable))
    "timestamps" (timestamps_of frame "a") (timestamps_of out "a")

(* For a frame with row-wise distinct floats, each
   output row's values form a permutation of {1.0, 2.0, ..., n_cols}. *)
let qcheck_rank_distinct_values_form_permutation =
  QCheck.Test.make ~count:200
    ~name:"qcheck_rank_distinct_values_form_permutation"
    Qcheck_gen.daily_frame_distinct_floats_arb (fun frame ->
      let out = Cairos.Frame.rank frame in
      let cols = columns_arrays out in
      let n_cols = Cairos.Nonempty.length cols in
      let n_rows = Array.length (Cairos.Nonempty.hd cols) in
      let expected = Array.init n_cols (fun j -> Float.of_int (j + 1)) in
      let row_ok i =
        let row =
          Array.of_list
            (List.map (fun a -> a.(i)) (Cairos.Nonempty.to_list cols))
        in
        Array.sort Float.compare row;
        Array.length row = Array.length expected
        && Array.for_all2 Float.equal row expected
      in
      let rec check i = i >= n_rows || (row_ok i && check (i + 1)) in
      check 0)

(* Sum of non-NaN output cells per row equals
   N(N+1)/2, even under arbitrary tie patterns (averages preserve the
   total). N is the count of non-NaN cells in the input row. *)
let qcheck_rank_sum_equals_n_times_n_plus_1_over_2 =
  QCheck.Test.make ~count:200
    ~name:"qcheck_rank_sum_equals_n_times_n_plus_1_over_2"
    Qcheck_gen.daily_frame_finite_floats_with_nan_arb (fun frame ->
      let in_cols = columns_arrays frame in
      let out = Cairos.Frame.rank frame in
      let out_cols = columns_arrays out in
      let n_rows = Array.length (Cairos.Nonempty.hd in_cols) in
      let row_ok i =
        let n_in =
          List.fold_left
            (fun acc a -> if Float.is_nan a.(i) then acc else acc + 1)
            0
            (Cairos.Nonempty.to_list in_cols)
        in
        let sum_out =
          List.fold_left
            (fun acc a -> if Float.is_nan a.(i) then acc else acc +. a.(i))
            0.0
            (Cairos.Nonempty.to_list out_cols)
        in
        let expected = Float.of_int (n_in * (n_in + 1)) /. 2.0 in
        Float.abs (sum_out -. expected) <= 1e-9
      in
      let rec check i = i >= n_rows || (row_ok i && check (i + 1)) in
      check 0)

(* Strict order is preserved on row-wise distinct
   inputs: input[a] < input[b] ⟹ output[a] < output[b]. *)
let qcheck_rank_order_preserving_on_distinct_values =
  QCheck.Test.make ~count:200
    ~name:"qcheck_rank_order_preserving_on_distinct_values"
    Qcheck_gen.daily_frame_distinct_floats_arb (fun frame ->
      let in_cols_ne = columns_arrays frame in
      let in_cols = Array.of_list (Cairos.Nonempty.to_list in_cols_ne) in
      let out = Cairos.Frame.rank frame in
      let out_cols =
        Array.of_list (Cairos.Nonempty.to_list (columns_arrays out))
      in
      let n_cols = Array.length in_cols in
      let n_rows = Array.length (Cairos.Nonempty.hd in_cols_ne) in
      let row_ok i =
        let ok = ref true in
        for ca = 0 to n_cols - 1 do
          for cb = ca + 1 to n_cols - 1 do
            let ia = in_cols.(ca).(i) and ib = in_cols.(cb).(i) in
            let oa = out_cols.(ca).(i) and ob = out_cols.(cb).(i) in
            if Float.compare ia ib < 0 && not (Float.compare oa ob < 0) then
              ok := false;
            if Float.compare ia ib > 0 && not (Float.compare oa ob > 0) then
              ok := false
          done
        done;
        !ok
      in
      let rec check i = i >= n_rows || (row_ok i && check (i + 1)) in
      check 0)

(* N=2 row of distinct values. mean=15, ss=50,
   std=sqrt(50)=5*sqrt(2) (ddof=1), so z=(±5)/(5*sqrt(2))=±1/sqrt(2). *)
let zscore_simple_two_value_row () =
  let a = Test_helpers.make_daily_series dates_1 [| 10.0 |] in
  let b = Test_helpers.make_daily_series dates_1 [| 20.0 |] in
  let frame = frame_of_columns [ ("a", a); ("b", b) ] in
  let out = Cairos.Frame.zscore frame in
  let inv_sqrt2 = 1.0 /. Float.sqrt 2.0 in
  Alcotest.(check (list (array (nan_float 1e-12))))
    "zscore"
    [ [| -.inv_sqrt2 |]; [| inv_sqrt2 |] ]
    (Cairos.Nonempty.to_list (columns_arrays out))

(* Pandas reference for row [1, 2, 3, 4] under
   df.sub(df.mean(axis=1), axis=0).div(df.std(axis=1, ddof=1), axis=0).
   mean=2.5, std=sqrt(5/3); z values inlined as constants per
   CONTRIBUTING §III (do not derive from the implementation). *)
let zscore_ddof1_matches_pandas_reference () =
  let a = Test_helpers.make_daily_series dates_1 [| 1.0 |] in
  let b = Test_helpers.make_daily_series dates_1 [| 2.0 |] in
  let c = Test_helpers.make_daily_series dates_1 [| 3.0 |] in
  let d = Test_helpers.make_daily_series dates_1 [| 4.0 |] in
  let frame = frame_of_columns [ ("a", a); ("b", b); ("c", c); ("d", d) ] in
  let out = Cairos.Frame.zscore frame in
  Alcotest.(check (list (array (nan_float 1e-12))))
    "zscore"
    [
      [| -1.161895003862225 |];
      [| -0.3872983346207417 |];
      [| 0.3872983346207417 |];
      [| 1.161895003862225 |];
    ]
    (Cairos.Nonempty.to_list (columns_arrays out))

(* Constant row: std=0, every output cell is NaN. *)
let zscore_constant_row_is_all_nan () =
  let a = Test_helpers.make_daily_series dates_1 [| 5.0 |] in
  let b = Test_helpers.make_daily_series dates_1 [| 5.0 |] in
  let c = Test_helpers.make_daily_series dates_1 [| 5.0 |] in
  let frame = frame_of_columns [ ("a", a); ("b", b); ("c", c) ] in
  let out = Cairos.Frame.zscore frame in
  Alcotest.(check (list (array (nan_float 1e-12))))
    "zscore"
    [ [| Float.nan |]; [| Float.nan |]; [| Float.nan |] ]
    (Cairos.Nonempty.to_list (columns_arrays out))

(* N=1 row (only one non-NaN cell): output row is
   all NaN, the single non-NaN input cell included. *)
let zscore_single_non_nan_cell_row_is_all_nan () =
  let a = Test_helpers.make_daily_series dates_1 [| 7.0 |] in
  let b = Test_helpers.make_daily_series dates_1 [| Float.nan |] in
  let c = Test_helpers.make_daily_series dates_1 [| Float.nan |] in
  let frame = frame_of_columns [ ("a", a); ("b", b); ("c", c) ] in
  let out = Cairos.Frame.zscore frame in
  Alcotest.(check (list (array (nan_float 1e-12))))
    "zscore"
    [ [| Float.nan |]; [| Float.nan |]; [| Float.nan |] ]
    (Cairos.Nonempty.to_list (columns_arrays out))

(* Single-column frame: every row has N=1, every
   output cell is NaN regardless of input value. *)
let zscore_single_column_frame_is_all_nan () =
  let a = Test_helpers.make_daily_series dates_2 [| 3.5; 7.0 |] in
  let frame = frame_of_columns [ ("a", a) ] in
  let out = Cairos.Frame.zscore frame in
  Alcotest.(check (list (array (nan_float 1e-12))))
    "zscore"
    [ [| Float.nan; Float.nan |] ]
    (Cairos.Nonempty.to_list (columns_arrays out))

(* NaN passes through; non-NaN cells are normalised
   over N=3. Row [NaN, 10, 20, 30]: mean=20, ss=200, std=sqrt(200/2)=10,
   so z=[NaN, -1, 0, 1]. *)
let zscore_nan_passthrough_with_partial_row () =
  let a = Test_helpers.make_daily_series dates_1 [| Float.nan |] in
  let b = Test_helpers.make_daily_series dates_1 [| 10.0 |] in
  let c = Test_helpers.make_daily_series dates_1 [| 20.0 |] in
  let d = Test_helpers.make_daily_series dates_1 [| 30.0 |] in
  let frame = frame_of_columns [ ("a", a); ("b", b); ("c", c); ("d", d) ] in
  let out = Cairos.Frame.zscore frame in
  Alcotest.(check (list (array (nan_float 1e-12))))
    "zscore"
    [ [| Float.nan |]; [| -1.0 |]; [| 0.0 |]; [| 1.0 |] ]
    (Cairos.Nonempty.to_list (columns_arrays out))

(* Parallel to the rank index-identity case, this time on [zscore]
   output. Keeps each case green by
   isolating the [rank] and [zscore] index-identity assertions. *)
let output_frame_index_and_columns_identical_to_input_zscore () =
  let a = Test_helpers.make_daily_series dates_4 [| 1.0; 2.0; 3.0; 4.0 |] in
  let b = Test_helpers.make_daily_series dates_4 [| 4.0; 3.0; 2.0; 1.0 |] in
  let frame = frame_of_columns [ ("a", a); ("b", b) ] in
  let out = Cairos.Frame.zscore frame in
  Alcotest.(check (list string))
    "columns" [ "a"; "b" ]
    (Cairos.Nonempty.to_list (Cairos.Frame.columns out));
  let timestamps_of f name =
    match Cairos.Frame.get name f with
    | None -> Alcotest.fail "unreachable: column was just looked up"
    | Some s -> Cairos.Index.timestamps (Cairos.Series.index s)
  in
  Alcotest.(check (array Test_helpers.ptime_testable))
    "timestamps" (timestamps_of frame "a") (timestamps_of out "a")

(* For every row of a well-conditioned frame, the mean
   of non-NaN output cells is within 1e-10 of 0. The
   well-conditioned arb guarantees ≥2 distinct non-NaN cells per row, so
   no row collapses to all-NaN. *)
let qcheck_zscore_output_mean_is_zero =
  QCheck.Test.make ~count:200 ~name:"qcheck_zscore_output_mean_is_zero"
    Qcheck_gen.daily_frame_zscore_well_conditioned_arb (fun frame ->
      let out = Cairos.Frame.zscore frame in
      let out_cols = columns_arrays out in
      let n_rows = Array.length (Cairos.Nonempty.hd out_cols) in
      let row_ok i =
        let n, sum =
          List.fold_left
            (fun (n, s) a ->
              if Float.is_nan a.(i) then (n, s) else (n + 1, s +. a.(i)))
            (0, 0.0)
            (Cairos.Nonempty.to_list out_cols)
        in
        if n = 0 then false
        else
          let mean = sum /. Float.of_int n in
          Float.abs mean <= 1e-10
      in
      let rec check i = i >= n_rows || (row_ok i && check (i + 1)) in
      check 0)

(* For every row of a well-conditioned frame, the
   sample (ddof=1) std of non-NaN output cells is within 1e-10 of 1.
   N≥2 by the well-conditioned arb's invariant, so the
   ddof=1 denominator is well-defined. *)
let qcheck_zscore_output_std_is_one =
  QCheck.Test.make ~count:200 ~name:"qcheck_zscore_output_std_is_one"
    Qcheck_gen.daily_frame_zscore_well_conditioned_arb (fun frame ->
      let out = Cairos.Frame.zscore frame in
      let out_cols = columns_arrays out in
      let n_rows = Array.length (Cairos.Nonempty.hd out_cols) in
      let row_ok i =
        let n, sum =
          List.fold_left
            (fun (n, s) a ->
              if Float.is_nan a.(i) then (n, s) else (n + 1, s +. a.(i)))
            (0, 0.0)
            (Cairos.Nonempty.to_list out_cols)
        in
        if n < 2 then false
        else
          let mean = sum /. Float.of_int n in
          let ss =
            List.fold_left
              (fun acc a ->
                if Float.is_nan a.(i) then acc
                else acc +. ((a.(i) -. mean) *. (a.(i) -. mean)))
              0.0
              (Cairos.Nonempty.to_list out_cols)
          in
          let std = Float.sqrt (ss /. Float.of_int (n - 1)) in
          Float.abs (std -. 1.0) <= 1e-10
      in
      let rec check i = i >= n_rows || (row_ok i && check (i + 1)) in
      check 0)

let () =
  Qcheck_gen.pin_seed_from_env ();
  Alcotest.run "Frame.xsec"
    [
      ( "column_map",
        [
          Alcotest.test_case "output_length_matches_input" `Quick
            column_map_output_length_matches_input;
          Alcotest.test_case "passes_per_column_values_in_order" `Quick
            column_map_passes_per_column_values_in_order;
          Alcotest.test_case "index_identical_to_input" `Quick
            column_map_index_identical_to_input;
          Alcotest.test_case "buffer_is_reused_across_calls" `Quick
            column_map_buffer_is_reused_across_calls;
          QCheck_alcotest.to_alcotest
            qcheck_column_map_output_length_equals_row_count;
        ] );
      ( "rank",
        [
          Alcotest.test_case "simple_distinct_values" `Quick
            rank_simple_distinct_values;
          Alcotest.test_case "average_tie_breaking_two_way" `Quick
            rank_average_tie_breaking_two_way;
          Alcotest.test_case "average_tie_breaking_three_way" `Quick
            rank_average_tie_breaking_three_way;
          Alcotest.test_case "nan_passthrough_and_excluded_from_n" `Quick
            rank_nan_passthrough_and_excluded_from_n;
          Alcotest.test_case "constant_row_uniform_average" `Quick
            rank_constant_row_uniform_average;
          Alcotest.test_case "single_column_frame" `Quick
            rank_single_column_frame;
          Alcotest.test_case "all_nan_row_stays_all_nan" `Quick
            rank_all_nan_row_stays_all_nan;
          Alcotest.test_case "output_frame_index_and_columns_identical_to_input"
            `Quick output_frame_index_and_columns_identical_to_input;
          QCheck_alcotest.to_alcotest
            qcheck_rank_distinct_values_form_permutation;
          QCheck_alcotest.to_alcotest
            qcheck_rank_sum_equals_n_times_n_plus_1_over_2;
          QCheck_alcotest.to_alcotest
            qcheck_rank_order_preserving_on_distinct_values;
        ] );
      ( "zscore",
        [
          Alcotest.test_case "simple_two_value_row" `Quick
            zscore_simple_two_value_row;
          Alcotest.test_case "ddof1_matches_pandas_reference" `Quick
            zscore_ddof1_matches_pandas_reference;
          Alcotest.test_case "constant_row_is_all_nan" `Quick
            zscore_constant_row_is_all_nan;
          Alcotest.test_case "single_non_nan_cell_row_is_all_nan" `Quick
            zscore_single_non_nan_cell_row_is_all_nan;
          Alcotest.test_case "single_column_frame_is_all_nan" `Quick
            zscore_single_column_frame_is_all_nan;
          Alcotest.test_case "nan_passthrough_with_partial_row" `Quick
            zscore_nan_passthrough_with_partial_row;
          Alcotest.test_case
            "output_frame_index_and_columns_identical_to_input_zscore" `Quick
            output_frame_index_and_columns_identical_to_input_zscore;
          QCheck_alcotest.to_alcotest qcheck_zscore_output_mean_is_zero;
          QCheck_alcotest.to_alcotest qcheck_zscore_output_std_is_one;
        ] );
    ]
