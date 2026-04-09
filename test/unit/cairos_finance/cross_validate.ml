(* Cross-validation infrastructure for cairos_finance.
   Tests CSV parsing and array comparison utilities. *)

let read_fixture path : (float array, string) result =
  match In_channel.with_open_text path In_channel.input_all with
  | exception Sys_error msg -> Error msg
  | content ->
      let lines =
        String.split_on_char '\n' content
        |> List.filter (fun s -> String.length (String.trim s) > 0)
      in
      let rec parse acc line_num = function
        | [] -> Ok (Array.of_list (List.rev acc))
        | line :: rest -> (
            match float_of_string_opt (String.trim line) with
            | Some v -> parse (v :: acc) (line_num + 1) rest
            | None ->
                Error
                  (Printf.sprintf "failed to parse line %d: %S" line_num
                     (String.trim line)))
      in
      parse [] 1 lines

(* write_computed is used by the cross-validation harness, not by unit tests *)
let[@warning "-32"] write_computed path (arr : float array) : unit =
  Out_channel.with_open_text path (fun oc ->
      Array.iter (fun v -> Printf.fprintf oc "%.17g\n" v) arr)

let compare_arrays ~tolerance ~expected ~actual : (unit, string) result =
  let elen = Array.length expected in
  let alen = Array.length actual in
  if elen <> alen then
    Error (Printf.sprintf "length mismatch: expected %d, got %d" elen alen)
  else
    let rec check i =
      if i >= elen then Ok ()
      else
        let diff = Float.abs (expected.(i) -. actual.(i)) in
        if diff > tolerance then
          Error
            (Printf.sprintf
               "mismatch at index %d: expected %.17g, got %.17g (diff %.17g)" i
               expected.(i) actual.(i) diff)
        else check (i + 1)
    in
    check 0

(* --- Tests --- *)

let string_contains haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if not !found then
        if String.sub haystack i nlen = needle then found := true
    done;
    !found

let read_fixture_parses_valid_csv () =
  let tmp = Filename.temp_file "cairos_cv_test_" ".csv" in
  Fun.protect
    ~finally:(fun () -> Sys.remove tmp)
    (fun () ->
      Out_channel.with_open_text tmp (fun oc ->
          List.iter
            (fun v -> Printf.fprintf oc "%.17g\n" v)
            [ 1.5; 2.0; 3.14159 ]);
      match read_fixture tmp with
      | Error e -> Alcotest.fail ("unexpected error: " ^ e)
      | Ok arr ->
          Alcotest.(check int) "length" 3 (Array.length arr);
          Alcotest.(check (float 0.0)) "first" 1.5 arr.(0);
          Alcotest.(check (float 0.0)) "second" 2.0 arr.(1);
          Alcotest.(check (float 1e-15)) "third" 3.14159 arr.(2))

let read_fixture_returns_error_on_malformed_line () =
  let tmp = Filename.temp_file "cairos_cv_test_" ".csv" in
  Fun.protect
    ~finally:(fun () -> Sys.remove tmp)
    (fun () ->
      Out_channel.with_open_text tmp (fun oc ->
          Printf.fprintf oc "1.0\nnot_a_number\n3.0\n");
      match read_fixture tmp with
      | Ok _ -> Alcotest.fail "expected Error for malformed CSV"
      | Error msg ->
          Alcotest.(check bool)
            "error mentions line number" true
            (string_contains msg "line 2"))

let compare_arrays_passes_within_tolerance () =
  let expected = [| 1.0; 2.0; 3.0 |] in
  let actual = [| 1.0 +. 1e-12; 2.0 -. 1e-12; 3.0 +. 1e-11 |] in
  match compare_arrays ~tolerance:1e-10 ~expected ~actual with
  | Ok () -> ()
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)

let compare_arrays_fails_on_divergence () =
  let expected = [| 1.0; 2.0; 3.0 |] in
  let actual = [| 1.0; 2.5; 3.0 |] in
  match compare_arrays ~tolerance:1e-10 ~expected ~actual with
  | Ok () -> Alcotest.fail "expected Error for divergent arrays"
  | Error msg ->
      Alcotest.(check bool)
        "error mentions index" true
        (string_contains msg "index 1")

let compare_arrays_fails_on_length_mismatch () =
  let expected = [| 1.0; 2.0; 3.0 |] in
  let actual = [| 1.0; 2.0 |] in
  match compare_arrays ~tolerance:1e-10 ~expected ~actual with
  | Ok () -> Alcotest.fail "expected Error for length mismatch"
  | Error msg ->
      Alcotest.(check bool)
        "error mentions expected length" true (string_contains msg "3")

let () =
  Alcotest.run "cross_validate"
    [
      ( "read_fixture",
        [
          Alcotest.test_case "parses valid CSV" `Quick
            read_fixture_parses_valid_csv;
          Alcotest.test_case "returns error on malformed line" `Quick
            read_fixture_returns_error_on_malformed_line;
        ] );
      ( "compare_arrays",
        [
          Alcotest.test_case "passes within tolerance" `Quick
            compare_arrays_passes_within_tolerance;
          Alcotest.test_case "fails on divergence" `Quick
            compare_arrays_fails_on_divergence;
          Alcotest.test_case "fails on length mismatch" `Quick
            compare_arrays_fails_on_length_mismatch;
        ] );
    ]
