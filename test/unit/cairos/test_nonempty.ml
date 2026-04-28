open Cairos

let of_list_empty_is_none () =
  Alcotest.(check bool)
    "of_list [] = None" true
    (Option.is_none (Nonempty.of_list []))

let () =
  Alcotest.run "Nonempty"
    [
      ( "deterministic",
        [ Alcotest.test_case "of_list [] is None" `Quick of_list_empty_is_none ]
      );
    ]
