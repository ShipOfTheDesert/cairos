open Cairos

let of_list_empty_is_none () =
  Alcotest.(check bool)
    "of_list [] = None" true
    (Option.is_none (Nonempty.of_list []))

let nonempty_map_preserves_length_and_order () =
  let ne = Nonempty.make 1 [ 2; 3; 4 ] in
  let mapped = Nonempty.map (fun x -> x * 10) ne in
  Alcotest.(check int) "length" 4 (Nonempty.length mapped);
  Alcotest.(check (list int))
    "elements in order" [ 10; 20; 30; 40 ] (Nonempty.to_list mapped)

let () =
  Alcotest.run "Nonempty"
    [
      ( "deterministic",
        [
          Alcotest.test_case "of_list [] is None" `Quick of_list_empty_is_none;
          Alcotest.test_case "map preserves length and order" `Quick
            nonempty_map_preserves_length_and_order;
        ] );
    ]
