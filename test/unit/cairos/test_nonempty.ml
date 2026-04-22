open Cairos

let int_list_gen = QCheck.(list nat_small)
let int_nonempty_gen = QCheck.(pair nat_small int_list_gen)

let to_list_after_make =
  QCheck.Test.make ~count:200 ~name:"to_list (make x xs) = x :: xs"
    int_nonempty_gen (fun (x, xs) ->
      Nonempty.to_list (Nonempty.make x xs) = x :: xs)

let length_of_singleton =
  QCheck.Test.make ~count:50 ~name:"length (singleton x) = 1" QCheck.nat_small
    (fun x -> Nonempty.length (Nonempty.singleton x) = 1)

let hd_of_make =
  QCheck.Test.make ~count:200 ~name:"hd (make x xs) = x" int_nonempty_gen
    (fun (x, xs) -> Nonempty.hd (Nonempty.make x xs) = x)

let map_preserves_length =
  QCheck.Test.make ~count:200 ~name:"map preserves length" int_nonempty_gen
    (fun (x, xs) ->
      let ne = Nonempty.make x xs in
      Nonempty.length (Nonempty.map (fun v -> v + 1) ne) = Nonempty.length ne)

let of_list_empty_is_none () =
  Alcotest.(check bool)
    "of_list [] = None" true
    (Option.is_none (Nonempty.of_list []))

let of_list_non_empty_is_some =
  QCheck.Test.make ~count:200 ~name:"of_list (x :: xs) = Some" int_nonempty_gen
    (fun (x, xs) ->
      match Nonempty.of_list (x :: xs) with
      | Some ne -> Nonempty.to_list ne = x :: xs
      | None -> false)

let length_of_make =
  QCheck.Test.make ~count:200 ~name:"length (make x xs) = 1 + List.length xs"
    int_nonempty_gen (fun (x, xs) ->
      Nonempty.length (Nonempty.make x xs) = 1 + List.length xs)

let fold_left_accumulates_all =
  QCheck.Test.make ~count:200 ~name:"fold_left sums all elements"
    int_nonempty_gen (fun (x, xs) ->
      let ne = Nonempty.make x xs in
      Nonempty.fold_left ( + ) 0 ne = List.fold_left ( + ) 0 (x :: xs))

let append_extends_tail =
  QCheck.Test.make ~count:200 ~name:"to_list (append ne ys) = to_list ne @ ys"
    QCheck.(triple nat_small int_list_gen int_list_gen)
    (fun (x, xs, ys) ->
      let ne = Nonempty.make x xs in
      Nonempty.to_list (Nonempty.append ne ys) = Nonempty.to_list ne @ ys)

let qcheck_tests =
  List.map QCheck_alcotest.to_alcotest
    [
      to_list_after_make;
      length_of_singleton;
      hd_of_make;
      map_preserves_length;
      of_list_non_empty_is_some;
      length_of_make;
      fold_left_accumulates_all;
      append_extends_tail;
    ]

let deterministic_tests =
  [ Alcotest.test_case "of_list [] is None" `Quick of_list_empty_is_none ]

let () =
  Alcotest.run "Nonempty"
    [ ("property", qcheck_tests); ("deterministic", deterministic_tests) ]
