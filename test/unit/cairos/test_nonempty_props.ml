(* Property suite for [Cairos.Nonempty]. Lifted from [test_nonempty.ml] so the
   QCheck cases follow the [test_<module>_props.ml] convention established by
   RFC 0046; the deterministic Alcotest case stays in [test_nonempty.ml].

   Properties run at [~count:200] (one at [~count:50]) per the original
   [test_nonempty.ml] spelling; CI/local reproducibility comes from
   [Qcheck_gen.pin_seed_from_env] (FR-4). *)

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

let tl_of_make =
  QCheck.Test.make ~count:200 ~name:"tl (make x xs) = xs" int_nonempty_gen
    (fun (x, xs) -> Nonempty.tl (Nonempty.make x xs) = xs)

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

let () =
  Qcheck_gen.pin_seed_from_env ();
  let tests =
    List.map QCheck_alcotest.to_alcotest
      [
        to_list_after_make;
        length_of_singleton;
        hd_of_make;
        tl_of_make;
        of_list_non_empty_is_some;
        length_of_make;
      ]
  in
  Alcotest.run "Nonempty.props" [ ("property", tests) ]
