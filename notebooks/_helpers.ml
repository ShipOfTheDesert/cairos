(* Load from a notebook cell via: #use "_helpers.ml" *)

let unwrap what = function
  | Ok v -> v
  | Error e -> failwith (Printf.sprintf "%s: %s" what e)
