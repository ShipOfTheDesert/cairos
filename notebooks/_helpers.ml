(* Load from a notebook cell via: #use "_helpers.ml" *)

let unwrap what = function
  | Ok v -> v
  | Error e -> failwith (Printf.sprintf "%s: %s" what e)

let product_1p window =
  let arr = Nx.to_array window in
  Array.fold_left (fun acc r -> acc *. (1.0 +. r)) 1.0 arr
