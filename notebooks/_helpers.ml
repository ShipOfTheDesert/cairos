(* Load from a notebook cell via: #use "_helpers.ml" *)

(* [~render] is the error module's own [err_to_string]: every fallible Cairos
   entry point returns a closed variant, so the renderer differs per call. *)
let unwrap ~render what = function
  | Ok v -> v
  | Error e -> failwith (Printf.sprintf "%s: %s" what (render e))
