let epoch = 1_704_067_200.0
(* 2024-01-01 UTC *)

let make_index ?(start = 0) ~length () =
  let ts =
    Array.init length (fun i -> epoch +. (float_of_int (start + i) *. 86_400.0))
  in
  match Cairos.Index.of_unix_floats Cairos.Freq.Day ts with
  | Ok i -> i
  | Error e -> failwith ("bench input index: " ^ Cairos.Index.err_to_string e)

let make_values ~length =
  Nx.create Nx.float64 [| length |]
    (Array.init length (fun i ->
         Float.of_int (((i * 7) + 13) mod 1000) /. 100.0))

let make_series idx vals =
  match Cairos.Series.make idx vals with
  | Ok s -> s
  | Error e -> failwith ("bench input series: " ^ e)
