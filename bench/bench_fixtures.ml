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

let make_frame ~bars ~columns =
  let idx = make_index ~length:bars () in
  let bars_f = Float.of_int bars in
  let make_pair j =
    let offset = Float.of_int j /. 1000.0 in
    let values =
      Nx.create Nx.float64 [| bars |]
        (Array.init bars (fun i -> (Float.of_int (i + j) /. bars_f) +. offset))
    in
    ("c" ^ string_of_int j, make_series idx values)
  in
  let pairs = List.init columns make_pair in
  match Cairos.Nonempty.of_list pairs with
  | None -> failwith "bench input frame: columns must be >= 1"
  | Some ne -> (
      match Cairos.Frame.of_series ne with
      | Ok f -> f
      | Error e -> failwith ("bench input frame: " ^ e))
