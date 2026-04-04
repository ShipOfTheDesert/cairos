type 'freq t = { freq : 'freq Freq.t; timestamps : Ptime.t array }

let ( let* ) = Result.bind

let validate_monotonic (ts : Ptime.t array) : (unit, string) result =
  let len = Array.length ts in
  let rec loop i =
    if i >= len then Ok ()
    else if Ptime.compare ts.(i - 1) ts.(i) >= 0 then
      Error
        (Printf.sprintf "timestamps not strictly monotonic at position %d" i)
    else loop (i + 1)
  in
  if len <= 1 then Ok () else loop 1

let parse_timestamp i s =
  (* Accept date-only strings by appending T00:00:00Z *)
  let s' =
    if String.contains s 'T' || String.contains s 't' then s
    else s ^ "T00:00:00Z"
  in
  match Ptime.of_rfc3339 s' with
  | Ok (t, _, _) -> Ok t
  | Error _ -> Error (Printf.sprintf "invalid timestamp at position %d: %s" i s)

let parse_timestamps (strings : string array) : (Ptime.t array, string) result =
  let len = Array.length strings in
  let ts = Array.make len Ptime.epoch in
  let rec loop i =
    if i >= len then Ok ts
    else
      let* t = parse_timestamp i strings.(i) in
      ts.(i) <- t;
      loop (i + 1)
  in
  loop 0

let make_index freq ts =
  let* () = validate_monotonic ts in
  Ok { freq; timestamps = ts }

let daily strings =
  let* ts = parse_timestamps strings in
  make_index Freq.Day ts

let minute strings =
  let* ts = parse_timestamps strings in
  make_index Freq.Minute ts

let hourly strings =
  let* ts = parse_timestamps strings in
  make_index Freq.Hour ts

let weekly strings =
  let* ts = parse_timestamps strings in
  make_index Freq.Week ts

let of_unix_floats freq floats =
  let len = Array.length floats in
  let ts = Array.make len Ptime.epoch in
  let rec loop i =
    if i >= len then
      let* () = validate_monotonic ts in
      Ok { freq; timestamps = ts }
    else
      match Ptime.of_float_s floats.(i) with
      | Some t ->
          ts.(i) <- t;
          loop (i + 1)
      | None ->
          Error
            (Printf.sprintf "invalid unix timestamp at position %d: %s" i
               (Float.to_string floats.(i)))
  in
  loop 0

let length t = Array.length t.timestamps
let freq t = t.freq
let timestamps t = Array.copy t.timestamps

let slice ~start ~stop t =
  let len = Array.length t.timestamps in
  let start' = max 0 (min start len) in
  let stop' = max 0 (min stop len) in
  let new_len = max 0 (stop' - start') in
  let timestamps = Array.sub t.timestamps start' new_len in
  { freq = t.freq; timestamps }
