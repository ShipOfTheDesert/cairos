type 'freq t = { freq : 'freq Freq.t; timestamps : Ptime.t array }

type err =
  | Invalid_timestamp of { position : int; raw : string }
  | Non_monotonic of { position : int }
  | Invalid_unix_timestamp of { position : int; raw : float }

let err_to_string = function
  | Invalid_timestamp { position; raw } ->
      Printf.sprintf "invalid timestamp at position %d: %s" position raw
  | Non_monotonic { position } ->
      Printf.sprintf "timestamps not strictly monotonic at position %d" position
  | Invalid_unix_timestamp { position; raw } ->
      Printf.sprintf "invalid unix timestamp at position %d: %s" position
        (Float.to_string raw)

let ( let* ) = Result.bind

let validate_monotonic (ts : Ptime.t array) : (unit, err) result =
  let len = Array.length ts in
  let rec loop i =
    if i >= len then Ok ()
    else if Ptime.compare ts.(i - 1) ts.(i) >= 0 then
      Error (Non_monotonic { position = i })
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
  | Error _ -> Error (Invalid_timestamp { position = i; raw = s })

let parse_timestamps (strings : string array) : (Ptime.t array, err) result =
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
          Error (Invalid_unix_timestamp { position = i; raw = floats.(i) })
  in
  loop 0

let length t = Array.length t.timestamps
let freq t = t.freq
let timestamps t = Array.copy t.timestamps
let of_ptime_array_unsafe freq timestamps = { freq; timestamps }

let slice ~start ~stop t =
  let len = Array.length t.timestamps in
  let start' = max 0 (min start len) in
  let stop' = max 0 (min stop len) in
  let new_len = max 0 (stop' - start') in
  let timestamps = Array.sub t.timestamps start' new_len in
  { freq = t.freq; timestamps }
