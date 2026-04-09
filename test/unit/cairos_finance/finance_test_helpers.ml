(* 2020-01-01T00:00:00Z in POSIX seconds. *)
let jan_1_2020_utc = 1_577_836_800.0

let index_of_length ~freq ~step n =
  let floats =
    Array.init n (fun i -> jan_1_2020_utc +. (float_of_int i *. step))
  in
  match Cairos.Index.of_unix_floats freq floats with
  | Ok idx -> idx
  | Error msg -> Alcotest.fail (Printf.sprintf "index_of_length: %s" msg)

let daily_index_of_length n =
  index_of_length ~freq:Cairos.Freq.Day ~step:86_400.0 n

let hourly_index_of_length n =
  index_of_length ~freq:Cairos.Freq.Hour ~step:3_600.0 n

let minute_index_of_length n =
  index_of_length ~freq:Cairos.Freq.Minute ~step:60.0 n

let weekly_index_of_length n =
  index_of_length ~freq:Cairos.Freq.Week ~step:604_800.0 n

let make_series idx values =
  let n = Array.length values in
  let nx = Nx.create Nx.float64 [| n |] values in
  match Cairos.Series.make idx nx with
  | Ok s -> s
  | Error msg -> Alcotest.fail (Printf.sprintf "make_series: %s" msg)

let make_daily_series values =
  make_series (daily_index_of_length (Array.length values)) values
