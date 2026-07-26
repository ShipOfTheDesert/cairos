let generated_fixture_dir = "validation/fixtures"
let oracle_fixture_dir = "validation/oracle_fixtures"
let default_tolerance = 1e-10

let die ~prefix ~code fmt =
  Printf.ksprintf
    (fun s ->
      flush stdout;
      prerr_endline (prefix ^ s);
      exit code)
    fmt

let die_tooling ~binary fmt = die ~prefix:(binary ^ " (tooling): ") ~code:2 fmt
let die_mismatch ~binary fmt = die ~prefix:(binary ^ ": ") ~code:1 fmt

let read_lines ~binary path =
  match In_channel.with_open_text path In_channel.input_all with
  | exception Sys_error msg -> die_tooling ~binary "%s: %s" path msg
  | content ->
      String.split_on_char '\n' content
      |> List.filter (fun s -> String.length (String.trim s) > 0)

let float_close ~tolerance a b =
  match (Float.is_nan a, Float.is_nan b) with
  | true, true -> true
  | true, false
  | false, true ->
      false
  | false, false -> Float.abs (a -. b) <= tolerance

let abs_diff a b =
  if Float.is_nan a || Float.is_nan b then Float.infinity else Float.abs (a -. b)
