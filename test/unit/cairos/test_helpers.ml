let make_daily_series dates values =
  match Cairos.Index.daily dates with
  | Error e -> Alcotest.fail (Cairos.Index.err_to_string e)
  | Ok idx -> (
      let vals = Nx.create Nx.float64 [| Array.length values |] values in
      match Cairos.Series.make idx vals with
      | Error e -> Alcotest.fail (Cairos.Series.err_to_string e)
      | Ok s -> s)

let make_minute_series dates values =
  match Cairos.Index.minute dates with
  | Error e -> Alcotest.fail (Cairos.Index.err_to_string e)
  | Ok idx -> (
      let vals = Nx.create Nx.float64 [| Array.length values |] values in
      match Cairos.Series.make idx vals with
      | Error e -> Alcotest.fail (Cairos.Series.err_to_string e)
      | Ok s -> s)

let make_hourly_series dates values =
  match Cairos.Index.hourly dates with
  | Error e -> Alcotest.fail (Cairos.Index.err_to_string e)
  | Ok idx -> (
      let vals = Nx.create Nx.float64 [| Array.length values |] values in
      match Cairos.Series.make idx vals with
      | Error e -> Alcotest.fail (Cairos.Series.err_to_string e)
      | Ok s -> s)

let make_weekly_series dates values =
  match Cairos.Index.weekly dates with
  | Error e -> Alcotest.fail (Cairos.Index.err_to_string e)
  | Ok idx -> (
      let vals = Nx.create Nx.float64 [| Array.length values |] values in
      match Cairos.Series.make idx vals with
      | Error e -> Alcotest.fail (Cairos.Series.err_to_string e)
      | Ok s -> s)

let make_monthly_series dates values =
  match Cairos.Index.monthly dates with
  | Error e -> Alcotest.fail (Cairos.Index.err_to_string e)
  | Ok idx -> (
      let vals = Nx.create Nx.float64 [| Array.length values |] values in
      match Cairos.Series.make idx vals with
      | Error e -> Alcotest.fail (Cairos.Series.err_to_string e)
      | Ok s -> s)

let frame_get_exn name frame =
  match Cairos.Frame.get name frame with
  | Some s -> s
  | None -> Alcotest.fail (Printf.sprintf "missing column '%s'" name)

let assoc_exn name lst =
  match List.assoc_opt name lst with
  | Some v -> v
  | None -> Alcotest.fail (Printf.sprintf "missing key '%s'" name)

let ptime_testable = Alcotest.testable (Ptime.pp_rfc3339 ()) Ptime.equal

(* Unwraps the [Freq.any] existential to a comparable name. Assertions and
   failure messages that mention a frequency witness go through this rather than
   over the [Freq.any] values themselves, which would put structural equality on
   a GADT existential.

   Shared rather than re-derived per file: test_resample.ml and
   test_resample_props.ml both want it for diagnostics, where an independent
   projection buys nothing. test_freq.ml deliberately keeps its own copy — there
   the projection is the oracle for the round-trip under test, so importing this
   one would check the existential against itself. *)
let name_of_any (Cairos.Freq.Any f) =
  match f with
  | Cairos.Freq.Minute -> "Minute"
  | Cairos.Freq.Hour -> "Hour"
  | Cairos.Freq.Day -> "Day"
  | Cairos.Freq.Week -> "Week"
  | Cairos.Freq.Month -> "Month"
