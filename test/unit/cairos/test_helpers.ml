let make_daily_series dates values =
  match Cairos.Index.daily dates with
  | Error e -> Alcotest.fail e
  | Ok idx -> (
      let vals = Nx.create Nx.float64 [| Array.length values |] values in
      match Cairos.Series.make idx vals with
      | Error e -> Alcotest.fail e
      | Ok s -> s)

let make_minute_series dates values =
  match Cairos.Index.minute dates with
  | Error e -> Alcotest.fail e
  | Ok idx -> (
      let vals = Nx.create Nx.float64 [| Array.length values |] values in
      match Cairos.Series.make idx vals with
      | Error e -> Alcotest.fail e
      | Ok s -> s)

let make_hourly_series dates values =
  match Cairos.Index.hourly dates with
  | Error e -> Alcotest.fail e
  | Ok idx -> (
      let vals = Nx.create Nx.float64 [| Array.length values |] values in
      match Cairos.Series.make idx vals with
      | Error e -> Alcotest.fail e
      | Ok s -> s)

let make_weekly_series dates values =
  match Cairos.Index.weekly dates with
  | Error e -> Alcotest.fail e
  | Ok idx -> (
      let vals = Nx.create Nx.float64 [| Array.length values |] values in
      match Cairos.Series.make idx vals with
      | Error e -> Alcotest.fail e
      | Ok s -> s)

let ptime_testable = Alcotest.testable (Ptime.pp_rfc3339 ()) Ptime.equal
