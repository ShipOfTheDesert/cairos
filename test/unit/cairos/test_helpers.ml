let make_daily_series dates values =
  match Cairos.Index.daily dates with
  | Error e -> Alcotest.fail e
  | Ok idx -> (
      let vals = Nx.create Nx.float64 [| Array.length values |] values in
      match Cairos.Series.make idx vals with
      | Error e -> Alcotest.fail e
      | Ok s -> s)
