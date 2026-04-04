let () =
  Alcotest.run "cairos"
    [ ("Index", Test_index.tests); ("Series", Test_series.tests) ]
