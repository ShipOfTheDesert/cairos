let () =
  Alcotest.run "cairos"
    [
      ("Index", Test_index.tests);
      ("Series", Test_series.tests);
      ("Align", Test_align.tests);
      ("Window", Test_window.tests);
    ]
