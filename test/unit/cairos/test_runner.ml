let () =
  Alcotest.run "cairos"
    [
      ("Index", Test_index.tests);
      ("Series", Test_series.tests);
      ("Align", Test_align.tests);
      ("Window", Test_window.tests);
      ("Resample", Test_resample.tests);
      ("Frame", Test_frame.tests);
    ]
