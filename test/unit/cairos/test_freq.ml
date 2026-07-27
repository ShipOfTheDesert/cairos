(* Deliberately local, not Test_helpers.name_of_any. Here the projection is the
   oracle for the round-trip under test — that [Any] preserves the witness it
   wrapped — so importing the shared one would check the existential against
   itself. Same reasoning as test_resample_props.ml's [rank_of]. *)
let name_of_any (Cairos.Freq.Any f) =
  match f with
  | Cairos.Freq.Minute -> "Minute"
  | Cairos.Freq.Hour -> "Hour"
  | Cairos.Freq.Day -> "Day"
  | Cairos.Freq.Week -> "Week"
  | Cairos.Freq.Month -> "Month"

let freq_any_wraps_every_frequency () =
  let all =
    [
      Cairos.Freq.Any Cairos.Freq.Minute;
      Cairos.Freq.Any Cairos.Freq.Hour;
      Cairos.Freq.Any Cairos.Freq.Day;
      Cairos.Freq.Any Cairos.Freq.Week;
      Cairos.Freq.Any Cairos.Freq.Month;
    ]
  in
  Alcotest.(check (list string))
    "every frequency survives the existential"
    [ "Minute"; "Hour"; "Day"; "Week"; "Month" ]
    (List.map name_of_any all)

let tests =
  [
    Alcotest.test_case "any wraps every frequency" `Quick
      freq_any_wraps_every_frequency;
  ]

let () = Alcotest.run "Freq" [ ("Freq", tests) ]
