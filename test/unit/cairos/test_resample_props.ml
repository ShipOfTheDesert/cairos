(* Property suite for [Cairos.Resample]. RFC 0046 TP-Resample-1..3.

   Each property runs at [~count:200] per RFC 0046 Test Plan; CI/local
   reproducibility is provided by [Qcheck_gen.pin_seed_from_env] (FR-4). *)

(* TP-Resample-1 — downsampling minute-frequency input to daily never grows the
   output past the source length. The intersection of "one daily bucket per
   calendar day touched" and "buckets are non-empty by construction" caps the
   output at min(len_source, days_spanned), and days_spanned <= len_source
   trivially when source granularity is finer than target.

   [minute_finite_float_series_arb] starts at the 2024-01-01T00:00:00Z epoch
   with a 60s bucket interval (qcheck_gen.ml:127), so the timestamps are
   strictly-increasing finite POSIX seconds well within Ptime range.
   [Resample.resample] returns [Error] only when the target frequency is not
   strictly lower than the source (lib/resample.ml:98) or when an internal
   Ptime.sub_span / Ptime.of_date_time call fails (structurally unreachable
   for Day target on a synthetic 2024-era minute series). The [Error] branch
   is therefore unreachable; per ~/.claude/solutions/ocaml/qcheck-generator-failwith.md
   (RFC 0030 §R4), it is terminated with [failwith] so QCheck does not
   mis-shrink a generator-internal failure into a phantom library bug. *)
let downsample_never_grows_length =
  QCheck.Test.make ~count:200 ~name:"downsample_never_grows_length"
    Qcheck_gen.minute_finite_float_series_arb (fun s ->
      match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Day s with
      | Error _ ->
          (* Unreachable: minute -> daily is a valid downsample for synthetic
             2024-era inputs. RFC 0030 §R4. *)
          failwith
            "unreachable: minute_finite_float_series_arb produces valid \
             downsample inputs for Cairos.Freq.Day"
      | Ok result -> Cairos.Series.length result <= Cairos.Series.length s)

(* TP-Resample-2 — every output timestamp from an hourly -> daily downsample
   sits exactly at calendar midnight UTC. Bucket boundaries are reconstructed
   from the (year, month, day) triple with hour/min/sec zeroed
   (lib/resample.ml:43-45, 77-78), so a regression that emits the
   first-element-of-bucket timestamp instead of the calendar midnight would
   surface here.

   [hourly_finite_float_series_arb] starts at the 2024-01-01T00:00:00Z epoch
   with a 3_600s bucket interval (qcheck_gen.ml:139). The Day target
   exercises only two Ptime entry points — [Ptime.to_date] (lib/resample.ml:44,
   total on any valid Ptime.t) and [Ptime.of_date_time] (lib/resample.ml:78,
   returns option but always Some when the input came from [Ptime.to_date] of
   a valid Ptime.t with a zero time-of-day, as is the case here). The Hour /
   Week branches' [Ptime.weekday] / [Ptime.sub_span] calls are unreachable at
   the Day target. [Error] is therefore unreachable for the same reasoning as
   TP-Resample-1 and terminated with [failwith] per RFC 0030 §R4. The
   time-of-day comparison uses the [Ptime.to_date_time] tuple shape —
   [(date, ((hh, mm, ss), tz_offset_s))] — directly, mirroring the task
   plan's spelling. *)
let downsample_timestamps_calendar_aligned =
  QCheck.Test.make ~count:200 ~name:"downsample_timestamps_calendar_aligned"
    Qcheck_gen.hourly_finite_float_series_arb (fun s ->
      match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Day s with
      | Error _ ->
          (* Unreachable: hourly -> daily is a valid downsample for synthetic
             2024-era inputs. RFC 0030 §R4. *)
          failwith
            "unreachable: hourly_finite_float_series_arb produces valid \
             downsample inputs for Cairos.Freq.Day"
      | Ok result ->
          let ts = Cairos.Index.timestamps (Cairos.Series.index result) in
          ts
          |> Array.for_all (fun t ->
              let _, time_of_day = Ptime.to_date_time t in
              time_of_day = ((0, 0, 0), 0)))

(* TP-Resample-3 was originally drafted as a [~count:200] property
   asserting Daily -> Minute resampling is rejected with [Error]. Per RFC
   0046 review C9, demoted to a deterministic Alcotest case
   [rejects_upsampling_daily_to_minute] in test_resample.ml — the contract
   is one branch of the rank guard at lib/resample.ml:98 and does not
   depend on input shape, so a [~count:200] declaration would have promised
   random coverage the contract neither needs nor benefits from. The
   property file therefore registers two rather than three TP-Resample-*
   tests; the third lives next to its peer rejection cases in
   test_resample.ml. *)

let () =
  Qcheck_gen.pin_seed_from_env ();
  let tests =
    List.map QCheck_alcotest.to_alcotest
      [ downsample_never_grows_length; downsample_timestamps_calendar_aligned ]
  in
  Alcotest.run "Resample.props" [ ("property", tests) ]
