(* Property suite for [Cairos.Resample].

   Each property runs at [~count:200]; CI/local
   reproducibility is provided by [Qcheck_gen.pin_seed_from_env]. *)

(* Downsampling minute-frequency input to daily never grows the
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
   is therefore unreachable; it is terminated with [failwith] so QCheck does not
   mis-shrink a generator-internal failure into a phantom library bug. *)
let downsample_never_grows_length =
  QCheck.Test.make ~count:200 ~name:"downsample_never_grows_length"
    Qcheck_gen.minute_finite_float_series_arb (fun s ->
      match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Day s with
      | Error _ ->
          (* Unreachable: minute -> daily is a valid downsample for synthetic
             2024-era inputs. *)
          failwith
            "unreachable: minute_finite_float_series_arb produces valid \
             downsample inputs for Cairos.Freq.Day"
      | Ok result -> Cairos.Series.length result <= Cairos.Series.length s)

(* Every output timestamp from an hourly -> daily downsample
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
   the length property above and terminated with [failwith]. The
   time-of-day comparison uses the [Ptime.to_date_time] tuple shape —
   [(date, ((hh, mm, ss), tz_offset_s))] — directly. *)
let downsample_timestamps_calendar_aligned =
  QCheck.Test.make ~count:200 ~name:"downsample_timestamps_calendar_aligned"
    Qcheck_gen.hourly_finite_float_series_arb (fun s ->
      match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Day s with
      | Error _ ->
          (* Unreachable: hourly -> daily is a valid downsample for synthetic
             2024-era inputs. *)
          failwith
            "unreachable: hourly_finite_float_series_arb produces valid \
             downsample inputs for Cairos.Freq.Day"
      | Ok result ->
          let ts = Cairos.Index.timestamps (Cairos.Series.index result) in
          ts
          |> Array.for_all (fun t ->
              let _, time_of_day = Ptime.to_date_time t in
              time_of_day = ((0, 0, 0), 0)))

(* A downsample-to-minute rejection property was originally drafted as a
   [~count:200] property asserting Daily -> Minute resampling is rejected with
   [Error]. It was demoted to a deterministic Alcotest case
   [rejects_upsampling_daily_to_minute] in test_resample.ml — the contract
   is one branch of the rank guard at lib/resample.ml:98 and does not
   depend on input shape, so a [~count:200] declaration would have promised
   random coverage the contract neither needs nor benefits from. That
   rejection case therefore lives next to its peer rejection cases in
   test_resample.ml rather than here. *)

(* Daily -> monthly downsampling groups source bars by calendar month.
   The three structural invariants of the output, over a source spanning
   many months (and at least one year boundary, since the arb reaches up to
   ~600 days from the 2024-01-01 epoch):

   - Output length equals the number of *distinct* (year, month) pairs in the
     source. Empty buckets are omitted by contract, and every touched month
     produces exactly one bucket, so the count is the distinct-month count —
     not the source length (many days share a month) and not the month span
     (a source could skip months, though this arb's consecutive days never do).
   - Every output timestamp is a calendar-month start: day 01 at 00:00:00 UTC,
     the synthesised anchor. A regression that labelled buckets with the first
     source bar of the month (frequently not the 1st) surfaces here.
   - Output is strictly monotonic — buckets are emitted in ascending
     (year, month) order with no repeats.

   [daily_multi_month_series_arb] starts at the 2024-01-01T00:00:00Z epoch
   with an 86_400s bucket interval, so timestamps are strictly-increasing
   finite POSIX seconds well within Ptime range. [Resample.resample] returns
   [Error] only when the target rank is not strictly above the source
   (Day < Month always) or on an internal Ptime failure (structurally
   unreachable for a Month target on synthetic 2024/2025-era daily bars). The
   [Error] branch is therefore unreachable and terminated with [failwith] so
   QCheck does not mis-shrink a generator-internal failure into a phantom
   library bug. *)
let monthly_resample_bucket_count_and_labels =
  QCheck.Test.make ~count:200 ~name:"monthly_resample_bucket_count_and_labels"
    Qcheck_gen.daily_multi_month_series_arb (fun s ->
      match Cairos.Resample.resample ~agg:`Last Cairos.Freq.Month s with
      | Error _ ->
          (* Unreachable: daily -> monthly is a valid downsample for synthetic
             2024/2025-era inputs. *)
          failwith
            "unreachable: daily_multi_month_series_arb produces valid \
             downsample inputs for Cairos.Freq.Month"
      | Ok result ->
          let src_ts = Cairos.Index.timestamps (Cairos.Series.index s) in
          let distinct_year_months =
            src_ts
            |> Array.to_list
            |> List.map (fun t ->
                let y, m, _d = Ptime.to_date t in
                (y, m))
            |> List.sort_uniq compare
            |> List.length
          in
          let out_ts = Cairos.Index.timestamps (Cairos.Series.index result) in
          let count_matches_distinct_months =
            Array.length out_ts = distinct_year_months
          in
          let all_month_starts =
            Array.for_all
              (fun t ->
                let (_y, _m, day), time_of_day = Ptime.to_date_time t in
                day = 1 && time_of_day = ((0, 0, 0), 0))
              out_ts
          in
          let strictly_monotonic =
            let ok = ref true in
            for i = 1 to Array.length out_ts - 1 do
              if not (Ptime.is_later out_ts.(i) ~than:out_ts.(i - 1)) then
                ok := false
            done;
            !ok
          in
          count_matches_distinct_months
          && all_month_starts
          && strictly_monotonic)

let () =
  Qcheck_gen.pin_seed_from_env ();
  let tests =
    List.map QCheck_alcotest.to_alcotest
      [
        downsample_never_grows_length;
        downsample_timestamps_calendar_aligned;
        monthly_resample_bucket_count_and_labels;
      ]
  in
  Alcotest.run "Resample.props" [ ("property", tests) ]
