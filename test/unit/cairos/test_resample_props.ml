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
   strictly lower than the source ([freq_rank] guard, lib/resample.ml) or when
   an internal
   Ptime.sub_span / Ptime.of_date_time call fails (structurally unreachable
   for Day target on a synthetic 2024-era minute series). The [Error] branch
   is therefore unreachable; it is terminated with [failwith] rather than
   [false] because reaching it means a library invariant broke, which a
   property failure would misreport as a contract violation. [failwith] does
   not suppress shrinking — QCheck shrinks raised exceptions too. See
   ocaml/qcheck-generator-failwith.md. *)
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
   ([bucket_key_of_ptime], lib/resample.ml), so a regression that emits the
   first-element-of-bucket timestamp instead of the calendar midnight would
   surface here.

   [hourly_finite_float_series_arb] starts at the 2024-01-01T00:00:00Z epoch
   with a 3_600s bucket interval (qcheck_gen.ml:139). The Day target
   exercises only two Ptime entry points — [Ptime.to_date] (called from
   [bucket_key_of_ptime], total on any valid Ptime.t) and [Ptime.of_date_time]
   (called from [ptime_of_bucket_key], lib/resample.ml;
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

(* Non-NaN observations per calendar month, in ascending (year, month) order,
   derived from the source alone. The grouping rule is re-derived from the
   documented contract — one bucket per calendar (year, month) touched, empty
   buckets omitted (resample.mli) — rather than from [bucket_key_of_ptime], so
   the property checks the implementation against the contract and not against
   itself. Consecutive grouping is sound because index timestamps are strictly
   increasing by construction. *)
let expected_month_counts s =
  let ts = Cairos.Index.timestamps (Cairos.Series.index s) in
  let vs = Nx.to_array (Cairos.Series.values s) in
  let acc = ref [] in
  Array.iteri
    (fun i t ->
      let y, m, _d = Ptime.to_date t in
      let observed = if Float.is_nan vs.(i) then 0 else 1 in
      match !acc with
      | (key, n) :: rest when key = (y, m) -> acc := (key, n + observed) :: rest
      | _ -> acc := ((y, m), observed) :: !acc)
    ts;
  Array.of_list (List.rev !acc)

(* [`Count] reports, per output bucket, exactly the source observations landing
   in that bucket less that bucket's NaN count — the contract the aggregation
   exists for, checked over a NaN density high enough that essentially every
   bucket is mixed (qcheck_gen.ml, [daily_multi_month_with_nan_series_arb]). The
   deterministic cases in test_resample.ml pin the boundary shapes (all-NaN
   bucket, infinities); what random input adds is bucket *membership* — that
   the exclusion is applied to the right slice, across variable month lengths
   and a year boundary.

   The second assertion bounds each count by its bucket's cardinality, obtained
   as [`Sum] over an all-ones series built at the same length and therefore
   carrying the identical index ([make_series_from_floats] derives timestamps
   from the position alone). It is strictly weaker than the equality above — a
   non-NaN count cannot exceed its bucket's size, so any input that trips the
   bound trips the equality too. It is checked *first* precisely for that
   reason: a count above the bucket's cardinality is the one failure class with
   a diagnosis better than "expected 21, got 31", and reported second it would
   be dead code behind the equality's report. What it adds over the equality is
   a second, library-side derivation of bucket membership, so a regression that
   moved observations between buckets is described in cardinality terms rather
   than only as a per-month number disagreeing.

   [Error] is unreachable for the same reason as the properties above — Day <
   Month always, and synthetic 2024/2025-era daily bars produce no Ptime
   boundary failure — and is terminated with [failwith] because reaching it
   means a library invariant broke, not that the contract was violated. *)
let resample_count_matches_bucket_membership =
  QCheck.Test.make ~count:200 ~name:"resample_count_matches_bucket_membership"
    Qcheck_gen.daily_multi_month_with_nan_series_arb (fun s ->
      let ones =
        Qcheck_gen.make_series_from_floats ~freq:Cairos.Freq.Day
          (Array.make (Cairos.Series.length s) 1.0)
      in
      match
        ( Cairos.Resample.resample ~agg:`Count Cairos.Freq.Month s,
          Cairos.Resample.resample ~agg:`Sum Cairos.Freq.Month ones )
      with
      | Error _, _
      | Ok _, Error _ ->
          (* Unreachable: daily -> monthly is a valid downsample for synthetic
             2024/2025-era inputs. *)
          failwith
            "unreachable: daily_multi_month_with_nan_series_arb produces valid \
             downsample inputs for Cairos.Freq.Month"
      | Ok counted, Ok sized -> (
          let expected = expected_month_counts s in
          let counts = Nx.to_array (Cairos.Series.values counted) in
          let sizes = Nx.to_array (Cairos.Series.values sized) in
          if
            Array.length counts <> Array.length expected
            || Array.length sizes <> Array.length counts
          then
            QCheck.Test.fail_reportf
              "length %d: %d count buckets, %d size buckets, %d distinct \
               calendar months in the source"
              (Cairos.Series.length s) (Array.length counts)
              (Array.length sizes) (Array.length expected)
          else
            let mismatches =
              counts
              |> Array.mapi (fun i c ->
                  let (year, month), n = expected.(i) in
                  if c > sizes.(i) then
                    Some
                      (Printf.sprintf
                         "%04d-%02d: count %g exceeds bucket size %g" year month
                         c sizes.(i))
                  else if not (Float.equal c (float_of_int n)) then
                    Some
                      (Printf.sprintf "%04d-%02d: count %g, expected %d" year
                         month c n)
                  else None)
              |> Array.to_list
              |> List.filter_map Fun.id
            in
            match mismatches with
            | [] -> true
            | ms ->
                QCheck.Test.fail_reportf "length %d: %d bucket(s) wrong: %s"
                  (Cairos.Series.length s) (List.length ms)
                  (String.concat "; " ms)))

(* A downsample-to-minute rejection property was originally drafted as a
   [~count:200] property asserting Daily -> Minute resampling is rejected with
   [Error]. It was demoted to a deterministic Alcotest case
   [rejects_upsampling_daily_to_minute] in test_resample.ml — the contract
   is one branch of the [freq_rank] guard (lib/resample.ml) and does not
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
   [Error] branch is therefore unreachable and terminated with [failwith] for
   the same reason as the length property above: it means a library invariant
   broke, not that the contract was violated. Shrinking still happens either
   way. *)
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

(* [resample] rejects exactly when the target frequency is not strictly lower
   than the source, over every one of the 25 ordered frequency pairs.

   The pair set is walked exhaustively inside the property rather than sampled
   by the generator: 25 pairs against [~count:200] draws leaves a real chance
   (~0.7%) that some pair is never exercised on a given run, and "holds for
   every pair" is precisely the claim. What the generator varies is the source
   series' length — the one input dimension the classification must be
   insensitive to. Values are drawn finite but nothing here depends on them.

   Length 0 is inside the generator's range but is not pinned here: at 1-in-65
   per draw it is missed on roughly 1 run in 22, so the empty-input case has its
   own deterministic case, [rejects_upsampling_on_empty_series] in
   test_resample.ml, rather than resting on a draw that usually arrives.

   [rank_of] is re-derived from the documented total order (Minute < Hour < Day
   < Week < Month, resample.mli) rather than imported from [lib/resample.ml], so
   the property checks the implementation against the contract and not against
   itself.

   The two [Unrepresentable_*] variants are structurally unreachable for these
   synthetic 2020/2024-era inputs; those arms terminate with [failwith] rather
   than returning [false], because a property failure would report a broken
   library invariant as a contract violation — the wrong diagnosis on the one
   path whose purpose is post-mortem debuggability. Note that these arms match
   on [resample]'s own return value, not on generator output: reaching one is a
   library regression, not generator noise. QCheck shrinks a raised exception
   exactly as it shrinks a [false] return (QCheck2.ml:1932 — "test raised [e]
   on [input]; try to shrink then fail"; only [Failed_precondition] and
   [No_example_found] are exempt), so [failwith] buys no shrink suppression and
   is not chosen for any. See ocaml/qcheck-generator-failwith.md. *)
let all_frequencies =
  [
    Cairos.Freq.Any Cairos.Freq.Minute;
    Cairos.Freq.Any Cairos.Freq.Hour;
    Cairos.Freq.Any Cairos.Freq.Day;
    Cairos.Freq.Any Cairos.Freq.Week;
    Cairos.Freq.Any Cairos.Freq.Month;
  ]

let rank_of (Cairos.Freq.Any f) =
  match f with
  | Cairos.Freq.Minute -> 0
  | Cairos.Freq.Hour -> 1
  | Cairos.Freq.Day -> 2
  | Cairos.Freq.Week -> 3
  | Cairos.Freq.Month -> 4

(* Shared, unlike [rank_of] above: this one only labels failure messages, so it
   has no contract to encode independently. *)
let name_of = Test_helpers.name_of_any

(* [true] when [resample]'s classification of this pair matches the contract:
   [Error (Target_not_lower _)] exactly when the target rank does not exceed the
   source rank, [Ok] otherwise. *)
let classified_per_contract xs source target =
  let must_reject = rank_of target <= rank_of source in
  match (source, target) with
  | Cairos.Freq.Any src, Cairos.Freq.Any tgt -> (
      let s = Qcheck_gen.make_series_from_floats ~freq:src xs in
      match Cairos.Resample.resample ~agg:`Last tgt s with
      | Error (Cairos.Resample.Target_not_lower _) -> must_reject
      | Ok _ -> not must_reject
      | Error (Cairos.Resample.Unrepresentable_week_start _)
      | Error (Cairos.Resample.Unrepresentable_bucket_timestamp _) ->
          failwith
            "unreachable: synthetic epoch-anchored series produce no Ptime \
             boundary failures")

(* Prefix truncation, matching [qcheck_gen.shrink_daily_series]. Length is the
   only structural parameter the property depends on — the values are never
   inspected, only carried through [resample] — so shrinking length alone
   minimises a counterexample fully. Without a shrinker the report names
   whatever length was drawn, which for a 0-64 range is usually not the
   boundary that matters. *)
let shrink_float_array xs =
  let n = Array.length xs in
  let open QCheck.Iter in
  if n <= 1 then empty
  else
    let candidates =
      List.sort_uniq compare [ 0; 1; n / 2; n - 1 ]
      |> List.filter (fun k -> k >= 0 && k < n)
    in
    of_list candidates >|= fun k -> Array.sub xs 0 k

let float_array_arb =
  let open QCheck in
  make ~shrink:shrink_float_array
    ~print:(fun xs -> Printf.sprintf "<float array len=%d>" (Array.length xs))
    (Gen.array_size (Gen.int_range 0 64) (Gen.float_range (-1e6) 1e6))

let resample_rejects_iff_target_not_lower =
  QCheck.Test.make ~count:200 ~name:"resample_rejects_iff_target_not_lower"
    float_array_arb (fun xs ->
      let misclassified =
        all_frequencies
        |> List.concat_map (fun source ->
            all_frequencies |> List.map (fun target -> (source, target)))
        |> List.filter (fun (source, target) ->
            not (classified_per_contract xs source target))
      in
      match misclassified with
      | [] -> true
      | pairs ->
          QCheck.Test.fail_reportf
            "length %d: %d of 25 frequency pairs misclassified: %s"
            (Array.length xs) (List.length pairs)
            (pairs
            |> List.map (fun (source, target) ->
                Printf.sprintf "%s -> %s" (name_of source) (name_of target))
            |> String.concat ", "))

let () =
  Qcheck_gen.pin_seed_from_env ();
  let tests =
    List.map QCheck_alcotest.to_alcotest
      [
        downsample_never_grows_length;
        downsample_timestamps_calendar_aligned;
        monthly_resample_bucket_count_and_labels;
        resample_count_matches_bucket_membership;
        resample_rejects_iff_target_not_lower;
      ]
  in
  Alcotest.run "Resample.props" [ ("property", tests) ]
