let freq_rank : type a. a Freq.t -> int = function
  | Freq.Minute -> 0
  | Freq.Hour -> 1
  | Freq.Day -> 2
  | Freq.Week -> 3

let freq_name : type a. a Freq.t -> string = function
  | Freq.Minute -> "Minute"
  | Freq.Hour -> "Hour"
  | Freq.Day -> "Day"
  | Freq.Week -> "Week"

(* A bucket key identifies the calendar period a timestamp belongs to.
   Decomposition from Ptime.t is total (Ptime.to_date_time never fails).
   Reconstruction to Ptime.t returns option and is isolated to the output
   boundary where resample already returns result. *)
type bucket_key = { year : int; month : int; day : int; hour : int }

let bucket_key_equal a b =
  a.year = b.year && a.month = b.month && a.day = b.day && a.hour = b.hour

(* Ptime.to_date_time and Ptime.weekday never fail on a valid Ptime.t.
   The Freq.t match determines which components matter for grouping:
   - Hour: year, month, day, hour (minutes/seconds ignored)
   - Day: year, month, day (time ignored)
   - Week: Monday's year, month, day (weekday offset subtracted)
   - Minute: all components (identity — never a valid target, included for
     exhaustiveness)
   Returns result because Ptime.sub_span in the Week branch returns option;
   the None is structurally unreachable but propagated rather than unwrapped. *)
let bucket_key_of_ptime : type a.
    a Freq.t -> Ptime.t -> (bucket_key, string) result =
 fun freq t ->
  match freq with
  | Freq.Minute ->
      (* Unreachable: Minute cannot be a target frequency (rank 0, nothing
         lower). Included for GADT exhaustiveness. *)
      let (year, month, day), ((hour, _mm, _ss), _tz) = Ptime.to_date_time t in
      Ok { year; month; day; hour }
  | Freq.Hour ->
      let (year, month, day), ((hour, _mm, _ss), _tz) = Ptime.to_date_time t in
      Ok { year; month; day; hour }
  | Freq.Day ->
      let year, month, day = Ptime.to_date t in
      Ok { year; month; day; hour = 0 }
  | Freq.Week -> (
      let days_since_monday =
        match Ptime.weekday ~tz_offset_s:0 t with
        | `Mon -> 0
        | `Tue -> 1
        | `Wed -> 2
        | `Thu -> 3
        | `Fri -> 4
        | `Sat -> 5
        | `Sun -> 6
      in
      (* Subtract days_since_monday to reach Monday, then take the date.
         Ptime.sub_span on a valid Ptime.t with a 0-6 day offset always
         succeeds (cannot underflow Ptime.min). The None branch is
         structurally unreachable but propagated as Error rather than
         unwrapped. *)
      let span = Ptime.Span.of_int_s (days_since_monday * 24 * 60 * 60) in
      match Ptime.sub_span t span with
      | Some monday ->
          let year, month, day = Ptime.to_date monday in
          Ok { year; month; day; hour = 0 }
      | None ->
          Error
            (Format.asprintf "internal: failed to compute Monday for %a"
               (Ptime.pp_rfc3339 ()) t))

(* Reconstruct a Ptime.t from a bucket key. Returns option because
   Ptime.of_date_time validates the date-time tuple. In practice, bucket
   keys are always derived from valid Ptime.t values with only time
   components zeroed, so None is structurally unreachable — but we
   propagate the option rather than unwrapping it. *)
let ptime_of_bucket_key key =
  Ptime.of_date_time ((key.year, key.month, key.day), ((key.hour, 0, 0), 0))

let aggregate agg slice =
  match agg with
  | `First -> Nx.item [ 0 ] slice
  | `Last -> Nx.item [ Nx.numel slice - 1 ] slice
  | `Sum -> Nx.sum slice |> Nx.item []
  | `Mean -> Nx.mean slice |> Nx.item []
  | `Min -> Nx.min slice |> Nx.item []
  | `Max -> Nx.max slice |> Nx.item []

let ( let* ) = Result.bind

let resample : type src target b.
    agg:[ `First | `Last | `Sum | `Mean | `Min | `Max ] ->
    target Freq.t ->
    (src, (float, b) Nx.t) Series.t ->
    ((target, (float, Bigarray.float64_elt) Nx.t) Series.t, string) result =
 fun ~agg target_freq series ->
  let src_freq = Index.freq (Series.index series) in
  if freq_rank target_freq <= freq_rank src_freq then
    Error
      (Printf.sprintf "cannot resample from %s to %s: target must be lower"
         (freq_name src_freq) (freq_name target_freq))
  else
    let len = Series.length series in
    if len = 0 then
      let idx = Index.of_ptime_array_unsafe target_freq [||] in
      let vals = Nx.create Nx.float64 [| 0 |] [||] in
      Ok (Series.make_unsafe idx vals)
    else
      let ts = Index.timestamps (Series.index series) in
      let vals = Series.values series in
      (* Mutable refs + imperative for-loop: a single O(n) pass over sorted
         timestamps, grouping consecutive entries by bucket key. Mutation is
         used here because the scan maintains four pieces of evolving state
         (current key, group start, accumulated keys, accumulated values)
         that update conditionally — a fold would require threading a
         four-element tuple through each iteration with no clarity gain. *)
      let bucket_keys = ref [] in
      let bucket_values = ref [] in
      let error = ref None in
      let current_key =
        match bucket_key_of_ptime target_freq ts.(0) with
        | Ok k -> ref k
        | Error e ->
            error := Some e;
            ref { year = 0; month = 0; day = 0; hour = 0 }
      in
      let group_start = ref 0 in
      if Option.is_none !error then begin
        for i = 1 to len - 1 do
          if Option.is_none !error then
            match bucket_key_of_ptime target_freq ts.(i) with
            | Error e -> error := Some e
            | Ok key ->
                if not (bucket_key_equal key !current_key) then begin
                  let slice = Nx.slice [ R (!group_start, i) ] vals in
                  bucket_keys := !current_key :: !bucket_keys;
                  bucket_values := aggregate agg slice :: !bucket_values;
                  current_key := key;
                  group_start := i
                end
        done;
        if Option.is_none !error then begin
          let slice = Nx.slice [ R (!group_start, len) ] vals in
          bucket_keys := !current_key :: !bucket_keys;
          bucket_values := aggregate agg slice :: !bucket_values
        end
      end;
      match !error with
      | Some e -> Error e
      | None ->
          let keys = List.rev !bucket_keys in
          let agg_vals = Array.of_list (List.rev !bucket_values) in
          let n_buckets = Array.length agg_vals in
          (* Convert bucket keys to Ptime.t array. ptime_of_bucket_key returns
             option; propagate as Error if any key fails (structurally
             unreachable but keeps the function total and exception-free). *)
          let boundaries = Array.make n_buckets Ptime.epoch in
          let rec convert_keys i = function
            | [] -> Ok ()
            | key :: rest ->
                let* t =
                  ptime_of_bucket_key key
                  |> Option.to_result
                       ~none:
                         (Printf.sprintf
                            "internal: failed to reconstruct boundary for \
                             %04d-%02d-%02dT%02d:00:00Z"
                            key.year key.month key.day key.hour)
                in
                boundaries.(i) <- t;
                convert_keys (i + 1) rest
          in
          let* () = convert_keys 0 keys in
          let idx = Index.of_ptime_array_unsafe target_freq boundaries in
          let values = Nx.create Nx.float64 [| n_buckets |] agg_vals in
          Ok (Series.make_unsafe idx values)
