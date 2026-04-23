type 'freq t = {
  index : 'freq Index.t;
  columns : (string * (float, Bigarray.float64_elt) Nx.t) list;
}

let indices_equal (a : _ Index.t) (b : _ Index.t) : bool =
  let ts_a = Index.timestamps a in
  let ts_b = Index.timestamps b in
  let len_a = Array.length ts_a in
  let len_b = Array.length ts_b in
  len_a = len_b
  &&
  let rec loop i =
    i >= len_a || (Ptime.equal ts_a.(i) ts_b.(i) && loop (i + 1))
  in
  loop 0

let has_duplicate_names pairs =
  let rec check seen = function
    | [] -> None
    | (name, _) :: tl ->
        if List.mem name seen then Some name else check (name :: seen) tl
  in
  check [] pairs

let of_series pairs_ne =
  let _, first_series = Nonempty.hd pairs_ne in
  let rest = Nonempty.tl pairs_ne in
  let pairs = Nonempty.to_list pairs_ne in
  match has_duplicate_names pairs with
  | Some name ->
      Error ("Frame.of_series: duplicate column name \"" ^ name ^ "\"")
  | None -> (
      let ref_index = Series.index first_series in
      let rec validate = function
        | [] -> Ok ()
        | (name, s) :: tl ->
            if indices_equal ref_index (Series.index s) then validate tl
            else
              Error
                ("Frame.of_series: index mismatch for column \"" ^ name ^ "\"")
      in
      match validate rest with
      | Error _ as e -> e
      | Ok () ->
          Ok
            {
              index = ref_index;
              columns = List.map (fun (n, s) -> (n, Series.values s)) pairs;
            })

let get name frame =
  match List.assoc_opt name frame.columns with
  | None -> None
  | Some values -> Some (Series.make_unsafe frame.index values)

let columns frame = List.map fst frame.columns

let head n frame =
  let len = Index.length frame.index in
  let stop = max 0 (min n len) in
  let index = Index.slice ~start:0 ~stop frame.index in
  let columns =
    List.map
      (fun (name, values) -> (name, Nx.slice [ R (0, stop) ] values))
      frame.columns
  in
  { index; columns }

let tail n frame =
  let len = Index.length frame.index in
  let n' = max 0 (min n len) in
  let start = len - n' in
  let index = Index.slice ~start ~stop:len frame.index in
  let columns =
    List.map
      (fun (name, values) -> (name, Nx.slice [ R (start, len) ] values))
      frame.columns
  in
  { index; columns }

type column_stats = {
  count : int;
  mean : float;
  std : float;
  min : float;
  p25 : float;
  median : float;
  p75 : float;
  max : float;
}

let quantile q sorted len =
  let h = Float.of_int (len - 1) *. q in
  let lo = int_of_float (floor h) in
  let hi = min (lo + 1) (len - 1) in
  let frac = h -. Float.of_int lo in
  (sorted.(lo) *. (1.0 -. frac)) +. (sorted.(hi) *. frac)

let describe frame =
  List.map
    (fun (name, values) ->
      let arr = Nx.to_array values in
      let valid =
        Array.of_list
          (Array.fold_right
             (fun v acc -> if Float.is_nan v then acc else v :: acc)
             arr [])
      in
      let count = Array.length valid in
      let stats =
        if count = 0 then
          {
            count = 0;
            mean = Float.nan;
            std = Float.nan;
            min = Float.nan;
            p25 = Float.nan;
            median = Float.nan;
            p75 = Float.nan;
            max = Float.nan;
          }
        else begin
          (* Intentional in-place sort on a locally-created array *)
          Array.sort Float.compare valid;
          let sum = Array.fold_left ( +. ) 0.0 valid in
          let mean = sum /. Float.of_int count in
          let sq_sum =
            Array.fold_left
              (fun acc v -> acc +. ((v -. mean) *. (v -. mean)))
              0.0 valid
          in
          let std = Float.sqrt (sq_sum /. Float.of_int count) in
          {
            count;
            mean;
            std;
            min = valid.(0);
            p25 = quantile 0.25 valid count;
            median = quantile 0.5 valid count;
            p75 = quantile 0.75 valid count;
            max = valid.(count - 1);
          }
        end
      in
      (name, stats))
    frame.columns
