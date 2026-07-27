type 'freq t = {
  index : 'freq Index.t;
  columns : (string * (float, Bigarray.float64_elt) Nx.t) Nonempty.t;
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
              columns =
                Nonempty.map (fun (n, s) -> (n, Series.values s)) pairs_ne;
            })

let get name frame =
  match List.assoc_opt name (Nonempty.to_list frame.columns) with
  | None -> None
  | Some values -> Some (Series.make_unsafe frame.index values)

let columns frame = Nonempty.map fst frame.columns
let index frame = frame.index

let to_series frame =
  Nonempty.map
    (fun (name, values) -> (name, Series.make_unsafe frame.index values))
    frame.columns

(* Output shape is taken from [frame] — same index, same names in the same
   order — so the concrete record literal is built directly rather than
   through [of_series], whose two error cases cannot arise here.

   The column position is threaded by decomposing [frame.columns] into head
   and tail rather than by a counter inside [Nonempty.map]: [map]'s result
   order is guaranteed but the order in which it applies its function is
   not. *)
let mapi_cells ~f frame =
  let n_rows = Index.length frame.index in
  let map_column col (name, values) =
    let src = Nx.to_array values in
    let dst = Array.make n_rows 0.0 in
    for row = 0 to n_rows - 1 do
      dst.(row) <- f ~col ~name ~row src.(row)
    done;
    (name, Nx.create Nx.float64 [| n_rows |] dst)
  in
  let head = map_column 0 (Nonempty.hd frame.columns) in
  let tail =
    List.mapi (fun i c -> map_column (i + 1) c) (Nonempty.tl frame.columns)
  in
  { index = frame.index; columns = Nonempty.make head tail }

let head n frame =
  let len = Index.length frame.index in
  let stop = max 0 (min n len) in
  let index = Index.slice ~start:0 ~stop frame.index in
  let columns =
    Nonempty.map
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
    Nonempty.map
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
    (Nonempty.to_list frame.columns)

(* Cross-sectional operations.

   Each operation traverses the input frame row-by-row. For each row index,
   per-column values are gathered into a single [float array scratch] of
   length [n_cols], allocated once outside the row loop and reused per row.
   The shared scratch buffer is private to the function call (no aliasing of
   input cells).

   Output frames are built via the concrete record literal
   [{ index = frame.index; columns = new_columns }] from inside this module
   — never via [of_series], which returns [result]. The invariants
   [of_series] validates (non-empty, index identity, distinct column names)
   hold by construction at every output: the index is reused verbatim, the
   column-name list mirrors [frame.columns]'s names in order, and
   non-emptiness is carried by [Nonempty.t]. *)

(* Writes the cross-section at row [i] into the caller-allocated [scratch]
   buffer. [Array.length scratch] must equal [List.length cols]; no
   allocation per call, which is why callers hoist [cols] out of the row
   loop rather than re-listing [frame.columns] per row. Internal to
   [frame.ml]; not exposed in [.mli]. *)
let gather_row cols i scratch =
  List.iteri (fun j (_, values) -> scratch.(j) <- Nx.item [ i ] values) cols

let column_map ~f frame =
  (* Imperative row loop — see header above. *)
  let cols = Nonempty.to_list frame.columns in
  let n_cols = Nonempty.length frame.columns in
  let n_rows = Index.length frame.index in
  let scratch = Array.make n_cols 0.0 in
  let out = Array.make n_rows 0.0 in
  for i = 0 to n_rows - 1 do
    gather_row cols i scratch;
    out.(i) <- f scratch
  done;
  let values = Nx.create Nx.float64 [| n_rows |] out in
  Series.make_unsafe frame.index values

let rank frame =
  (* Imperative row loop — see header above. *)
  let cols = Nonempty.to_list frame.columns in
  let n_cols = Nonempty.length frame.columns in
  let n_rows = Index.length frame.index in
  let scratch = Array.make n_cols 0.0 in
  let tie_buf = Array.make n_cols (0, 0.0) in
  let new_columns =
    Nonempty.map
      (fun (name, _) ->
        (name, Nx.create Nx.float64 [| n_rows |] (Array.make n_rows Float.nan)))
      frame.columns
  in
  let out_tensors =
    Array.of_list (List.map snd (Nonempty.to_list new_columns))
  in
  for i = 0 to n_rows - 1 do
    gather_row cols i scratch;
    let non_nan_count = ref 0 in
    for j = 0 to n_cols - 1 do
      let v = scratch.(j) in
      if not (Float.is_nan v) then begin
        tie_buf.(!non_nan_count) <- (j, v);
        incr non_nan_count
      end
    done;
    let n = !non_nan_count in
    (* Stable sort the non-NaN prefix ascending by value. Stability isn't
       required for correctness — equal-valued cells collapse into a
       single run that all receive the same average rank. *)
    let prefix = Array.sub tie_buf 0 n in
    Array.stable_sort (fun (_, a) (_, b) -> Float.compare a b) prefix;
    let p = ref 0 in
    while !p < n do
      let _, v_run = prefix.(!p) in
      let q = ref !p in
      while !q < n && Float.equal (snd prefix.(!q)) v_run do
        incr q
      done;
      let k = !q - !p in
      let rank_value =
        ((Float.of_int !p +. Float.of_int (!p + k - 1)) /. 2.0) +. 1.0
      in
      for r = !p to !q - 1 do
        let orig_col, _ = prefix.(r) in
        Nx.set_item [ i ] rank_value out_tensors.(orig_col)
      done;
      p := !q
    done
  done;
  { index = frame.index; columns = new_columns }

let zscore frame =
  (* Two-pass kernel; imperative row loop. *)
  let cols = Nonempty.to_list frame.columns in
  let n_cols = Nonempty.length frame.columns in
  let n_rows = Index.length frame.index in
  let scratch = Array.make n_cols 0.0 in
  let new_columns =
    Nonempty.map
      (fun (name, _) ->
        (name, Nx.create Nx.float64 [| n_rows |] (Array.make n_rows Float.nan)))
      frame.columns
  in
  let out_tensors =
    Array.of_list (List.map snd (Nonempty.to_list new_columns))
  in
  for i = 0 to n_rows - 1 do
    gather_row cols i scratch;
    let n = ref 0 in
    let sum = ref 0.0 in
    for j = 0 to n_cols - 1 do
      let v = scratch.(j) in
      if not (Float.is_nan v) then begin
        incr n;
        sum := !sum +. v
      end
    done;
    if !n >= 2 then begin
      let mean = !sum /. Float.of_int !n in
      let ss = ref 0.0 in
      for j = 0 to n_cols - 1 do
        let v = scratch.(j) in
        if not (Float.is_nan v) then ss := !ss +. ((v -. mean) *. (v -. mean))
      done;
      (* ddof=1; matches Cairos_finance.annualised_vol. *)
      let std = Float.sqrt (!ss /. Float.of_int (!n - 1)) in
      if not (Float.equal std 0.0) then
        for j = 0 to n_cols - 1 do
          let v = scratch.(j) in
          if not (Float.is_nan v) then
            Nx.set_item [ i ] ((v -. mean) /. std) out_tensors.(j)
        done
    end
  done;
  { index = frame.index; columns = new_columns }
