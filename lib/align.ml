type ('freq, 'a, 'b) aligned = { index : 'freq Index.t; left : 'a; right : 'b }

let index t = t.index
let left t = t.left
let right t = t.right

let align_inner left right =
  let l_idx = Series.index left in
  let r_idx = Series.index right in
  let l_ts = Index.timestamps l_idx in
  let r_ts = Index.timestamps r_idx in
  let l_len = Array.length l_ts in
  let r_len = Array.length r_ts in
  (* O(n + m) sorted merge — collect matching position pairs, accumulate
     in reverse then List.rev once (see quadratic-list-append solution) *)
  let rec loop i j acc_ts acc_li acc_ri =
    if i >= l_len || j >= r_len then (acc_ts, acc_li, acc_ri)
    else
      let cmp = Ptime.compare l_ts.(i) r_ts.(j) in
      if cmp < 0 then loop (i + 1) j acc_ts acc_li acc_ri
      else if cmp > 0 then loop i (j + 1) acc_ts acc_li acc_ri
      else loop (i + 1) (j + 1) (l_ts.(i) :: acc_ts) (i :: acc_li) (j :: acc_ri)
  in
  let ts_rev, li_rev, ri_rev = loop 0 0 [] [] [] in
  match ts_rev with
  | [] -> Error "empty intersection: no common timestamps"
  | _ ->
      let ts = Array.of_list (List.rev ts_rev) in
      let l_positions = Array.of_list (List.rev li_rev) in
      let r_positions = Array.of_list (List.rev ri_rev) in
      let freq = Index.freq l_idx in
      (* Safety: ts extracted from valid Index.t values in sorted order *)
      let index = Index.of_ptime_array_unsafe freq ts in
      let to_idx arr =
        Nx.create Nx.int32 [| Array.length arr |] (Array.map Int32.of_int arr)
      in
      let left_nx = Nx.take (to_idx l_positions) (Series.values left) in
      let right_nx = Nx.take (to_idx r_positions) (Series.values right) in
      Ok { index; left = left_nx; right = right_nx }

let align_left left right =
  let l_idx = Series.index left in
  let r_idx = Series.index right in
  let l_ts = Index.timestamps l_idx in
  let r_ts = Index.timestamps r_idx in
  let l_len = Array.length l_ts in
  let r_len = Array.length r_ts in
  let right_nx = Series.values right in
  let right_arr = Nx.to_array right_nx in
  (* NaN-filled tensor preserving the right tensor's element type *)
  let result_right = Nx.full (Nx.dtype right_nx) [| l_len |] Float.nan in
  (* O(n + m) sorted scan: for each left timestamp, advance right pointer
     looking for exact match. Both arrays are strictly monotonic. *)
  let j = ref 0 in
  for i = 0 to l_len - 1 do
    while !j < r_len && Ptime.compare r_ts.(!j) l_ts.(i) < 0 do
      j := !j + 1
    done;
    if !j < r_len && Ptime.compare r_ts.(!j) l_ts.(i) = 0 then begin
      Nx.set_item [ i ] right_arr.(!j) result_right;
      j := !j + 1
    end
  done;
  (* Safety: l_ts extracted from valid Index.t via Index.timestamps; order preserved *)
  let index = Index.of_ptime_array_unsafe (Index.freq l_idx) l_ts in
  Ok { index; left = Series.values left; right = result_right }

let align_asof direction left right =
  let l_idx = Series.index left in
  let r_idx = Series.index right in
  let l_ts = Index.timestamps l_idx in
  let r_ts = Index.timestamps r_idx in
  let l_len = Array.length l_ts in
  let r_len = Array.length r_ts in
  let right_nx = Series.values right in
  let right_arr = Nx.to_array right_nx in
  let result_right = Nx.full (Nx.dtype right_nx) [| l_len |] Float.nan in
  (match direction with
  | `Backward ->
      (* For each left timestamp, find largest right timestamp <= it.
         Advancing pointer: j tracks how far we've scanned into right. *)
      let j = ref 0 in
      for i = 0 to l_len - 1 do
        while !j < r_len && Ptime.compare r_ts.(!j) l_ts.(i) <= 0 do
          j := !j + 1
        done;
        (* j now points past the last right timestamp <= l_ts.(i).
           If j > 0, then r_ts.(j-1) <= l_ts.(i) is our match. *)
        if !j > 0 then Nx.set_item [ i ] right_arr.(!j - 1) result_right
      done
  | `Forward ->
      (* For each left timestamp, find smallest right timestamp >= it.
         Advancing pointer: j tracks how far we've scanned into right. *)
      let j = ref 0 in
      for i = 0 to l_len - 1 do
        while !j < r_len && Ptime.compare r_ts.(!j) l_ts.(i) < 0 do
          j := !j + 1
        done;
        (* j now points at the first right timestamp >= l_ts.(i), or past end. *)
        if !j < r_len then Nx.set_item [ i ] right_arr.(!j) result_right
      done);
  (* Safety: l_ts extracted from valid Index.t via Index.timestamps; order preserved *)
  let index = Index.of_ptime_array_unsafe (Index.freq l_idx) l_ts in
  Ok { index; left = Series.values left; right = result_right }

let align ~strategy left right =
  match strategy with
  | `Inner -> align_inner left right
  | `Left -> align_left left right
  | `Asof dir -> align_asof dir left right

let map2 f aligned =
  let left_arr = Nx.to_array aligned.left in
  let right_arr = Nx.to_array aligned.right in
  let len = Array.length left_arr in
  let result = Array.init len (fun i -> f left_arr.(i) right_arr.(i)) in
  let values = Nx.create Nx.float64 [| len |] result in
  (* Safety: aligned.index length equals array length by construction —
     the aligned type guarantees this invariant. *)
  Series.make_unsafe aligned.index values
