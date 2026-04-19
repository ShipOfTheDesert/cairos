type ('freq, 'v) t = { index : 'freq Index.t; values : 'v }

let make index values =
  let idx_len = Index.length index in
  let val_len = (Nx.shape values).(0) in
  if idx_len = val_len then Ok { index; values }
  else
    Error
      (Printf.sprintf "index length %d does not match values length %d" idx_len
         val_len)

let make_unsafe index values = { index; values }
let index t = t.index
let values t = t.values
let length t = Index.length t.index

let slice ~start ~stop t =
  let len = length t in
  let start' = max 0 (min start len) in
  let stop' = max 0 (min stop len) in
  let index = Index.slice ~start:start' ~stop:stop' t.index in
  let values = Nx.slice [ R (start', stop') ] t.values in
  { index; values }

let head n t = slice ~start:0 ~stop:(max 0 n) t

let tail n t =
  let len = length t in
  slice ~start:(len - max 0 n) ~stop:len t

let map f t = { index = t.index; values = f t.values }

let shift n t =
  let len = length t in
  if n = 0 then { index = t.index; values = Nx.copy t.values }
  else
    let dst = Nx.full_like t.values Float.nan in
    let abs_n = abs n in
    if abs_n < len then
      if n > 0 then
        Nx.set_slice [ R (n, len) ] dst (Nx.slice [ R (0, len - n) ] t.values)
      else
        Nx.set_slice
          [ R (0, len - abs_n) ]
          dst
          (Nx.slice [ R (abs_n, len) ] t.values);
    { index = t.index; values = dst }

let pct_change t =
  let prev = shift 1 t in
  let diff = Nx.sub t.values prev.values in
  let result = Nx.div diff prev.values in
  { index = t.index; values = result }

let ffill t =
  let arr = Nx.to_array t.values in
  let len = Array.length arr in
  let out = Array.copy arr in
  let last = ref Float.nan in
  for i = 0 to len - 1 do
    if Float.is_nan out.(i) then (
      if not (Float.is_nan !last) then out.(i) <- !last)
    else last := out.(i)
  done;
  let values = Nx.create (Nx.dtype t.values) [| len |] out in
  { index = t.index; values }

let first_valid t =
  let arr = Nx.to_array t.values in
  let len = Array.length arr in
  let rec loop i =
    if i >= len then None
    else if Float.is_nan arr.(i) then loop (i + 1)
    else Some (i, arr.(i))
  in
  loop 0

let bfill t =
  let arr = Nx.to_array t.values in
  let len = Array.length arr in
  let out = Array.copy arr in
  let next = ref Float.nan in
  for i = len - 1 downto 0 do
    if Float.is_nan out.(i) then (
      if not (Float.is_nan !next) then out.(i) <- !next)
    else next := out.(i)
  done;
  let values = Nx.create (Nx.dtype t.values) [| len |] out in
  { index = t.index; values }

let scan f init t =
  let arr = Nx.to_array t.values in
  let len = Array.length arr in
  let out = Array.make len 0.0 in
  let acc = ref init in
  for i = 0 to len - 1 do
    acc := f !acc arr.(i);
    out.(i) <- !acc
  done;
  let values = Nx.create Nx.float64 [| len |] out in
  { index = t.index; values }

let cumsum t = scan ( +. ) 0.0 t
let cumprod t = scan ( *. ) 1.0 t

let dropna t =
  let arr = Nx.to_array t.values in
  let n = Array.length arr in
  let ts_in = Index.timestamps t.index in
  let k = ref 0 in
  for i = 0 to n - 1 do
    if not (Float.is_nan arr.(i)) then incr k
  done;
  let out_len = !k in
  let positions = Array.make out_len 0l in
  let ts_out = Array.make out_len Ptime.epoch in
  let j = ref 0 in
  for i = 0 to n - 1 do
    if not (Float.is_nan arr.(i)) then begin
      positions.(!j) <- Int32.of_int i;
      ts_out.(!j) <- ts_in.(i);
      incr j
    end
  done;
  let pos_nx = Nx.create Nx.int32 [| out_len |] positions in
  let values = Nx.take pos_nx t.values in
  let index = Index.of_ptime_array_unsafe (Index.freq t.index) ts_out in
  { index; values }
