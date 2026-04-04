type ('freq, 'v) t = { index : 'freq Index.t; values : 'v }

let make index values =
  let idx_len = Index.length index in
  let val_len = (Nx.shape values).(0) in
  if idx_len = val_len then Ok { index; values }
  else
    Error
      (Printf.sprintf "index length %d does not match values length %d" idx_len
         val_len)

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

let map f t = { index = t.index; values = f t.values }
