let rolling ~n f series =
  let len = Series.length series in
  let vals = Series.values series in
  let out = Array.make len Float.nan in
  if n >= 1 then
    for i = n - 1 to len - 1 do
      let window = Nx.slice [ R (i - n + 1, i + 1) ] vals in
      out.(i) <- f window
    done;
  let values = Nx.create Nx.float64 [| len |] out in
  Series.make_unsafe (Series.index series) values

let expanding f series =
  let len = Series.length series in
  let vals = Series.values series in
  let out =
    Array.init len (fun i ->
        let window = Nx.slice [ R (0, i + 1) ] vals in
        f window)
  in
  let values = Nx.create Nx.float64 [| len |] out in
  Series.make_unsafe (Series.index series) values

let sma ~n series = rolling ~n (fun w -> Nx.mean w |> Nx.item []) series
let rolling_std ~n series = rolling ~n (fun w -> Nx.std w |> Nx.item []) series
let rolling_min ~n series = rolling ~n (fun w -> Nx.min w |> Nx.item []) series
let rolling_max ~n series = rolling ~n (fun w -> Nx.max w |> Nx.item []) series
