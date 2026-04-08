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

let of_series pairs =
  match pairs with
  | [] -> Error "Frame.of_series: empty series list"
  | (_, first_series) :: rest -> (
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
                    ("Frame.of_series: index mismatch for column \"" ^ name
                   ^ "\"")
          in
          match validate rest with
          | Error _ as e -> e
          | Ok () ->
              Ok
                {
                  index = ref_index;
                  columns = List.map (fun (n, s) -> (n, Series.values s)) pairs;
                }))

let get name frame =
  match List.assoc_opt name frame.columns with
  | None -> None
  | Some values -> Some (Series.make_unsafe frame.index values)

let columns frame = List.map fst frame.columns
