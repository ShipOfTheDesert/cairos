type 'a t = { head : 'a; tail : 'a list }

let make head tail = { head; tail }
let singleton head = { head; tail = [] }

let of_list = function
  | [] -> None
  | x :: xs -> Some { head = x; tail = xs }

let to_list { head; tail } = head :: tail
let hd { head; _ } = head
let length { tail; _ } = 1 + List.length tail
let map f { head; tail } = { head = f head; tail = List.map f tail }
let fold_left f init { head; tail } = List.fold_left f (f init head) tail
let append { head; tail } xs = { head; tail = tail @ xs }
