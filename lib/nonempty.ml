type 'a t = { head : 'a; tail : 'a list }

let make head tail = { head; tail }
let singleton head = { head; tail = [] }

let of_list = function
  | [] -> None
  | x :: xs -> Some { head = x; tail = xs }

let to_list { head; tail } = head :: tail
let hd { head; _ } = head
let tl { tail; _ } = tail
let length { tail; _ } = 1 + List.length tail
