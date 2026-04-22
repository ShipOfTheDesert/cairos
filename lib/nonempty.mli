(** Non-empty list.

    Non-emptiness is structurally true: the type carries a [head] element
    alongside a (possibly empty) [tail], so consumers never need to handle the
    impossible empty case via [assert false] or [Option.get]. The [private]
    modifier permits destructuring by pattern-match on [{ head; tail }] while
    forbidding external construction — all values are built through the smart
    constructors. *)

type 'a t = private { head : 'a; tail : 'a list }

val make : 'a -> 'a list -> 'a t
(** [make head tail] constructs a non-empty list with [head] followed by [tail].
*)

val singleton : 'a -> 'a t
(** [singleton x] is equivalent to [make x []]. *)

val of_list : 'a list -> 'a t option
(** [of_list xs] is [Some] when [xs] is non-empty, [None] otherwise. The one
    honest boundary where a plain list enters the [Nonempty] world. *)

val to_list : 'a t -> 'a list
(** [to_list ne] converts back to a plain list. *)

val hd : 'a t -> 'a
(** [hd ne] returns the head element. Total by construction. *)

val tl : 'a t -> 'a list
(** [tl ne] returns the (possibly empty) tail. Total by construction — symmetric
    to {!hd}, lets callers decompose without the [private]-record pattern-match.
*)

val length : 'a t -> int
(** [length ne] returns the number of elements, always at least [1]. *)
