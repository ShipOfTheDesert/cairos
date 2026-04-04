(** Frequency witness GADT — carried at the type level, inspectable at runtime.
*)
type _ t =
  | Day : [ `Daily ] t
  | Minute : [ `Minute ] t
  | Hour : [ `Hour ] t
  | Week : [ `Weekly ] t
