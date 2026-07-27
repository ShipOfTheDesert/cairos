(** Frequency witness GADT — carried at the type level, inspectable at runtime.
*)
type _ t =
  | Day : [ `Daily ] t
  | Minute : [ `Minute ] t
  | Hour : [ `Hour ] t
  | Week : [ `Weekly ] t
  | Month : [ `Monthly ] t

(** A frequency witness with its index type hidden, so that a non-parametric
    type can carry one. Error variants are the motivating case: an [err] naming
    the frequencies it rejected cannot be parameterised over them. Unwrap [Any]
    to recover a witness the compiler will refine constructor by constructor. *)
type any = Any : _ t -> any
