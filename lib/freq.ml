type _ t =
  | Day : [ `Daily ] t
  | Minute : [ `Minute ] t
  | Hour : [ `Hour ] t
  | Week : [ `Weekly ] t
  | Month : [ `Monthly ] t

type any = Any : _ t -> any
