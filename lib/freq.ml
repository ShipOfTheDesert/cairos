type _ t =
  | Day : [ `Daily ] t
  | Minute : [ `Minute ] t
  | Hour : [ `Hour ] t
  | Week : [ `Weekly ] t
