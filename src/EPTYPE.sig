
signature EPTYPE =
sig
  datatype t = Prim of string
             | Dynamic
             | Tuple of t * t
             | Arrow of t * t

  val toString : t -> string
end

