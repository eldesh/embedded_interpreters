
structure EpType :> EPTYPE =
struct
  datatype t = Prim of string
             | Dynamic
             | Tuple of t * t
             | Arrow of t * t

  fun toString x =
    let
      fun str (Prim s) = s
        | str  Dynamic = "<dyn>"
        | str (Tuple (x,y)) = concat ["(", str x, ",", str y, ")"]
        | str (Arrow (x,y)) = concat ["(", str x, "->", str y, ")"]
    in
      str x
    end
end

