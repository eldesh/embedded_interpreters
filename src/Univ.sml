
structure Univ =
struct
  datatype U = UF of U -> U
             | UP of U * U
             | UU of unit
             | UI of int
             | US of string
             | UB of bool
             | UT of int * U
             | UD of Dynamic.dyn
  
  local
    val cat = String.concat
  in
  fun toString u =
    let
      fun tos (UF f)     = "UF <fn>"
        | tos (UP (x,y)) = cat ["(", tos x, ",", tos y, ")"]
        | tos (UI x)     = cat ["UI(", Int.toString x, ")"]
        | tos (US s)     = cat ["US(", s, ")"]
        | tos (UB b)     = cat ["UB(", Bool.toString b, ")"]
        | tos (UU ())    = "UU ()"
        | tos (UT (n,u)) = cat ["UT(", Int.toString n, ",", tos u,")"]
        | tos (UD d)     = "UD <dyn>"
    in
      tos u
    end
  end
end

