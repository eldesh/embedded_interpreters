
structure Expr =
struct
  datatype Exp = EId of string
               | EI of int
               | ES of string
               | EB of bool
               | EApp of Exp * Exp
               | EP of Exp * Exp
               | ELet of string * Exp * Exp
               | Eif of Exp * Exp * Exp
               | ELam of string * Exp
               | ELetfun of string * string * Exp * Exp
  
  local
    val cat = String.concat
  in
  fun toString x =
    let
      fun str (EId id) = "EId "^id
        | str (EI i) = "EI "^(Int.toString i)
        | str (ES s) = "ES "^s
        | str (EB b) = Bool.toString b
        | str (EApp (s,b)) = cat ["EApp(", str s, "," , str b, ")"]
        | str (EP (a,b)) = cat ["EP(", str a, ",", str b, ")"]
        | str (ELet (s,a,b)) = cat ["ELet(", s, ",", str a, ",", str b, ")"]
        | str (Eif (c,t,f)) = cat ["Eif(", str c, ",", str t, ",", str f, ")"]
        | str (ELam (s,e)) = cat ["ELam(", s, ",", str e, ")"]
        | str (ELetfun (s,n,a,b)) =
                cat ["ELetfun(", s, ",", n, ",", str a, ",", str b, ")"]
    in
      str x
    end
  end
  
  fun print exp = TextIO.print (toString exp)
end

