
structure Builtins :>
sig
  val lookup : string -> Univ.U
end =
struct
local
  open Embed
  open Util
  (* --> workaround for sml/nj *)
  infixr 5 -->
  infix  6 **
  (* <-- workaround for sml/nj *)
in
  fun iter m f n =
    if n=0
    then m
    else f n (iter m f (n-1))

  val combinators =
    [("I", embed (any-->any) I),
     ("K", embed (any-->any-->any) K),
     ("S", embed ((any-->any-->any)-->(any-->any)-->any-->any) S)]

  val typeof =
    [("isfunction", embed (any-->bool) (fn UF _=>true | _=>false)),
     ("ispair"    , embed (any-->bool) (fn UP _=>true | _=>false)),
     ("isunit"    , embed (any-->bool) (fn UU _=>true | _=>false)),
     ("isint"     , embed (any-->bool) (fn UI _=>true | _=>false)),
     ("isstring"  , embed (any-->bool) (fn US _=>true | _=>false)),
     ("isbool"    , embed (any-->bool) (fn UB _=>true | _=>false))]

  val embed_types_op =
    [("cons", embed ((any**(list any))-->(list any)) (op::)),
     ("nil" , embed (list any) []),
     ("null", embed ((list any)-->bool) List.null),
     ("hd"  , embed ((list any)-->any) List.hd),
     ("tl"  , embed ((list any)-->(list any)) List.tl)]

  val basis =
    [("+"        , embed ((int**int)-->int) Int.+ ),
     ("-"        , embed ((int**int)-->int) Int.- ),
     ("*"        , embed ((int**int)-->int) Int.* ),
     ("print"    , embed (string-->unit) print),
     ("toString" , embed (int-->string) Int.toString),
     ("iter"     , embed (int-->(int-->int-->int)-->int-->int) iter),
     ("implies"  , embed (bool-->bool-->bool) (fn p=>fn q=> not p orelse q)),
     ("leqint"   , embed ((int**int)-->bool) op=),
     ("leqstring", embed ((string**string)-->bool) op=),
     ("fst"      , embed ((any**any)-->any) fst),
     ("snd"      , embed ((any**any)-->any) snd)]

  val builtins =
    List.concat [basis, embed_types_op, combinators, typeof]

  fun find x ((y,v)::rest) = if x=y then v
                             else find x rest
    | find x [] = (print ("Not found <function:"^x^">"); raise Match)
  fun lookup s = find s builtins
end (* local *)
end

