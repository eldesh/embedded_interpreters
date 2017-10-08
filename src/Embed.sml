
(**
 * Embedded / Projection pairs combinator
 *)
structure Embed :> EMBED =
struct
local
  (* open OS.Process *)
  structure U = Univ
  structure EpT = EpType
  open Util

  fun println str = print (str ^ "\n")
  (* --> workaround for sml/nj *)
  infixr 1 $
  (* <-- workaround for sml/nj *)

  val cat = concat
in
  datatype U = datatype Univ.U
  type 'a EP = ('a->U) * (U->'a) * EpT.t

  fun embed (e,p,_) = e
  fun project (e,p,_) = p
  fun cross (f,g) (x,y) = (f x, g y)
  fun arrow (f,g) h = g o h o f

  fun eptypeof ((_,_,t) : 'a EP) : EpT.t = t

  fun PF (UF(f)) = f
  fun PP (UP(p)) = p
  fun PI (UI(n)) = n
  fun PS (US(s)) = s
  fun PB (UB(b)) = b
  fun PU (UU( )) = ()
  fun PD (UD(d)) = d

  fun newtype () =
    let
      val (tod, fromd) = Dynamic.newdyn ()
    in
      (UD o tod, fromd o PD, EpT.Dynamic)
    end

  fun S x y z = x z (y z)
  fun K x y = x
  fun I x = x

  infixr -->
  infix **
  val bool   = (UB, PB, EpT.Prim "bool"  )
  val unit   = (UU, PU, EpT.Prim "unit"  )
  val int    = (UI, PI, EpT.Prim "int"   )
  val string = (US, PS, EpT.Prim "string")
  val any    = ( I,  I, EpT.Prim "any"   )
  fun (e,p,s)** (e',p',s') = (UP o cross (e,e'), cross (p,p') o PP, EpT.Tuple (s,s'))
  fun (e,p,s)-->(e',p',s') = (UF o arrow (p,e'), arrow (e,p') o PF, EpT.Arrow (s,s'))

  fun wrap (x:'a->'b, y:'b->'a) (ep:'b EP) : 'a EP =
    ((embed ep) o x, y o (project ep), eptypeof ep)

  fun sum ss =
    let fun cases brs n x = UT(n, embed (hd brs) x)
                  handle Match => cases (tl brs) (n+1) x
        val emb = fn x=> cases ss 0 x
        val pro = (fn (UT(n,u)) => project (List.nth(ss,n)) u | _=> raise Match)
    in (emb, pro, (fn (_,_,x)=>x ) (hd ss))
    end

  fun thd (_,_,x) = x
  fun mu f = (fn x => embed (f (mu f)) x, fn u => project (f (mu f)) u, EpT.Prim "mu")

  fun list elem = mu (fn l =>
    sum [ wrap (fn []=>(), fn()=>[]) unit
        , wrap (fn (x::xs)=>(x,xs), op::) (elem ** l)
        ])

end (* local *)
end

