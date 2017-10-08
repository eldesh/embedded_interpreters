
structure Embed :> EMBED =
struct
local
  (* open OS.Process *)
  structure U = Univ
  open Util

  fun println str = print (str ^ "\n")
  (* --> workaround for sml/nj *)
  infixr 1 $
  (* <-- workaround for sml/nj *)

  val cat = concat
in
  datatype U = datatype Univ.U
  datatype EpTree = EpPrim of string
                  | EpDynamic
                  | EpTuple of EpTree * EpTree
                  | EpArrow of EpTree * EpTree
  withtype 'a EP = ('a->U) * (U->'a) * EpTree
  fun embed (e,p,_) = e
  fun project (e,p,_) = p
  fun cross (f,g) (x,y) = (f x, g y)
  fun arrow (f,g) h = g o h o f

  fun eptypeof ((_,_,t) : 'a EP) : EpTree = t
  fun EpTreeToString (x:EpTree) : string =
    let
      fun tree2str (EpPrim s) = s
        | tree2str EpDynamic  = "<Dynamic>"
        | tree2str (EpTuple (x,y)) = String.concat ["(",tree2str x,"," ,tree2str y,")"]
        | tree2str (EpArrow (x,y)) = String.concat ["(",tree2str x,"->",tree2str y,")"]
    in tree2str x
    end

  fun PF (UF(f)) = f before println "PF(f)"
    | PF _       = (print "PF(?)\n"; raise Match)
  fun PP (UP(p)) = p before println $ cat ["PP(", U.toString (UP p), ")"]
    | PP _       = (print "PP(?)\n"; raise Match)
  fun PI (UI(n)) = n before println $ cat ["PI(", Int.toString n, ")"]
    | PI _       = (print "PI(?)\n"; raise Match)
  fun PS (US(s)) = s before println $ cat ["PS(", s, ")"]
    | PS _       = (print "PS(?)\n"; raise Match)
  fun PB (UB(b)) = b before println $ cat ["PB(", Bool.toString b, ")"]
    | PB _       = (print "PB(?)\n"; raise Match)
  fun PU (UU( )) = () before println $ "PU( )"
    | PU _       = (print "PU(?)\n"; raise Match)
  fun PD (UD(d)) = d before print "PD(?)"

  (* dynamic type declaration *)
  fun newtype () = let val (tod,fromd) = Dynamic.newdyn () 
                   in (UD o tod, fromd o PD, EpDynamic)
                   end

  fun S x y z = x z (y z)
  fun K x y = x
  fun I x = x

  infixr --> infix **
  val bool   = (UB, PB, EpPrim "bool"  )
  val unit   = (UU, PU, EpPrim "unit"  )
  val int    = (UI, PI, EpPrim "int"   )
  val string = (US, PS, EpPrim "string")
  val any    = ( I,  I, EpPrim "any"   )
  fun (e,p,s)** (e',p',s') = (UP o cross (e,e'), cross (p,p') o PP, EpTuple (s,s'))
  fun (e,p,s)-->(e',p',s') = (UF o arrow (p,e'), arrow (e,p') o PF, EpArrow (s,s'))

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
  fun mu f = (fn x => embed (f (mu f)) x, fn u => project (f (mu f)) u, EpPrim "mu")

  fun list elem = mu (fn l =>
    sum [ wrap (fn []=>(), fn()=>[]) unit
        , wrap (fn (x::xs)=>(x,xs), op::) (elem ** l)
        ])
end (* local *)
end

