
structure ExprInterpreter :> EXPR_INTERPRETER =
struct
local
  structure IO = TextIO
  open Util
  open Dynamic
  open Univ
  open Expr
  open Embed
in
  type Exp = Expr.Exp
  type U = Univ.U

  type staticenv  = string list
  type dynamicenv = U list

  fun indexof (name::names, x) =
    if x = name
    then 0
    else 1 + indexof(names, x)

  fun interpret (e, static) =
    case e of
         EI n => (fn dyn => (UI n))
       | ES s => (fn dyn => (US s))
       | EB b => (fn dyn => (UB b))
       | EP (e1,e2) => let val e1' = interpret (e1,static)
                           val e2' = interpret (e2,static)
                       in fn dyn => (UP (e1' dyn, e2' dyn))
                       end
       | EId s => (let val n = indexof (static, s)
                   in fn dyn => List.nth (dyn, n)
                   end
                   handle Match =>
                     let val lib = Builtins.lookup s
                     in fn dyn => lib end)
       | EApp (e1,e2) => let val s1 = interpret (e1,static) 
                             val s2 = interpret (e2,static)
                         in fn dyn => let val UF(f) = s1 dyn
                                              val a = s2 dyn
                                          in f a
                                          end
                         end
         (* let x=e1 in e2 *)
       | ELet (x,e1,e2) => let val s1 = interpret (e1, static)
                               val s2 = interpret (e2, x::static)
                           in fn dyn => let val g = s1 dyn
                                            in s2 (g::dyn)
                                            end
                           end
       | Eif (cond,t,f) => let val s = interpret (cond,static)
                           in fn dyn => case s dyn of
                                                 UB b => interpret ((if b then t else f), static) dyn
                                               | _    => (IO.print "failure :(";OS.Process.exit OS.Process.failure)
                           end
       | ELam (f, e) => let val s = interpret (e, f::static)
                        in fn dyn => let fun g v = s (v::dyn)
                                         in UF g end
                        end
         (* let f x=e1 in e2 *)
       | ELetfun (f,x,e1,e2) =>
                          let val s1 = interpret (e1, x::f::static)
                              val s2 = interpret (e2, f::static)
                          in fn dyn => let fun g v = s1 (v::UF(g)::dyn)
                                           in s2 (UF(g)::dyn)
                                           end
                          end

  (* evaluate expression on empty environment *)
  fun interpretclosed e = interpret (e, []) []

  local
    open SMLUnit
    open Assert
    structure U = Univ

    val ($,%,&,?) = let open Test in (TestLabel, TestCase, TestList, assert) end

    val eval = interpret
    val eval' = interpretclosed

    fun partialEqU (u1, u2) =
      case (u1, u2)
        of (UU (), UU ()) => () = ()
         | (UI i1, UI i2) => i1 = i2
         | (US s1, US s2) => s1 = s2
         | (UB b1, UB b2) => b1 = b2
         | (UP(v11,v12), UP(v21,v22)) => partialEqU (v11,v21) andalso
                                         partialEqU (v12,v22)
         | (UT(t1,u1), UT(t2,u2)) => t1=t2 andalso partialEqU (u1, u2)
         | _ => failByNotEqual (U.toString u1, U.toString u2)

    fun assertU exp act =
      assertEqual partialEqU U.toString exp act
  in
    fun testNegate () =
      let
        val negate_true = ELetfun
                          ("negate",
                           "cond",
                           Eif (EId "cond", EB false, EB true),
                           EApp (EId "negate", EB true))
        val negate_false = ELetfun
                          ("negate",
                           "cond",
                           Eif (EId "cond", EB false, EB true),
                           EApp (EId "negate", EB false))
        val negate       = ELetfun ("negate", "cond",
                           Eif (EId "cond", EB false, EB true),
                           EApp (EId "negate", EId "x"))
      in
        $("negate",
          &[ %(fn()=> assertU (UB false) (eval' negate_true )),
             %(fn()=> assertU (UB  true) (eval' negate_false)),
             %(fn()=> assertU (UB false) (eval (negate,["x"]) [UB true])),
             %(fn()=> assertU (UB  true) (eval (negate,["x"]) [UB false]))
          ])
      end

    fun testSquare () =
      let
        (* let fun sq arg = arg * arg *)
        fun square x = ELetfun
                       ("sq",
                        "arg",
                        EApp
                          (EId "*",
                           EP(EId "arg", EId "arg")),
                        EApp
                          (EId "sq", EI x))
      in
        $("square",
          &[ %(fn()=> assertU (UI 900) (eval' (square 30))),
             %(fn()=> assertU (UI   0) (eval' (square  0))),
             %(fn()=> assertU (UI   1) (eval' (square  1)))
          ])
      end

    fun testLambda () =
      let
        fun mult x y = EApp
                        (EApp
                          (ELam("x",
                            ELam("y",
                              EApp(EId "*",
                                EP(EId "x",EId "y")))),
                           EI x),
                         EI y)
      in
        $("lambda",
          &[ %(fn()=> assertU (UI (8*5)) (eval' (mult 8  5))),
             %(fn()=> assertU (UI (0*42)) (eval' (mult 0 42)))
          ])
      end

    fun test_suite () =
      $("ExprInterpreter",
        &[ testNegate(),
           testSquare(),
           testLambda()
         ])

    val () = SMLUnit.TextUITestRunner.runTest
             {output = TextIO.stdOut}
             (test_suite ())
  end

  end (* local *)
end

