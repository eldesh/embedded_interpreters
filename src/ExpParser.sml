
structure ExpParser =
struct
local
  structure Lex = ExpLexer
  open Util
  open Parser

  (* --> workaround for sml/nj *)
  infixr 1 $
  infixr 6 <&>
  infixr 6 <&
  infixr 6 &>
  infix  5 <&=>
  infix  5 <?@
  infix  5 <@
  infixr 4 <|>
  infix    <:&>
  (* <-- workaround for sml/nj *)
  (* *)
  type 'a frag = 'a SMLofNJ.frag
  datatype z = datatype SMLofNJ.frag

  val K = const
in
  datatype Exp = datatype Expr.Exp
  datatype z = datatype ExpLexer.t

  type t = (Exp, Exp frag) Parser.t

  fun isTkNum (TkNum _) = true
    | isTkNum _ = false

  fun isTkStr (TkStr _) = true
    | isTkStr _ = false

  fun isTkId (TkId _) = true
    | isTkId _ = false

  fun isTkExt (TkExt _) = true
    | isTkExt _ = false

  fun number (ss: 'a Lex.t list) : (Exp * 'a Lex.t list) option =
    satisfy2 isTkNum (fn (TkNum num) => EI (LargeInt.toInt num)) $ ss

  fun string (ss: 'a Lex.t list) : (Exp * 'a Lex.t list) option =
    satisfy2 isTkStr (fn (TkStr str) => ES str) $ ss

  fun ident (ss: 'a Lex.t list) : (Exp * 'a Lex.t list) option =
    satisfy2 isTkId (fn (TkId id) => EId id) $ ss

  val ! = symbol

  fun chainL p s =
    p <&> <*> (s <&> p) (* chain left for building syntax tree *)
    <@ (fn (e0,ls) =>
          foldl
            (fn ((e,y),l) => EApp(e, EP(l, y)))
            e0
            ls)

  fun $$ sym id =
    ! sym <@ K (EId id)

  fun value ss =
    (number <|> string <|> ident <|> value_exp) ss

  and value_exp (ss: Exp Lex.t list) : (Exp * Exp Lex.t list) option =
    satisfy2 isTkExt (fn (TkExt exp) => exp) $ ss

  fun expr ss = expr_app <|> expr_single $ ss

  and expr_single (ss: Exp Lex.t list) : (Exp * Exp Lex.t list) option =
    (   pack (symbol TkParenOpen) expr (!TkParenClose)
    <|> expr_if
    <|> expr_let
    <|> expr_lambda
    <|> equals
    <|> tuple
    <|> value
    ) ss

  and tuple ss =
    (pack (!TkParenOpen) (expr <&> (!TkComma &> expr)) (!TkParenClose) <@ EP) ss

  (* binary expr *)
  and equals  ss = chainL sum     ($$ TkEqual "=" <|> $$ TkNotEqual "<>") ss
  and sum     ss = chainL product ($$ TkPlus  "+" <|> $$ TkMinus    "-") ss
  and product ss = chainL value   ($$ TkMult  "*" <|> $$ TkDiv      "/") ss

  (* if expr *)
  and expr_if ss =
    (   (!TkIf &> expr)
    <&> (!TkThen &> expr)
    <&> (!TkElse &> expr)
    ) <@ (fn(cond,(t,f))=> Eif (cond,t,f))
    $ ss

  (* let expr *)
  and expr_let ss =
    let
      val valdef = pack (!TkVal)  ident            (!TkEqual) <&> expr
      val fundef = pack (!TkFun) (ident <&> ident) (!TkEqual) <&> expr
      fun letin e = pack (!TkLet) e (!TkIn)
    in
          (letin valdef <&> expr
             <@ (fn( (EId id,         def),body) => ELet(id, def, body)))
      <|> (letin fundef <&> expr
             <@ (fn(((EId id,EId arg),def),body) => ELetfun(id, arg, def, body)))
      $ ss
    end

  (* lambda expr *)
  and expr_lambda ss =
    let
      val lam = pack (!TkFn) ident (!TkRightArrow)
    in
      lam <&> expr
        <@ (fn (EId id, expr) => ELam (id, expr))
        $ ss
    end

  (* app *)
  and expr_app ss =
    expr_single <&> <+> expr_single
      <@ (fn (f, args) =>
            foldl
              (fn(a,f)=>EApp(f,a))
              f
              args)
      $ ss

  (* ... expr *)


  fun parser_str ss =
    let val tokens = Lex.tokenize' (explode ss)
    in expr tokens end

  fun to_tokens fs =
    let
      fun to_list (    QUOTE ss) = Lex.tokenize' $ explode ss
        | to_list (ANTIQUOTE ee) = [Lex.TkExt ee]
    in
      List.concat $ map to_list fs
    end

  fun parser fs =
    let
      val tks = to_tokens fs
    in
      expr tks
    end

  val read = just parser_str

  val % = just parser



  local
    open SMLUnit
    open Assert
    open Embed Expr ExprInterpreter
    infixr 5 -->
    infix  6 **

    val ($>,%,&,?) = let open Test in (TestLabel, TestCase, TestList, assert) end

    fun assertExpr exp act =
      assertEqual op= Expr.toString exp act
  in
    fun test_xplus1 () =
      let
        val exp = ELam("x",EApp(EId "+",EP(EId "x",EI 1)))
        val act = read "fn x=>x+1"
      in
        assertExpr exp act
      end

    fun test_skk () =
      let
        val exp = EP(EApp(
                       EApp(
                         EApp(EId "S", EId "K"),
                           EId "K"),
                       EI 2),
                     EApp(
                       EApp(
                         EApp(EId "S", EId "K"),
                           EId "K"),
                       ES "two"))
        val act = read "(S K K 2 , S K K \"two\" )"
      in
        assertExpr exp act
      end

    fun test_any () =
      let
        val exp = (3, "four")
        val act =
          let
            val eK = embed (any-->any-->any) K
            val pK = fn a => fn b => project (a-->b-->a) eK
          in
            (pK int string 3 "three", pK string unit "four" ())
          end
      in
        assertEqual2Tuple (assertEqualInt, assertEqualString) exp act
      end

    fun test_polyY () =
      let
        val exp = 120
        val act = 
          let val embY = interpretclosed (read (
                "fn f => (fn g => f (fn a => (g g) a)) "^
                        "(fn g => f (fn a => (g g) a))"))
              val polyY = fn a => fn b => project
                        (((a-->b)-->a-->b)-->a-->b) embY
              val sillyfact = polyY int int
                            (fn f => fn n => if n=0 then 1 else n*(f (n-1)))
          in sillyfact 5 end
      in
        assertEqualInt exp act
      end

    fun leq t p : bool =
      let
        val expr = read (
          "let fun leq p = let val x = fst p in "^
                          "let val y = snd p in "^
                            "if isint x then leqint (x , y) "^
                            "else if isstring x then leqstring (x , y) "^
                            "else if ispair x then "^
                            "      and (leq (fst x , fst y) , leq (snd x , snd y)) "^
                            "else if isbool x then implies x y "^
                            "else if isunit x then true "^
                            "else false "^
                            "in leq")
         val eleq = interpretclosed expr
      in project (t**t-->bool) eleq p end

    fun test_polyeq () : Test.test =
      let val assert = assertEqualBool
      in
        &[
          $>("int  eq", %(fn()=> assert true (leq int (5,5)))),
          $>("int neq", %(fn()=> assert false (leq int (5,4)))),
          $>("str  eq", %(fn()=> assert true (leq string ("foo","foo")))),
          $>("str neq", %(fn()=> assert false (leq string ("foo","bar"))))
        ]
      end

    fun test_umap () =
      let
        val umap = interpretclosed
                     (read
                       (concat ["let fun map f l = if null l then nil "
                               ,"else cons (f (hd l) , map f (tl l)) in map"]))

        val sq = project ((int-->int)-->(list int)-->(list int)) umap
        val exp = [1,4,9]
        val act = sq (fn x=>x*x) [1,2,3]
      in
        assertEqualList assertEqualInt exp act
      end

    fun test_suite () =
      $>("test_suite",
        &[ $>("xplus1", %test_xplus1),
           $>("skk", %test_skk),
           $>("any", %test_any),
           $>("polyeq", test_polyeq()),
           $>("umap", %test_umap)
         ])

    val () = SMLUnit.TextUITestRunner.runTest
             {output = TextIO.stdOut}
             (test_suite ())
  end

end (* local *)
end

