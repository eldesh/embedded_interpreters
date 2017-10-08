
structure ExpLexer =
struct
local
  open Util
  open Parser
  (* --> workaround for sml/nj *)
  infixr 6 <&>
  infixr 6 <&
  infixr 6 &>
  infix  5 <&=>
  infix  5 <?@
  infix  5 <@
  infixr 4 <|>
  infix    <:&>
  (* <-- workaround for sml/nj *)
  val K = const
in
  type Exp = Expr.Exp
  datatype 'a t = TkExt of 'a
                | TkNum of LargeInt.int
                | TkStr of string
                | TkId of string
                | TkParenOpen  (* ( *)
                | TkParenClose (* ) *)
                | TkRightArrow (* => *)
                | TkPlus (* + *)
                | TkMult (* * *)
                | TkMinus (* - *)
                | TkDiv (* / *)
                | TkEqual (* = *)
                | TkNotEqual (* <> *)
                | TkComma (* , *)
                | TkFun (* fun *)
                | TkFn (* fn *)
                | TkLet (* let *)
                | TkIf (* if *)
                | TkThen (* then *)
                | TkElse (* else *)
                | TkVal (* val *)
                | TkIn (* in *)

  val keyword (* : ('a t, char) Parser.t list *) =
    let
      fun bekey (tok, sym) : (Expr.Exp t, char) Parser.t =
        token tok <& spaces <@ (K sym)
    in
      map bekey [
        ("fun", TkFun),
        ("fn", TkFn),
        ("let", TkLet),
        ("if", TkIf),
        ("then", TkThen),
        ("else", TkElse),
        ("val", TkVal),
        ("let", TkLet),
        ("in", TkIn)
      ]
    end

  val number : (Exp t, char) Parser.t = number <@ TkNum

  val exp_token (* : (unit t, char) Parser.t *) =
        (choice' keyword)
    <|> (number )
    <|> (string <@ TkStr)
    <|> (ident  <@ TkId)
    <|> (token "=>" <@ K TkRightArrow)
    <|> (symbol #"(" <@ K TkParenOpen)
    <|> (symbol #")" <@ K TkParenClose)
    <|> (symbol #"+" <@ K TkPlus)
    <|> (symbol #"*" <@ K TkMult)
    <|> (symbol #"-" <@ K TkMinus)
    <|> (symbol #"/" <@ K TkDiv)
    <|> (symbol #"=" <@ K TkEqual)
    <|> (token "<>" <@ K TkNotEqual)
    <|> (symbol #"," <@ K TkComma)

  fun tokenize ss (* : (char list) * ('a t list) *) =
    case (<*> space &> exp_token) ss
      of NONE => (ss, [])
       | SOME(tk, ss') =>
           let val (ss'', tks) = tokenize ss'
           in (ss'', tk::tks) end

  fun tokenize' ss (* : unit t list *) =
    let
      val (rest, r) = tokenize ss
    in
      if not (null rest)
      then raise Fail ("lexer:" ^ implode rest)
      else r
    end

end (* local *)
end

