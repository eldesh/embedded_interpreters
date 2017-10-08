
structure Parser =
struct
local
  open Util
  val K = const
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
in
  open ParserCore

  val digit_list = satisfy Char.isDigit

  val alpha_lower = satisfy Char.isLower
  val alpha_upper = satisfy Char.isUpper
  val alpha       = satisfy Char.isAlpha
  val alphanum    = satisfy Char.isAlphaNum
  val space       = satisfy Char.isSpace
  val digit       = satisfy Char.isDigit
  val digit1_9    = satisfy (fn c=> c <> #"0" andalso Char.isDigit c)

  val space       = satisfy Char.isSpace
  val spaces      = <+> space

  fun number_general fromString =
        (symbol #"0" <@ K (fromString "0"))
    <|> (digit1_9 <&> <*> digit <@ (fromString o implode o op::))

  val number_str = number_general id
  val number_int = number_general (valOf o Int.fromString)
  val number = number_general (valOf o LargeInt.fromString)

  val string =
    let val notq = satisfy (fn c=> c <> #"\"")
    in symbol #"\"" &> <*> notq <& symbol #"\"" <@ implode end

  val ident =
    alpha <&> <*> alphanum <@ (implode o op::)

  fun token tok ss =
    let
      val xs = List.take (ss, size tok)
    in
      if implode xs = tok
      then SOME(tok, List.drop (ss, size tok))
      else NONE
    end
    handle Subscript => NONE

  local
    open SMLUnit

    fun toStringList toString ss =
      concat (map toString ss)

    fun toStringParserState toStringResult toStringElem ss =
      case ss
        of NONE        => "None"
         | SOME(r,ss') => concat[toStringResult r, "[", toStringList toStringElem ss', "]"]

    fun assertParserState toStringResult toStringElem exp act =
      Assert.assertEqual op= (toStringParserState toStringResult toStringElem) exp act
  in
    fun testNumber () =
      let
        val exp = SOME(123456, explode " foo")
        val act = number (explode "123456 foo")
      in
        assertParserState LargeInt.toString Char.toString exp act
      end

    val test_suite = (fn ()=>
      Test.labelTests [
        ("number", testNumber)
      ])

    val () = SMLUnit.TextUITestRunner.runTest
             {output = TextIO.stdOut}
             (test_suite ())
  end

end (* local *)
end

