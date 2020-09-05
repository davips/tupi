package parsing

import inference.Types.{BoolT, CharT, NumT, StrT}
import parsing.AST.{AnonIdent, Appl, Expr, NamedIdent, Num, Str, _}

import scala.util.parsing.combinator.{ImplicitConversions, JavaTokenParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

object Grammar extends RegexParsers with ImplicitConversions with JavaTokenParsers with Native with Lamb {
  type P[+T] = PackratParser[T]
  private lazy val program = sequence(false) | sequence(true)
  private lazy val sequence = (iargs: Boolean) => rep1sep(if (iargs) iexpr else prettyexpr, separator) ^^ Sequence
  private lazy val separator = ";" // not(":" ~ "\n") ~> "\n"
  private lazy val prettyexpr: P[Expr] = math(false) | expr
  private lazy val expr: P[Expr] = "(" ~> rep1sep(prettyexpr, separator) <~ ")" ^^ Sequence | "(" ~> prettyexpr <~ ")" | scala | appl | assign | (lambda | ilambda) | term(false) | "{" ~> infixops <~ "}"
  private lazy val iexpr: P[Expr] = scala | iassign | lambda | math(true) | term(true) | "{" ~> infixops <~ "}"
  private lazy val assign: P[Expr] = (newidentifier <~ "←") ~ prettyexpr ^^ Assign
  private lazy val iassign: P[Expr] = (newidentifier <~ "←") ~ (assign | lambda | math(true) | term(true)) ^^ Assign
  private lazy val lambda = ("{" ~> rep1(identifier) <~ ":") ~ (sequence(false) <~ "}") ^^ expandLambda
  private lazy val ilambda = ("{" ~> sequence(true) <~ "}") ^^ iexpandLambda
  private lazy val scala = ("[" ~> rep((identifier <~ ":") ~ typ) ~ (str <~ ":") ~ (typ <~ "]")) ^^ expandScala
  private lazy val typ = "b" ^^^ BoolT | "c" ^^^ CharT | "s" ^^^ StrT | "n" ^^^ NumT
  private lazy val identifier = ("_" | ident) ^^ NamedIdent
  private lazy val newidentifier = identifier | infixops
  private lazy val infixops = ("=" | "!=" | ">=" | "<=" | ">" | "<" | "+" | "-" | "*" | "/" | "^") ^^ NamedIdent
  private lazy val anonidentifier = "#" ~> """\d+""".r ^^ (idx => AnonIdent(idx.toInt))
  private lazy val math = (iargs: Boolean) => equality(iargs)
  private lazy val equality = (iargs: Boolean) => chainl1(
    sum(iargs), curry("=") | curry("!=") | curry(">=") | curry("<=") | curry(">") | curry("<")
  )
  private lazy val sum = (iargs: Boolean) => chainl1(product(iargs), curry("+") | curry("-"))
  private lazy val product = (iargs: Boolean) => chainl1(power(iargs), curry("*") | curry("/"))
  private lazy val power = (iargs: Boolean) => chainl1(if (iargs) iexpr else expr, curry("^"))
  private lazy val curry = (op: String) => op ^^^ ((a: Expr, b: Expr) => Appl(Appl(NamedIdent(op), a), b))

  private lazy val term = (iargs: Boolean) => (if (iargs) identifier | anonidentifier else identifier) | literal
  private lazy val literal = num | str
  private lazy val num = floatingPointNumber ^^ (n => Num(n.toDouble))
  private lazy val str = stringLiteral ^^ (str => Str(str.tail.dropRight(1)))

  private lazy val appl: P[Expr] = (appl ~ expr | func ~ expr) ^^ Appl
  private lazy val func: P[Expr] = lambda | ilambda | identifier | "{" ~> infixops <~ "}"

  def parse(txt: String): ParseResult[Sequence] = parseAll(phrase(program), new PackratReader(new CharSequenceReader(txt)))
}
