import scala.io.Source
import scala.util.parsing.combinator.{ImplicitConversions, JavaTokenParsers, PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

object Main extends HM2 with RegexParsers with ImplicitConversions with JavaTokenParsers with PackratParsers {
  type P[+T] = PackratParser[T]

  private def f(args: List[NamedIdent], body: Sequence) = {
    var newbody = body
    for (arg <- args.tail.reverse) {
      newbody = Sequence(List(Lambda(arg, newbody)))
    }
    Lambda(args.head, newbody)
  }

  private def g(body: Sequence) = {
    def traverse(e: Expr, mx: Int = 0): Int = {
      val vals = e.map {
        case AnonIdent(idx) if idx > mx => idx
        case e => traverse(e, mx)
      }
      if (vals.nonEmpty) vals.max else mx
    }

    val args = (1 to traverse(body)).map(AnonIdent)
    var newbody = body
    for (arg <- args.tail.reverse) {
      newbody = Sequence(List(Lambda(arg, newbody)))
    }
    Lambda(args.head, newbody)
  }

  private lazy val program = sequence() | sequence(true)

  private def sequence(impargs: Boolean = false) = rep1sep(if (impargs) iexpr else prettyexpr, separator) ^^ Sequence

  private lazy val separator = ";" // not(":" ~ "\n") ~> "\n"
  private lazy val prettyexpr: P[Expr] = math() | expr
  private lazy val expr: P[Expr] = "(" ~> expr <~ ")" | scala | appl | assign | (lambda | ilambda) | term()
  private lazy val iexpr: P[Expr] = scala | iassign | lambda | math(true) | term(true)
  private lazy val assign: P[Expr] = (newidentifier <~ "←") ~ prettyexpr ^^ Assign
  private lazy val iassign: P[Expr] = (newidentifier <~ "←") ~ (assign | lambda | math(true) | term(true)) ^^ Assign
  private lazy val lambda = ("{" ~> rep1(identifier) <~ ":") ~ (sequence() <~ "}") ^^ f
  private lazy val ilambda = ("{" ~> sequence(true) <~ "}") ^^ g
  private lazy val scala = "[" ~> rep((identifier <~ ":") ~ typ) ~ (str <~ ":") ~ (typ <~ "]") ^^ {
    case l ~ code ~ r =>
      val l2 = l.map { case id ~ t => id.t = t; id }
      Scala(l2, code, r)
  }
  private lazy val typ = "b" ^^^ BoolT | "c" ^^^ CharT | "s" ^^^ StrT | "n" ^^^ NumT

  private def identifier: P[NamedIdent] = ("_" | ident) ^^ NamedIdent

  private def newidentifier: P[NamedIdent] = identifier |
    ("=" | "!=" | ">=" | "<=" | ">" | "<" | "+" | "-" | "*" | "/" | "^") ^^ NamedIdent

  private def anonidentifier = "#" ~> """\d+""".r ^^ (idx => AnonIdent(idx.toInt))

  private def math(impargs: Boolean = false) = equality(impargs)

  private def equality(impargs: Boolean = false) = chainl1(
    sum(impargs), curry("=") | curry("!=") | curry(">=") | curry("<=") | curry(">") | curry("<")
  )

  private def sum(impargs: Boolean = false) = chainl1(product(impargs), curry("+") | curry("-"))

  private def product(impargs: Boolean = false) = chainl1(power(impargs), curry("*") | curry("/"))

  private def power(impargs: Boolean = false) = chainl1(if (impargs) iexpr else prettyexpr, curry("^"))

  private def curry(op: String) = op ^^^ ((a: Expr, b: Expr) => Appl(Appl(NamedIdent(op), a), b))

  private def term(impargs: Boolean = false) = (if (impargs) anonidentifier else identifier) | literal

  private lazy val literal = num | str
  private lazy val num = floatingPointNumber ^^ (n => Num(n.toFloat))
  private lazy val str = stringLiteral ^^ Str

  private lazy val appl: P[Expr] = (appl ~ expr | func ~ expr) ^^ Appl
  private lazy val func: P[Expr] = lambda | ilambda | identifier


  def main(args: Array[String]) {
    val arq = Source.fromFile("test.tupi")
    val txt = arq.getLines().mkString("\n")
    val txt2 = txt.replace("\n", ";\n") //.lines().map(_.trim).toArray().toList.mkString("\n").replace("\n}", "ŋ}").replace("{\n", "ŋ{").replace(":\n", "ŋ:").replace("\n", ";\n").replace("ŋ:", ":\n").replace("ŋ{", "{\n").replace("ŋ}", "\n}")
    val r = {
      val st = System.currentTimeMillis()
      val r = parseAll(phrase(program), new PackratReader(new CharSequenceReader(txt2)))
      println((System.currentTimeMillis() - st) + "ms")
      r
    }
    println(txt2)
    println()
    println(r)
    println()
    tryexp(r.get)

    val m = LMap[Expr]()

    //    def eval(e: Expr) = e match {
    //      case Assign(x, y) => m.put(x.name, y)
    //      case Sequence(l) => l.map(eval)
    //      case Lambda(param, body) => l.map(eval)
    //      case Add(x, y) => x + y
    //      case Sub(x, y) => x - y
    //      case Add(x, y) => x * y
    //      case Add(x, y) => x / y
    //    }
  }
}