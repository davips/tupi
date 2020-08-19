import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.{ImplicitConversions, JavaTokenParsers, PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

object Main extends HM2 with RegexParsers with ImplicitConversions with JavaTokenParsers with PackratParsers {
  type P[+T] = PackratParser[T]

  //Common.
  private lazy val program = sequence | isequence
  private lazy val num = floatingPointNumber ^^ (n => Num(n.toFloat))
  private lazy val str = stringLiteral ^^ Str
  private lazy val literal = num | str
  private lazy val scala = "[" ~> rep((identifier <~ ":") ~ t) ~ (str <~ ":") ~ (t <~ "]") ^^ {
    case l ~ code ~ r =>
      val l2 = l.map { case id ~ t => id.t = t; id }
      Scala(l2, code, r)
  }
  private lazy val t = "b" ^^^ BoolT | "c" ^^^ CharT | "s" ^^^ StrT | "n" ^^^ NumT
  private lazy val appl: P[Expr] = (lambda | ilambda | appl | identifier) ~ expr ^^ Appl

  // Explicit args.
  private lazy val separator = ";" // not(":" ~ "\n") ~> "\n"
  private lazy val sequence = rep1sep(expr, separator) ^^ Sequence
  private lazy val expr: P[Expr] = "("~> expr <~ ")" | scala | appl | assign | (lambda | ilambda) | math | term
  private lazy val assign: P[Expr] = (identifier2 <~ "←") ~ expr ^^ Assign
  private lazy val lambda = ("{" ~> rep1(identifier) <~ ":") ~ (sequence <~ "}") ^^ {
    case args ~ body =>
      var newbody = body
      for (arg <- args.tail.reverse) {
        newbody = Sequence(List(Lambda(arg, newbody)))
      }
      Lambda(args.head, newbody)
  }
  private lazy val math = equality
  private lazy val equality = chainl1(sum, curry("=") | curry("!=") | curry(">=") | curry("<=") | curry(">") | curry("<"))
  private lazy val sum = chainl1(product, curry("+") | curry("-"))
  private lazy val product = chainl1(power, curry("*") | curry("/"))
  private lazy val power = chainl1(expr, curry("^"))
  private lazy val term = identifier | literal
  private lazy val identifier: P[NamedIdent] = ("_" | ident) ^^ NamedIdent
  private lazy val identifier2: P[NamedIdent] = identifier | ("=" | "!=" | ">=" | "<=" | ">" | "<" | "+" | "-" | "*" | "/" | "^") ^^ NamedIdent


  // Implicit args.
  private lazy val isequence = rep1sep(iexpr, separator) ^^ Sequence
  private lazy val iexpr: P[Expr] = scala | iassign | lambda | imath | iterm
  private lazy val iassign: P[Expr] = (identifier2 <~ "←") ~ (assign | lambda | imath | iterm) ^^ { case (ide ~ e) => Assign(ide, e) }
  private lazy val ilambda = ("{" ~> isequence <~ "}") ^^ {
    body =>
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
  private lazy val imath = iequality
  private lazy val iequality = chainl1(isum, curry("=") | curry("!=") | curry(">=") | curry("<=") | curry(">") | curry("<"))
  private lazy val isum = chainl1(iproduct, curry("+") | curry("-"))
  private lazy val iproduct = chainl1(ipower, curry("*") | curry("/"))
  private lazy val ipower = chainl1(iexpr, curry("^"))
  private lazy val iterm = anonidentifier | literal
  private lazy val anonidentifier = "#" ~> """\d+""".r ^^ (idx => AnonIdent(idx.toInt))

  private def curry(op: String) = op ^^^ ((a: Expr, b: Expr) => Appl(Appl(NamedIdent(op), a), b))

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