import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.{ImplicitConversions, JavaTokenParsers, PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

object Main extends HM2 with RegexParsers with ImplicitConversions with JavaTokenParsers with PackratParsers  {
  type P[+T] = PackratParser[T]
  //  override protected val whiteSpace: Regex = """[ \t]""".r
  //  override def skipWhitespace = true

  private lazy val program = sequence | isequence
  private lazy val literal: P[Num] = wholeNumber ^^ (n => Num(n.toInt))

  // Explicit args.
  private lazy val separator = ";" // not(":" ~ "\n") ~> "\n"
  private lazy val sequence = rep1sep(expr, separator) ^^ Sequence
  private lazy val expr: P[Expr] = appl |assign | (lambda | ilambda) | math | term
  private lazy val appl: P[Expr] = (lambda | ilambda | appl | identifier) ~ expr ^^ Appl
  private lazy val assign: P[Expr] = (identifier <~ "←") ~ expr ^^ Assign
  private lazy val lambda = ("{" ~> rep1(identifier) <~ ":") ~ (sequence <~ "}") ^^ {
    case args ~ body =>
      var newbody = body
      for (arg <- args.tail.reverse) {
        newbody = Sequence(List(Lambda(arg, newbody)))
      }
      Lambda(args.head, newbody)
  }
  private lazy val math = equality // (expr <~ "*") ~ expr ^^ Mul
  private lazy val equality = chainl1(sum, "=" ^^^ Equal) // | "!=" ^^^ DiffE | ">=" ^^^ GreaterEqual | "<=" ^^^ LesserEqual | ">" ^^^ Greater | "<" ^^^ Lesser)
  private lazy val sum = chainl1(product, "+" ^^^ Add | "-" ^^^ Sub)
  //  private lazy val  list_concatenation = product * ("++" ^^^ ConcatenateListExpr)
  private lazy val product = chainl1(power, "*" ^^^ Mul)
  private lazy val power = chainl1(expr, "^" ^^^ Pow)
  private lazy val term = identifier | literal
  private lazy val identifier: P[Ident] = ("_" | ident) ^^ Ident


  // Implicit args.
  private lazy val isequence = rep1sep(iexpr, separator) ^^ Sequence
  private lazy val iexpr: P[Expr] = iassign | lambda | imath | iterm
  private lazy val iassign: P[Expr] = (identifier <~ "←") ~ (assign | lambda | imath | iterm) ^^ { case (ide ~ e) => Assign(ide, e) }
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
  private lazy val imath = iequality // (expr <~ "*") ~ expr ^^ Mul
  private lazy val iequality = chainl1(isum, "=" ^^^ Equal) // | "!=" ^^^ DiffE | ">=" ^^^ GreaterEqual | "<=" ^^^ LesserEqual | ">" ^^^ Greater | "<" ^^^ Lesser)
  private lazy val isum = chainl1(iproduct, "+" ^^^ Add | "-" ^^^ Sub)
  //  private lazy val  list_concatenation = product * ("++" ^^^ ConcatenateListExpr)
  private lazy val iproduct = chainl1(ipower, "*" ^^^ Mul)
  private lazy val ipower = chainl1(iexpr, "^" ^^^ Pow)
  private lazy val iterm = anonidentifier | literal
  private lazy val anonidentifier = "#" ~> """\d+""".r ^^ (idx => AnonIdent(idx.toInt))

  def main(args: Array[String]) {
    val arq = Source.fromFile("test.tupi")
    val txt = arq.getLines().mkString("\n")
    val txt2 = txt.replace("\n", ";\n") //.lines().map(_.trim).toArray().toList.mkString("\n").replace("\n}", "ŋ}").replace("{\n", "ŋ{").replace(":\n", "ŋ:").replace("\n", ";\n").replace("ŋ:", ":\n").replace("ŋ{", "{\n").replace("ŋ}", "\n}")
    val r = parseAll(phrase(program), new PackratReader(new CharSequenceReader(txt2)))
    println(txt2)
    println()
    println(r)
    println()
    println(tryexp(r.get))

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