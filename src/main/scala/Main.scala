import AST._
import Types.{BoolT, CharT, NumT, PrimitiveExprT, StrT}

import scala.collection.immutable.Queue
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
      val vals = e.nested.map { // Iterable
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
      val args = l.map { case id ~ t => id.t = t; id }
      var newbody = Sequence(List(Scala(args, code, r)))
      for (arg <- args.tail.reverse) {
        newbody = Sequence(List(Lambda(arg, newbody)))
      }
      Lambda(args.head, newbody)
  }
  private lazy val typ: P[PrimitiveExprT] = "b" ^^^ BoolT | "c" ^^^ CharT | "s" ^^^ StrT | "n" ^^^ NumT

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
  private lazy val num = floatingPointNumber ^^ (n => Num(n.toDouble))
  private lazy val str = stringLiteral ^^ (str => Str(str.tail.dropRight(1)))

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

    def ev(e: Expr, m: LMap[Expr], q: Queue[Expr]): (Expr, LMap[Expr]) = {
      val e2 -> m2 = e match {
        case Assign(x, y) => Empty() -> m.put(x.name, y)
        case Ident(name) => m.get(name) -> m
        case Sequence(x :: List()) => ev(x, m, q)
        case Sequence(x :: xs) => ev(Sequence(xs), ev(x, m, q)._2, q)
        case Appl(f, x) => ev(f, m, q :+ ev(x, m, q)._1)
        case Lambda(arg, body) =>
          val x -> q2 = q.dequeue
          ev(body, m.put(arg.name, x), q2)
        case p: PrimitiveExpr => p -> m
        case s@Scala(params, _, _) => s.func(params.map(x => m.get(x.name))) -> m
      }
      if (e2.isInstanceOf[PrimitiveExpr]) (e2, m2) else ev(e2, m2, q)
    }

    def eval(e: Expr): Any = {
      val re = ev(e, LMap(), Queue())
      //      println(">>>  " + re._1)
      re._1.asInstanceOf[PrimitiveExpr].value
    }

    val re = eval(r.get)
    println()
    println(re)
  }
}