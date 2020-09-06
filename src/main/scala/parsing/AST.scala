package parsing

import runtime.LMap

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

object AST {

  import inference.Types.{ExprT, PrimitiveExprT}

  sealed trait Expr {
    var t: Option[ExprT] = None

    def nested: Iterator[Expr]
  }


  trait PrimitiveExpr extends Expr {
    val value: Any
  }

  case object Native extends PrimitiveExpr {
    lazy val value: Any = ???
    override val toString = "'native'"

    def nested: Iterator[Expr] = Iterator.empty
  }

  case class Empty() extends PrimitiveExpr {
    val value: Null = null
    override val toString = "ø"

    def nested: Iterator[Expr] = Iterator.empty
  }

  case class Bool(value: java.lang.Boolean) extends PrimitiveExpr {
    override val toString: String = if (value) "↑" else "↓"

    def nested: Iterator[Expr] = Iterator.empty
  }

  case class Char(value: java.lang.Character) extends PrimitiveExpr {
    override val toString: String = value.toString

    def nested: Iterator[Expr] = Iterator.empty
  }

  case class Num(value: java.lang.Number) extends PrimitiveExpr {
    override val toString: String = value.toString

    def nested: Iterator[Expr] = Iterator.empty
  }

  case class Str(value: String) extends PrimitiveExpr {
    override lazy val toString: String = value

    def nested: Iterator[Expr] = Iterator.empty
  }

  case class Assign(a: NamedIdent, b: Expr) extends Expr {
    override val toString: String = a + "←" + b

    def nested: Iterator[Expr] = Iterator(a, b)
  }

  case class Appl(a: Expr, b: Expr) extends Expr {
    override val toString: String = a + "(" + b + ")"

    def nested: Iterator[Expr] = Iterator(a, b)
  }

  trait Ident extends Expr {
    val name: String
    override val toString: String = name

    def nested: Iterator[Expr] = Iterator.empty
  }

  object Ident {
    def unapply(e: Ident): Option[String] = Some(e.name)
  }

  case class NamedIdent(name: String) extends Ident

  case class AnonIdent() extends Ident {
    val name: String = "_"
  }

  case class AnonIdentN(idx: Int) extends Ident {
    val name: String = "_" + idx
  }

  case class Sequence(items: List[Expr]) extends Expr {
    override val toString: String = "<" + items.mkString("; ") + ">"

    def nested: Iterator[Expr] = items.iterator
  }

  case class Lambda(param: Ident, body: Sequence) extends Expr {
    override val toString: String = "{" + param + ": " + body + "}"

    def nested: Iterator[Expr] = body.nested
  }

  case class Closure(body: Expr, context: LMap[Expr]) extends Expr {
    override val toString: String = "(" + body + ": " + context.m.keys + ")"

    def nested: Iterator[Expr] = Iterator.empty
  }

  case class Scala(params: List[NamedIdent], code: Str, typ: PrimitiveExprT) extends Expr {
    override val toString: String = "[" + params.mkString(",") + ": " + code + "]"
    //    private val types = typ +: params.map(_.t).reverse
    //    t = Some(typ) //types.reduce((to, from) => LambdaT(from, to))

    def func(args: List[Any]): PrimitiveExpr = {
      val toolbox = currentMirror.mkToolBox()
      val vars = params.zipWithIndex.map {
        case (i@Ident(name), idx) => f"  val $name = args($idx).asInstanceOf[${i.t.get.scalaType}]\n"
      }
      //      println("111111111111111111111", code.value)
      val txt =
        f"""def f(args: List[Any]) = {
           |${vars.mkString}""".stripMargin + "  " + code.value +
          f"""
             |}
             |f(List(${args.mkString(", ")}))""".stripMargin
      //      println(txt)
      val evaluated = toolbox.eval(toolbox.parse(txt))
      evaluated.asInstanceOf[PrimitiveExprT].expr
    }

    def nested: Iterator[Expr] = Iterator.empty
  }

}