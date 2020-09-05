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

  case class Empty() extends PrimitiveExpr {
    val value: Null = null

    def nested: Iterator[Expr] = Iterator.empty

    override def toString = "ø"
  }

  case class Bool(value: java.lang.Boolean) extends PrimitiveExpr {
    def nested: Iterator[Expr] = Iterator.empty

    override def toString: String = if (value) "↑" else "↓"
  }

  case class Char(value: java.lang.Character) extends PrimitiveExpr {
    def nested: Iterator[Expr] = Iterator.empty

    override def toString: String = value.toString
  }

  case class Num(value: java.lang.Number) extends PrimitiveExpr {
    def nested: Iterator[Expr] = Iterator.empty

    override def toString: String = value.toString
  }

  case class Str(value: String) extends PrimitiveExpr {
    def nested: Iterator[Expr] = Iterator.empty

    override def toString: String = value
  }

  case class Assign(a: NamedIdent, b: Expr) extends Expr {
    def nested: Iterator[Expr] = Iterator(a, b)

    override def toString: String = a + "←" + b
  }

  case class Appl(a: Expr, b: Expr) extends Expr {
    def nested: Iterator[Expr] = Iterator(a, b)

    override def toString: String = a + "(" + b + ")"
  }

  trait Ident extends Expr {
    val name: String

    def nested: Iterator[Expr] = Iterator.empty

    override def toString: String = name
  }

  object Ident {
    def unapply(e: Ident): Option[String] = Some(e.name)
  }

  case class NamedIdent(name: String) extends Ident

  case class AnonIdent(idx: Int) extends Ident {
    val name: String = "#" + idx
  }

  case class Sequence(items: List[Expr]) extends Expr {
    def nested: Iterator[Expr] = items.iterator

    override def toString: String = "<" + items.mkString("; ") + ">"
  }

  case class Lambda(param: Ident, body: Sequence) extends Expr {
    def nested: Iterator[Expr] = body.nested

    override def toString: String = "{" + param + ": " + body + "}"
  }

  case class Closure(body: Expr, context: LMap[Expr]) extends Expr {
    def nested: Iterator[Expr] = Iterator.empty

    override def toString: String = "(" + body + ": " + context.m.keys + ")"
  }

  case class Scala(params: List[NamedIdent], code: Str, typ: PrimitiveExprT) extends Expr {
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
      typ.cast(evaluated)
    }

    def nested: Iterator[Expr] = Iterator.empty

    override def toString: String = "[" + params.mkString(",") + ": " + code + "]"
  }

}