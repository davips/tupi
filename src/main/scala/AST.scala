import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.currentMirror

trait AST extends Types {

  sealed trait Expr extends Iterable[Expr] {
    var t: ExprT = _
  }

  case class Empty() extends Expr {
    override def iterator: Iterator[Expr] = Iterator.empty

    override def toString = "ø"
  }

  trait PrimitiveExpr extends Expr {
    val value: Any
  }

  case class Bool(value: java.lang.Boolean) extends PrimitiveExpr {
    override def iterator: Iterator[Expr] = Iterator.empty

    override def toString: String = if (value) "↑" else "↓"
  }

  case class Char(value: java.lang.Character) extends PrimitiveExpr {
    override def iterator: Iterator[Expr] = Iterator.empty

    override def toString: String = value.toString
  }

  case class Num(value: java.lang.Number) extends PrimitiveExpr {
    override def iterator: Iterator[Expr] = Iterator.empty

    override def toString: String = value.toString
  }

  case class Str(value: String) extends PrimitiveExpr {
    override def iterator: Iterator[Expr] = Iterator.empty

    override def toString: String = value
  }

  case class Assign(a: NamedIdent, b: Expr) extends Expr {
    override def iterator: Iterator[Expr] = Iterator(a, b)

    override def toString: String = a + "←" + b
  }

  case class Appl(a: Expr, b: Expr) extends Expr {
    override def iterator: Iterator[Expr] = Iterator(a, b)

    override def toString: String = a + "(" + b + ")"
  }

  trait Ident extends Expr {
    val name: String

    override def iterator: Iterator[Expr] = Iterator.empty

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
    override def iterator: Iterator[Expr] = items.iterator

    override def toString: String = items.mkString("; ")
  }

  case class Lambda(param: Ident, body: Sequence) extends Expr {
    override def iterator: Iterator[Expr] = body.iterator

    override def toString: String = "(" + param + ": " + body + ")"
  }

  case class Scala(params: List[NamedIdent], code: Str, typ: ExprT) extends Expr {
    private val types = typ +: params.map(_.t).reverse
    t = types.reduce((to, from) => LambdaT(from, to))

    lazy val func: List[Any] => Any = (args: List[Any]) => {
      val toolbox = currentMirror.mkToolBox()
      val vars = params.zipWithIndex.map {
        case (i@Ident(name), idx) => f"  val $name = args($idx).asInstanceOf[${i.t.scalaType}]\n"
      }
      val txt =
        f"""def f(args: List[Any]) = {
           |$vars
           |  (${args.mkString(", ")}) => $code
           |}""".stripMargin
      println(txt)
      val evaluated = toolbox.eval(toolbox.parse(txt))
      typ match {
        case BoolT => evaluated.asInstanceOf[List[Any] => Boolean]
        case CharT => evaluated.asInstanceOf[List[Any] => Character]
        case StrT => evaluated.asInstanceOf[List[Any] => String]
        case NumT => evaluated.asInstanceOf[List[Any] => Double]
      }
    }

    override def iterator: Iterator[Expr] = Iterator.empty

    override def toString: String = "[" + params.mkString(",") + ": " + code + "]"
  }

}