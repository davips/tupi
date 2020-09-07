package inference

import parsing.AST
import parsing.AST._

object Types {

  sealed trait ExprT {
    val scalaType: String
  }

  case object EmptyT extends ExprT {
    override def toString = "'ø'"

    val scalaType: String = "Null"
  }

  trait PrimitiveExprT extends ExprT {
    val expr: PrimitiveExpr

    override def equals(obj: Any): Boolean = obj.getClass == this.getClass
  }

  object PrimitiveExprT {
    def unapply(exprt: PrimitiveExprT): Option[AST.PrimitiveExpr] = Some(exprt.expr)

    def apply(expr: PrimitiveExpr): PrimitiveExprT = expr match {
      case Bool(_) => BoolT(expr)
      case Num(_) => NumT(expr)
      case Char(_) => CharT(expr)
      case Str(_) => StrT(expr)
    }
  }

  case class BoolT(expr: PrimitiveExpr = Native) extends PrimitiveExprT {
    override val toString = "'boolean'"
    val scalaType: String = "Boolean"
  }

  case class NumT(expr: PrimitiveExpr = Native) extends PrimitiveExprT {
    override val toString = "'number'"
    val scalaType: String = "Double"
  }

  case class CharT(expr: PrimitiveExpr = Native) extends PrimitiveExprT {
    override val toString = "'char'"
    val scalaType: String = "Character"
  }

  case class StrT(expr: PrimitiveExpr = Native) extends PrimitiveExprT {
    override val toString = "'str'"
    val scalaType: String = "String"
  }

  //  case class ListT(elements_type: ExprT) extends ExprT {
  //    override def toString: String = "'list of " + elements_type + "'"
  //  }

  case class LambdaT(from: ExprT, to: ExprT) extends ExprT {
    lazy val expr: PrimitiveExpr = Func(from, to)
    lazy val scalaType: String = f"$from => $to"

    override def toString = f"{$from→$to}"
  }

  case class Var(id: Int) extends TypeSystem with ExprT {
    var instance: Option[ExprT] = None
    lazy val name: String = nextUniqueName
    lazy val scalaType: String = instance.get.scalaType

    override def toString: String = if (instance.isEmpty) name else instance.get.toString
  }

}
