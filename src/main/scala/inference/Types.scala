package inference

import parsing.AST.{Native, PrimitiveExpr}

object Types {

  sealed trait ExprT {
    val scalaType: String
  }

  case object EmptyT extends ExprT {
    override def toString = "'ø'"

    val scalaType: String = "null"
  }

  trait PrimitiveExprT extends ExprT {
    val expr: PrimitiveExpr

    override def equals(obj: Any): Boolean = obj.getClass == this.getClass
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
    lazy val scalaType: String = f"$from => $to"

    override def toString = f"{$from→$to}"
  }

  case class Var(id: Int) extends TypeSystem with ExprT {
    var instance: Option[ExprT] = None
    lazy val name: String = nextUniqueName
    lazy val scalaType: String = instance.get.scalaType

    override def toString: String = if (instance.isEmpty) ("t" + id) else instance.get.toString
  }

}
