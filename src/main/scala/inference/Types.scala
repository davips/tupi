package inference

import parsing.AST.{Bool, Char, Num, PrimitiveExpr, Str}
object Types {

  sealed trait ExprT {
    val scalaType: String
  }

  case object EmptyT extends ExprT {
    override def toString = "'empty'"

    val scalaType: String = "null"
  }

  trait PrimitiveExprT extends ExprT {
    def cast(value: Any): PrimitiveExpr
  }

  case object BoolT extends PrimitiveExprT {
    override def toString = "'boolean'"

    def cast(value: Any): Bool = Bool(value.asInstanceOf[Boolean])

    val scalaType: String = "Boolean"
  }

  case object NumT extends PrimitiveExprT {
    override def toString = "'number'"

    def cast(value: Any): Num = Num(value.asInstanceOf[Double])

    val scalaType: String = "Double"
  }

  case object CharT extends PrimitiveExprT {
    override def toString = "'character'"

    def cast(value: Any): Char = Char(value.asInstanceOf[Character])

    val scalaType: String = "Character"
  }

  case object StrT extends PrimitiveExprT {
    override def toString = "'str'"

    def cast(value: Any): Str = Str(value.asInstanceOf[String])

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
