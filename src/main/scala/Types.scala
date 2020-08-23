trait Types {

  sealed abstract class ExprT {
    val scalaType: String
  }

  case object EmptyT extends ExprT {
    override def toString = "'empty'"

    val scalaType: String = "null"
  }

  case object BoolT extends ExprT {
    override def toString = "'boolean'"

    val scalaType: String = "Boolean"
  }

  case object NumT extends ExprT {
    override def toString = "'number'"

    val scalaType: String = "Double"
  }

  case object CharT extends ExprT {
    override def toString = "'character'"

    val scalaType: String = "Character"
  }

  case object StrT extends ExprT {
    override def toString = "'str'"

    val scalaType: String = "String"
  }

  //  case class ListT(elements_type: ExprT) extends ExprT {
  //    override def toString: String = "'list of " + elements_type + "'"
  //  }

  case class LambdaT(from: ExprT, to: ExprT) extends ExprT {
    lazy val scalaType: String = f"$from => $to"

    override def toString = f"{$from→$to}"
  }

  case class Var(id: Int) extends ExprT {
    var instance: Option[ExprT] = None
    lazy val name: String = HM.nextUniqueName
    lazy val scalaType: String = instance.get.scalaType

    override def toString: String = if (instance.isEmpty) ("t" + id) else instance.get.toString
  }

}
