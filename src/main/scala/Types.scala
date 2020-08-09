trait Types {

  sealed abstract class ExprT

  case object EmptyT extends ExprT {
    override def toString = "'empty'"
  }

  case object BoolT extends ExprT {
    override def toString = "'boolean'"
  }

  case object NumT extends ExprT {
    override def toString = "'number'"
  }

  case object CharT extends ExprT {
    override def toString = "'character'"
  }

//  case class ListT(elements_type: ExprT) extends ExprT {
//    override def toString: String = "'list of " + elements_type + "'"
//  }

  case class FunT(from: ExprT, to: ExprT) extends ExprT

  case class Var(id: Int) extends ExprT {
    var instance: Option[ExprT] = None
    lazy val name: String = HM.nextUniqueName

    override def toString: String = "v" + id
  }

}
