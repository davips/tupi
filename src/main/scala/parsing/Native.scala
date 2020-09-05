package parsing

import inference.Types.{ExprT, LambdaT, PrimitiveExprT}
import parsing.AST.{Lambda, NamedIdent, PrimitiveExpr, Scala, Sequence, Str}

import scala.util.parsing.combinator.PackratParsers

trait Native extends PackratParsers {

  def expandScala(l: List[(NamedIdent ~ PrimitiveExprT)], code: Str, r: PrimitiveExprT): Lambda = {
    val args = l.map { case id ~ t => id.t = Some(t); id }
    var newbody = Sequence(List(Scala(args, code, r)))
    var res: ExprT = r
    for (arg <- args.tail.reverse) {
      val la = Lambda(arg, newbody)
      la.t = Some(LambdaT(arg.t.get, res))
      newbody = Sequence(List(la))
      res = la.t.get
    }
    val la = Lambda(args.head, newbody)
    la.t = Some(LambdaT(args.head.t.get, res))
    la
  }
}
