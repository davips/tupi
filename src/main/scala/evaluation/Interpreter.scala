package evaluation

import inference.Types.{EmptyT, LambdaT}
import parsing.AST._
import runtime.LMap

import scala.collection.immutable.Queue

object Interpreter {
  def ev(e: Expr, m: LMap[Expr]): (Expr, LMap[Expr]) = {
    val e2 -> m2 = e match {
      case Assign(x, y) =>
        if (m.get(x.name).isDefined) {
          println(x.name + " already assigned!")
          sys.exit()
        }
        Empty() -> m.put(x.name, y)
      case Ident(name) => m(name) -> m
      case Sequence(x :: List()) => ev(x, m)
      case Sequence((a@Assign(_, _)) :: xs) => ev(Sequence(xs), ev(a, m)._2)
      case Sequence(x :: xs) => ev(Sequence(xs), m) // TODO: tail call optimization
      case Appl(f, x) => ev(f, m)._1 -> ev(x, m)._1 match {
        case Func(Lambda(param, body), ctx) -> xev => ev(body, ctx.put(param.name, xev))
      }
      case p: PrimitiveExpr => p -> m
      case s@Scala(params, _, _) => s.func(params.map(x => m(x.name))) -> m
      case la: Lambda => Func(la, m) -> m // We need a closure here, since Lambda can be returned as a value to be applied later
    }
    if (e2.isInstanceOf[PrimitiveExpr]) (e2, m2) else ev(e2, m2)
  }

  def eval(e: Expr): Any = {
    val re = ev(e, LMap())
    re._1.asInstanceOf[PrimitiveExpr].value
  }
}
