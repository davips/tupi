package parsing

import parsing.AST._

trait Lamb {
  def expandLambda(args: List[NamedIdent], body: Sequence): Lambda = {
    var newbody = body
    for (arg <- args.tail.reverse) {
      newbody = Sequence(List(Lambda(arg, newbody)))
    }
    Lambda(args.head, newbody)
  }

  def iexpandLambda(body: Sequence): Lambda = {
    def minmax(expr: Expr): (Int, Int) = {
      val idxs = traverse(expr)
      (idxs.min, idxs.max)
    }

    def traverse(expr: Expr): List[Int] = expr.nested.toList.flatMap {
        case AnonIdent() => List(-1)
        case AnonIdentN(idx) => List(idx)
        case innerExpr => traverse(innerExpr)
      }

    val (mn, mx) = minmax(body)

    val (firstArg, newbody) = if (mn == -1) {
      if (mx != -1) {
        println("Mixed anonymous identifiers! _ cannot be user together with _1, _2, etc.")
      }
      AnonIdent() -> body
    } else {
      var newbody = body
      for (arg <- (mx to 2 by -1).map(AnonIdentN)) {
        newbody = Sequence(List(Lambda(arg, newbody)))
      }
      AnonIdentN(1) -> newbody
    }

    Lambda(firstArg, newbody)
  }
}