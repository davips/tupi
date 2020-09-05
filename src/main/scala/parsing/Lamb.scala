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
    def traverse(e: Expr, mx: Int = 0): Int = {
      val vals = e.nested.map { // Iterable
        case AnonIdent(idx) if idx > mx => idx
        case e => traverse(e, mx)
      }
      if (vals.nonEmpty) vals.max else mx
    }

    val args = (1 to traverse(body)).map(AnonIdent)
    var newbody = body
    for (arg <- args.tail.reverse) {
      newbody = Sequence(List(Lambda(arg, newbody)))
    }
    Lambda(args.head, newbody)
  }
}