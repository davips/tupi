trait AST extends Types {

  sealed trait Expr extends Iterable[Expr] {
    var t: ExprT = _
  }

  case class Empty() extends Expr {
    override def iterator: Iterator[Expr] = Iterator.empty

    override def toString = "ø"
  }

  case class Bool(value: java.lang.Boolean) extends Expr {
    override def iterator: Iterator[Expr] = Iterator.empty

    override def toString: String = if (value) "↑" else "↓"
  }

  case class Char(value: java.lang.Character) extends Expr {
    override def iterator: Iterator[Expr] = Iterator.empty

    override def toString: String = value.toString
  }

  case class Num(value: java.lang.Number) extends Expr {
    override def iterator: Iterator[Expr] = Iterator.empty

    override def toString: String = value.toString
  }

  sealed trait BinOp extends Expr {
    val a, b: Expr
    val op: String

    override def iterator: Iterator[Expr] = Iterator(a, b)

    override def toString: String = a + op + b
  }

  object BinOp {
    def unapply(e: BinOp): Option[(Expr, Expr)] = Some(e.a, e.b)
  }

  case class Add(a: Expr, b: Expr) extends BinOp {
    val op = "+"
  }

  case class Sub(a: Expr, b: Expr) extends BinOp {
    val op = "-"
  }

  case class Mul(a: Expr, b: Expr) extends BinOp {
    val op = "*"
  }

  case class Div(a: Expr, b: Expr) extends BinOp {
    val op = "/"
  }

  case class Pow(a: Expr, b: Expr) extends BinOp {
    val op = "^"
  }

  case class Rem(a: Expr, b: Expr) extends BinOp {
    val op = "%"
  }

  case class Equal(a: Expr, b: Expr) extends Expr {
    override def iterator: Iterator[Expr] = Iterator(a, b)

    override def toString: String = a + "=" + b
  }

  case class Assign(a: Ident, b: Expr) extends Expr {
    override def iterator: Iterator[Expr] = Iterator(a, b)

    override def toString: String = a + "←" + b
  }

  case class Appl(a: Expr, b: Expr) extends Expr {
    override def iterator: Iterator[Expr] = Iterator(a, b)

    override def toString: String = a + "(" + b + ")"
  }

  trait AbsIdent extends Expr {
    val name: String

    override def iterator: Iterator[Expr] = Iterator.empty

    override def toString: String = name
  }

  case class Ident(name: String) extends AbsIdent

  case class AnonIdent(idx: Int) extends AbsIdent {
    val name: String = "#" + idx
  }

  case class Sequence(items: List[Expr]) extends Expr {
    override def iterator: Iterator[Expr] = items.iterator

    override def toString: String = items.mkString("; ")
  }

  case class Lambda(param: AbsIdent, body: Sequence) extends Expr {
    override def iterator: Iterator[Expr] = body.iterator

    override def toString: String = "(" + param + " → " + body + ")"
  }

}
