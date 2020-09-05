import inference.HM
import parsing.AST._
import parsing.Grammar
import runtime.LMap

import scala.collection.immutable.Queue
import scala.io.Source

object Main extends App {
  val arq = Source.fromFile("test.tupi")
  val txt = arq.getLines().mkString("\n")
  val txt2 = txt.replace("\n", ";\n").replace("(;\n", "(\n").replace(";\n)", "\n)").replace(";\n;", ";").replace(";;", ";").lines().filter(!_.startsWith("~")).toArray().toList.mkString //.lines().map(_.trim).toArray().toList.mkString("\n").replace("\n}", "ŋ}").replace("{\n", "ŋ{").replace(":\n", "ŋ:").replace("\n", ";\n").replace("ŋ:", ":\n").replace("ŋ{", "{\n").replace("ŋ}", "\n}")
  println(txt)
  val txt3 = if (txt2.endsWith(";")) txt2.dropRight(1) else txt2
  val r = {
    val st = System.currentTimeMillis()
    val r = Grammar.parse(txt3)
    println((System.currentTimeMillis() - st) + "ms")
    r
  }
  println(txt3)
  println()
  println(r)
  println()
  if (r.successful) {
    if (!HM.check(r.get)) sys.exit()

    def ev(e: Expr, m: LMap[Expr], q: Queue[Expr]): (Expr, LMap[Expr]) = {
      val e2 -> m2 = e match {
        case Assign(x, y) =>
          if (m.get(x.name).isDefined) {
            println(x.name + " already assigned!")
            sys.exit()
          }
          Empty() -> m.put(x.name, y)
        case Ident(name) => m(name) -> m
        case Sequence(x :: List()) => ev(x, m, q)
        case Sequence((a@Assign(_, _)) :: xs) => ev(Sequence(xs), ev(a, m, q)._2, q)
        case Sequence(x :: xs) => ev(Sequence(xs), m, q) // TODO: tail call optimization
        case Appl(f, x) => ev(f, m, ev(x, m, q)._1 +: q)
        case Lambda(arg, body) if q.nonEmpty =>
          val x -> q2 = q.dequeue
          ev(body, m.put(arg.name, x), q2)
        case p: PrimitiveExpr => p -> m
        case s@Scala(params, _, _) => s.func(params.map(x => m(x.name))) -> m
        case Lambda(arg, body) => Empty() -> m
      }
      if (e2.isInstanceOf[PrimitiveExpr]) (e2, m2) else ev(e2, m2, q)
    }

    def eval(e: Expr): Any = {
      val re = ev(e, LMap(), Queue())
      //      println(">>>  " + re._1)
      re._1.asInstanceOf[PrimitiveExpr].value
    }

    val re = eval(r.get)
    println()
    println(re)
  }
}