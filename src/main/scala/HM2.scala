/*
 * http://dysphoria.net/code/hindley-milner/HindleyMilner.scala
 * Andrew Forrest
 *
 * Implementation of basic polymorphic type-checking for a simple language.
 * Based heavily on Nikita Borisov’s Perl implementation at
 * http://web.archive.org/web/20050420002559/www.cs.berkeley.edu/~nikitab/courses/cs263/hm.html
 * which in turn is based on the paper by Luca Cardelli at
 * http://lucacardelli.name/Papers/BasicTypechecking.pdf
 *
 * If you run it with "scala HindleyMilner.scala" it will attempt to report the types
 * for a few example expressions. (It uses UTF-8 for output, so you may need to set your
 * terminal accordingly.)
 *
 * Do with it what you will.
 */

//abstract class Expr
//
//case class Lambda(v: String, body: Expr) extends Expr
//
//case class Ident(name: String) extends Expr
//
//case class Appl(a: Expr, b: Expr) extends Expr
//
//case class Assign(a: Expr, b: Expr) extends Expr
//
//case class Let(v: String, defn: Expr, body: Expr) extends Expr
//
//case class Letrec(v: String, defn: Expr, body: Expr) extends Expr

//object Expr {
//  def string(ast: Expr): String = {
//    if (ast.isInstanceOf[Ident])
//      nakedString(ast)
//    else
//      "(" + nakedString(ast) + ")"
//  }
//
//  def nakedString(ast: Expr) = ast match {
//    case i: Ident => i.name
//    case l: Lambda => "fn " + l.v + " ⇒ " + string(l.body)
//    case f: Appl => string(f.a) + " " + string(f.b)
//    case l: Let => "let " + l.v + " = " + string(l.defn) + " in " + string(l.body)
//    case l: Letrec => "letrec " + l.v + " = " + string(l.defn) + " in " + string(l.body)
//  }
//}

class ParseError(msg: String) extends Exception(msg)


class TypeSystem extends AST {

  type Env = Map[String, ExprT]

  //  abstract class Type
  //
  //  case class Var(id: Int) extends Type {
  //    var instance: Option[Type] = None
  //    lazy val name = nextUniqueName
  //  }
  //
  //  case class Oper(name: String, args: Seq[Type]) extends Type

  var _nextVarName = 'α';

  def nextUniqueName = {
    val result = _nextVarName
    _nextVarName = (_nextVarName.toInt + 1).toChar
    result.toString
  }

  var _nextVarId = 0

  def newVar: Var = {
    val result = _nextVarId
    _nextVarId += 1
    Var(result)
  }

  def analyse(ast: Expr, env: Env, print: Boolean = false): ExprT = analyse(ast, env, Set.empty, print)._1

  def analyse(ast: Expr, env: Env, nongen: Set[Var], debug: Boolean): (ExprT, Env) = {
    var newenv = env
    val t = ast match {
      //      case BinOp(a, b) =>
      //        val (a_typed, _) = analyse(a, env, nongen, debug)
      //        val (b_typed, _) = analyse(b, env, nongen, debug)
      //        unify(NumT, a_typed)
      //        unify(NumT, b_typed)
      //        NumT

      case s: Scala => s.t
      case Sequence(items) =>
        val types = for (it <- items) yield {
          val (typ, env2) = analyse(it, newenv, nongen, debug)
          newenv = env2
          typ
        }
        types.last
      case Ident(name) => gettype(name, env, nongen)
      case Appl(fn, arg) =>
        val (funtype, _) = analyse(fn, env, nongen, debug)
        val (argtype, _) = analyse(arg, env, nongen, debug)
        val resulttype = newVar
        unify(LambdaT(argtype, resulttype), funtype)
        resulttype
      case Lambda(arg, body) =>
        val argtype = newVar
        val (resulttype, _) = analyse(body, env + (arg.name -> argtype), nongen + argtype, debug)
        LambdaT(argtype, resulttype)
      case Assign(id, e) =>
        val (etype, _) = analyse(e, env, nongen, debug)
        newenv += (id.name -> etype)
        EmptyT
      case Num(_) => NumT
      //      case Let(v, defn, body) =>
      //        val defntype = analyse(defn, env, nongen)
      //        val newenv = env + (v -> defntype)
      //        analyse(body, newenv, nongen)
      //      case Letrec(v, defn, body) =>
      //        val newtype = newVar
      //        val newenv = env + (v -> newtype)
      //        val defntype = analyse(defn, newenv, nongen + newtype)
      //        unify(newtype, defntype)
      //        analyse(body, newenv, nongen)
    }
    if (debug) println(ast + ":\t" + t)
    (t, newenv)
  }

  def gettype(name: String, env: Env, nongen: Set[Var]): ExprT = {
    if (env.contains(name))
      fresh(env(name), nongen)
    else
      throw new ParseError("Undefined symbol " + name)
  }

  def fresh(t: ExprT, nongen: Set[Var]) = {
    import scala.collection.mutable
    val mappings = new mutable.HashMap[Var, Var]

    def freshrec(tp: ExprT): ExprT = {
      prune(tp) match {
        case v: Var =>
          if (isgeneric(v, nongen))
            mappings.getOrElseUpdate(v, newVar)
          else {
            v
          }
        case LambdaT(from, to) => LambdaT(freshrec(from), freshrec(to))
        case EmptyT => EmptyT
        case BoolT => BoolT
        case NumT => NumT
        case CharT => CharT
      }
    }

    freshrec(t)
  }


  def unify(t1: ExprT, t2: ExprT) {
    val type1 = prune(t1)
    val type2 = prune(t2)
    (type1, type2) match {
      case (a: Var, b) => if (a != b) {
        if (occursintype(a, b))
          throw new TypeError("recursive unification")
        a.instance = Some(b)
      }
      case (a, b: Var) => unify(b, a)
      case (LambdaT(froma, toa), LambdaT(fromb, tob)) =>
        unify(froma, fromb)
        unify(toa, tob)
      case (LambdaT(froma, _), b) => throw new TypeError(b + " cannot be applied (to " + froma + ")")
      case (a, b) if a != b => throw new TypeError("Type mismatch: " + a + "≠" + b)
      case (a, b) =>
    }
  }


  // Returns the currently defining instance of t.
  // As a side effect, collapses the list of type instances.
  def prune(t: ExprT): ExprT = t match {
    case v: Var if v.instance.isDefined =>
      val inst = prune(v.instance.get)
      v.instance = Some(inst)
      inst
    case _ => t
  }

  // Note: must be called with v 'pre-pruned'
  def isgeneric(v: Var, nongen: Set[Var]) = !(occursin(v, nongen))

  // Note: must be called with v 'pre-pruned'
  def occursintype(v: Var, type2: ExprT): Boolean = {
    prune(type2) match {
      case `v` => true
      //      case Oper(name, args) => occursin(v, args) // acredito que o escopo e o parser já resolvam
      case _ => false
    }
  }

  def occursin(t: Var, list: Iterable[ExprT]) =
    list exists (t2 => occursintype(t, t2))

  val checkDigits = "^(\\d+)$".r

  //  def isIntegerLiteral(name: String) = checkDigits.findFirstIn(name).isDefined

}

class HM2 extends TypeSystem {

  //  def main(args: Array[String]) {
  //    //    Console.setOut(new java.io.PrintStream(Console.out, true, "utf-8"))
  //
  //    val var1 = TypeSystem.newVar
  //    val var2 = TypeSystem.newVar
  //    val pairtype = TypeSystem.Oper("×", Array(var1, var2))
  //
  //    val var3 = TypeSystem.newVar
  //
  //    val myenv: TypeSystem.Env = Map.empty ++ Array(
  //      "pair" -> TypeSystem.Function(var1, TypeSystem.Function(var2, pairtype)),
  //      "true" -> TypeSystem.Bool,
  //      "cond" -> TypeSystem.Function(TypeSystem.Bool, TypeSystem.Function(var3, TypeSystem.Function(var3, var3))),
  //      "zero" -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Bool),
  //      "pred" -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Integer),
  //      "times" -> TypeSystem.Function(TypeSystem.Integer, TypeSystem.Function(TypeSystem.Integer, TypeSystem.Integer))
  //    )
  //
  //
  //    val pair = Appl(Appl(Ident("pair"), Appl(Ident("f"), Ident("4"))), Appl(Ident("f"), Ident("true")))
  //    val examples = Array[Expr](
  //      // factorial
  //      Letrec("factorial", // letrec factorial =
  //        Lambda("n", // fn n =>
  //          Appl(
  //            Appl( // cond (zero n) 1
  //              Appl(Ident("cond"), // cond (zero n)
  //                Appl(Ident("zero"), Ident("n"))),
  //              Ident("1")),
  //            Appl( // times n
  //              Appl(Ident("times"), Ident("n")),
  //              Appl(Ident("factorial"),
  //                Appl(Ident("pred"), Ident("n")))
  //            )
  //          )
  //        ), // in
  //        Appl(Ident("factorial"), Ident("5"))
  //      ),
  //
  //      // Should fail:
  //      // fn x => (pair(x(3) (x(true)))
  //      Lambda("x",
  //        Appl(
  //          Appl(Ident("pair"),
  //            Appl(Ident("x"), Ident("3"))),
  //          Appl(Ident("x"), Ident("true")))),
  //
  //      // pair(f(3), f(true))
  //      Appl(
  //        Appl(Ident("pair"), Appl(Ident("f"), Ident("4"))),
  //        Appl(Ident("f"), Ident("true"))),
  //
  //
  //      // letrec f = (fn x => x) in ((pair (f 4)) (f true))
  //      Let("f", Lambda("x", Ident("x")), pair),
  //
  //      // fn f => f f (fail)
  //      Lambda("f", Appl(Ident("f"), Ident("f"))),
  //
  //      // let g = fn f => 5 in g g
  //      Let("g",
  //        Lambda("f", Ident("5")),
  //        Appl(Ident("g"), Ident("g"))),
  //
  //      // example that demonstrates generic and non-generic Vars:
  //      // fn g => let f = fn x => g in pair (f 3, f true)
  //      Lambda("g",
  //        Let("f",
  //          Lambda("x", Ident("g")),
  //          Appl(
  //            Appl(Ident("pair"),
  //              Appl(Ident("f"), Ident("3"))
  //            ),
  //            Appl(Ident("f"), Ident("true"))))),
  //
  //      // Function composition
  //      // fn f (fn g (fn arg (f g arg)))
  //      Lambda("f", Lambda("g", Lambda("arg", Appl(Ident("g"), Appl(Ident("f"), Ident("arg"))))))
  //    )
  //    for (eg <- examples) {
  //      tryexp(eg, myenv)
  //    }
  //  }

  def tryexp(ast: Expr, env: Env = Map.empty) {
    print(ast + " : ")
    try {
      analyse(ast, env, print = true)
    } catch {
      case t: ParseError => print(t.getMessage)
      case t: TypeError => print(t.getMessage)
    }
    println
  }
}
