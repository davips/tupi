package inference

import inference.Types._
import parsing.AST._

import scala.util.matching.Regex

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

class Undefined(msg: String) extends Exception(msg)

class TypeError(msg: String) extends Exception(msg)

class ParseTypeError(msg: String) extends Exception(msg)

class TypeSystem {

  type Env = Map[String, ExprT]
  var _nextVarName = 'α';
  var _nextVarId = 0

  def nextUniqueName: String = {
    val result = _nextVarName
    _nextVarName = (_nextVarName.toInt + 1).toChar
    result.toString
  }

  def newVar: Var = {
    val result = _nextVarId
    _nextVarId += 1
    Var(result)
  }

  def analyse(ast: Expr, env: Env, print: Boolean = false): ExprT = analyse(ast, env, Set.empty, print)._1

  def analyse(ast: Expr, env: Env, nongen: Set[Var], debug: Boolean): (ExprT, Env) = {
    //    println("AST: " + ast)
    var newenv = env
    if (ast.t.isDefined) (ast.t.get, env) else {
      val t = ast match {
        case s: Scala => s.t.get
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
          try {
            val (etype, _) = analyse(e, env, nongen, debug)
            newenv += (id.name -> etype)
          } catch { //loop?
            case p: Undefined =>
              println(p.getMessage)
              if (p.getMessage == id.name) {
                println("Loop detectado!")
                val newtype = newVar
                newenv += (id.name -> newtype)
                val (etype, _) = analyse(e, newenv, nongen + newtype, debug)
                unify(newtype, etype)
                analyse(e, newenv, nongen, debug)
              } else throw p
          }
          EmptyT
        case Num(_) => NumT
      }
      if (debug) println(ast + ":\t" + t)
      (t, newenv)
    }
  }

  def gettype(name: String, env: Env, nongen: Set[Var]): ExprT = {
    if (env.contains(name))
      fresh(env(name), nongen)
    else
      throw new Undefined(name)
  }

  def fresh(t: ExprT, nongen: Set[Var]): ExprT = {
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
    //    println("ty1: " + type1 + " ty2:" + type2)
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
      case (l@LambdaT(froma, body), func) => throw new TypeError("Expected: " + func + ". Found: " + l + ")")
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
  def isgeneric(v: Var, nongen: Set[Var]): Boolean = !(occursin(v, nongen))

  // Note: must be called with v 'pre-pruned'
  def occursintype(v: Var, type2: ExprT): Boolean = {
    prune(type2) match {
      case `v` => true
      //      case Oper(name, args) => occursin(v, args) // acredito que o escopo e o parser já resolvam
      case _ => false
    }
  }

  def occursin(t: Var, list: Iterable[ExprT]): Boolean =
    list exists (t2 => occursintype(t, t2))

  //  val checkDigits: Regex = "^(\\d+)$".r  //  def isIntegerLiteral(name: String) = checkDigits.findFirstIn(name).isDefined
}

object HM extends TypeSystem {
  def check(ast: Expr, env: Env = Map.empty): Boolean = {
    try {
      analyse(ast, env, print = true)
    } catch {
      case t: Undefined =>
        print("inference.Undefined symbol " + t.getMessage)
        return false
      case t: TypeError =>
        print(t.getMessage)
        return false
    }
    println
    true
  }
}
