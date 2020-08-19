/*
Based on the Andrew Forrest's adaptation for Scala
http://dysphoria.net/code/hindley-milner/HindleyMilner.scala

    Copyright 2020 Davi Pereira dos Santos
    This file is part of tupi.
    Initially written according to the guidelines in the Masterarbeit of Eugen Labun.

    Lamdheal is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Lamdheal is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with lamdheal.  If not, see <http://www.gnu.org/licenses/>.

Implementation of basic polymorphic type-checking for a simple language.
Based heavily on Nikita Borisov's Perl implementation at
http://web.archive.org/web/20050420002559/www.cs.berkeley.edu/~nikitab/courses/cs263/hm.html
which in turn is based on the paper by Luca Cardelli at
http://lucacardelli.name/Papers/BasicTypechecking.pdf
Do with it what you will.
"Do with [original work] what you will", but respect license for this version.
*/


//case class Let(v: String, defn: Printer, body: Printer) extends Printer
//
//case class Letrec(v: String, defn: Printer, body: Printer) extends Printer

class TypeError(msg: String) extends Exception(msg)

class ParseTypeError(msg: String) extends Exception(msg)

object HM extends AST {
  var last_line_with_errors = 0
  var globalEnv: Map[String, ExprT] = Map.empty
  var _nextVariableNameCounter = '§'
  var _nextVariableId = 0
  var last_new_var: Var = _


  //   def FunctionT(from: ExprT, to: ExprT) = ComplexType("->", Array(from, to))

  //  private lazy val MathOpT = FunT(NumT, FunT(NumT, NumT))

  private var last_types_with_errors = Array("", "")

  def nextUniqueName: String = {
    val result = _nextVariableNameCounter
    _nextVariableNameCounter = (_nextVariableNameCounter.toInt + 1).toChar
    result.toString
  }

  def newVariable: Var = {
    val result = _nextVariableId
    _nextVariableId += 1
    last_new_var = Var(result)
    last_new_var
  }

  //   def string(t: ExprT): String = t match {
  //      case v: Var => v.instance match {
  //         case Some(i) => string(i)
  //         case None => v.id
  //      }
  //      case SimpleType(id) => id
  //      case ComplexType(id, args) => {
  //         if (args.length == 1)
  //            id + "_of_" + string(args(0))
  //         else if (args.length == 2)
  //            "(" + string(args(0)) + " " + id + " " + string(args(1)) + ")"
  //         else
  //            args.mkString(id + " ", " ", "")
  //      }
  //      case x => "" //throw new TypeError("Unmatched " + x + " to pretty-print.")
  //   }

  def check(ast: Expr) {
    _nextVariableNameCounter = 'α'
    // Known operators/keywords and their respective types.
    last_line_with_errors = 0
    val hmC = new HMContext

    def tryexp(ast: Expr) {
      last_line_with_errors += 1
      last_types_with_errors = Array("", "")
      val t = hmC.analyse(ast)
      if (ast != Empty()) println("Type of " + ast + ": " + t + ".")
    }

    tryexp(ast)
    //    ast match {
    //      case Sequence(l) => l foreach tryexp
    //      case _ => tryexp(ast)
    //    }
  }

  class HMContext(var envi: Map[String, ExprT] = Map.empty) {

    def get_type_of_identifier(id: String, nongen: Set[Var]): ExprT = {
      if (envi.contains(id))
        fresh(envi(id), nongen)
      else
        throw new ParseTypeError("at line " + last_line_with_errors + ": Undefined symbol " + id)
    }

    def fresh(t: ExprT, nongen: Set[Var]): ExprT = {
      import scala.collection.mutable
      val mappings = new mutable.HashMap[Var, Var]

      def freshrec(tp: ExprT): ExprT = {
        prune(tp) match {
          case v: Var =>
            if (isgeneric(v, nongen)) mappings.getOrElseUpdate(v, newVariable) else v //.instance.get
          case EmptyT => EmptyT
          case BoolT => BoolT
          case NumT => NumT
          case CharT => CharT
          case LambdaT(from, to) => LambdaT(freshrec(from), freshrec(to))
//          case list: ListT => ListT(freshrec(list.elements_type))
          case x => throw new Exception("Unmatched case: " + x)
          //               case SimpleType(name) => SimpleType(name)
          //               case ComplexType(name, args) =>
          //                  ComplexType(name, args.map(freshrec(_)))
        }
      }

      freshrec(t)
    }


    // Returns the currently defining instance of t.
    // As a side effect, collapses the list of type instances.
    def prune(t: ExprT): ExprT = t match {
      case v: Var if v.instance.isDefined =>
        val inst = prune(v.instance.get) //var inst
        v.instance = Some(inst)
        inst
      case _ => t
    }

    // Note: must be called with v 'pre-pruned'
    def isgeneric(v: Var, nongen: Set[Var]): Boolean = !(occursin(v, nongen))

    // Note: must be called with v 'pre-pruned'
    def occursintype(v: Var, type2: ExprT): Boolean = prune(type2) match {
      case `v` => true
      //            case ComplexType(name, args) => occursin(v, args)
      case _ => false
    }

    def occursin(t: Var, list_of_nongen: Iterable[ExprT]): Boolean =
      list_of_nongen exists (t2 => occursintype(t, t2))

    def register_types_and_throw_exception(str1: String, str2: String): Nothing = {
      last_types_with_errors(0) = str1
      last_types_with_errors(1) = str2
      throw new TypeError("at line " + last_line_with_errors + ": " + str1 + " expected, but " + str2 + " found.")
    }

    def unify(t1: ExprT, t2: ExprT) {
      val type1 = prune(t1)
      val type2 = prune(t2)
      (type1, type2) match {
        case (a: Var, b) => if (a != b) {
          if (occursintype(a, b))
            throw new TypeError("at line " + last_line_with_errors + ": recursive unification.")
          a.instance = Some(b)
        }
        case (a, b: Var) => unify(b, a)
        case (a, b) if a.getClass != b.getClass => register_types_and_throw_exception(a.toString, b.toString)
        case (a, b) if a.getClass == b.getClass =>
      }
    }

    def analyse(ast: Expr): ExprT = analyse(ast, Set.empty)

    def math_op(a: Expr, b: Expr, nongen: Set[Var]): NumT.type = {
      val a_typed = analyse(a, nongen)
      val b_typed = analyse(b, nongen)
      unify(NumT, a_typed)
      unify(NumT, b_typed)
      NumT
    }

    def analyse(ast: Expr, nongen: Set[Var]): ExprT = {
      ast.t = ast match {
        case Empty() => EmptyT
        case a@Num(n) => NumT
        case Char(s) => CharT
        case Bool(s) => BoolT
        case Sequence(items) =>
          items map (it => analyse(it, nongen))
          items.last.t
        case Assign(id, e) =>
          envi += (id.name -> analyse(e))
          EmptyT
        case id: Ident =>
          id.t = get_type_of_identifier(id.name, nongen);
          id.t
        case Appl(f, arg) =>
          val funtype = analyse(f, nongen)
          val argtype = analyse(arg, nongen)
          val resulttype = newVariable
          unify(LambdaT(argtype, resulttype), funtype)
          resulttype
        case Lambda(arg, body) =>
          val argtype = newVariable
          envi += (arg.name -> argtype)
          val resulttype = analyse(body, nongen + argtype)
          LambdaT(argtype, resulttype)

        //      case li: List => {
        //        val element_types = li.exprs map (e => analyse(e, nongen))
        //        if (li.t.getClass == Empty.getClass) li.t = element_types.head
        //        try {
        //          element_types.tail map (unify(element_types.head, _))
        //        } catch {
        //          case e: TypeError => throw new TypeError("at line " + last_line_with_errors + ": " + e.getMessage + "\nHint: all elements should have the same type inside a list.")
        //        }
        //        li.t
        //      }
        //      case ConcatenateListExpr(a: Expr, b: Expr) => {
        //        val aT = analyse(a, nongen)
        //        val bT = analyse(b, nongen)
        //        var str = ""
        //        if (aT.getClass != ListT || bT.getClass != ListT)
        //          str = "\nHint: only lists or strings can be concatenated ('++')."
        //        if (aT.asInstanceOf[ListT].elements_type != bT.asInstanceOf[ListT].elements_type)
        //          str = "\nHint: only lists of the same type can be concatenated ('++')."
        //        try {
        //          unify(aT, bT)
        //        } catch {
        //          case e: TypeError => throw new TypeError("at line " + last_line_with_errors + ": " + e.getMessage + str)
        //        }
        //        aT
        //      }

        //EmptyT is not related to empty lists, just the opposite!
        //      case CommLineArgE => FunctionT(NumT, ListT(CharT))
        //      case BlockE(exprs) => {
        //         val lT = exprs.map {
        //            e =>
        //               analyse(e, env, nongen)
        //         }
        //         lT.last
        //      }
        //      case PrintE | PrintLnE => FunctionT(newVariable, EmptyT)
        //      case Show =>
        //      case Eval() => FunctionT(newVariable, EmptyT)
        //
        //      case exprs@ListaInterval(ini: ExprT, fim: ExprT) => {
        //         val iniT = analyse(ini, env, nongen)
        //         val fimT = analyse(fim, env, nongen)
        //         unify(iniT, NumT)
        //         unify(fimT, NumT)
        //         ListT(NumT)
        //      }
      }
      ast.t
    }
  }

}


//$x=0
//f = \\z z*z
//exprs = [1..10000] (\\y  ($x = $x + f y / 100; $x)  )
//`| $x
//n = "'`'\n"
//"the number is 'n'"

//as of rev. 266 (old HM inference):
//Time parsing 0.097s
//Time checking types 0.018s
//Time interpreting 0.567s
