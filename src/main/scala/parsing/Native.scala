// Copyright (c) 2020. Davi Pereira dos Santos
//     This file is part of the tupi project.
//     Please respect the license. Removing authorship by any means
//     (by code make up or closing the sources) or ignoring property rights
//     is a crime and is unethical regarding the effort and time spent here.
//     Relevant employers or funding agencies will be notified accordingly.
//
//     tupi is free software: you can redistribute it and/or modify
//     it under the terms of the GNU General Public License as published by
//     the Free Software Foundation, either version 3 of the License, or
//     (at your option) any later version.
//
//     tupi is distributed in the hope that it will be useful,
//     but WITHOUT ANY WARRANTY; without even the implied warranty of
//     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//     GNU General Public License for more details.
//
//     You should have received a copy of the GNU General Public License
//     along with tupi.  If not, see <http://www.gnu.org/licenses/>.
//
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
