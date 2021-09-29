// Copyright (c) 2021. Davi Pereira dos Santos
// This file is part of the tupi project.
// Please respect the license - more about this in the section (*) below.
//
// tupi is free software: you can redistribute it and/or pify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// tupi is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with tupi.  If not, see <http://www.gnu.org/licenses/>.
//
// (*) Removing authorship by any means, e.g. by distribution of derived
// works or verbatim, obfuscated, compiled or rewritten versions of any
// part of this work is illegal and it is unethical regarding the effort and
// time spent here.

package algebra

import ky.korins.blake3.Blake3

case class Hosh(blob: Option[Array[Byte]], cells: Array[BigInt]) {
  val p: BigInt = 1099511627689L
  val n: BigInt = if (blob.isDefined) {
    var divmod: (BigInt, BigInt) = Blake3.bigInt(blob.get, 240) -> 0
    for (i <- 5 to 0) {
      divmod = divmod._1 /% p
      cells(i) = divmod._2
    }
    n
  } else {
    cells(5) + cells(4) * p + cells(3) * p.pow(2) + cells(2) * p.pow(3) + cells(1) * p.pow(4) + cells(0) * p.pow(5)
  }

  def *(that: Hosh): Hosh = Hosh(None, Array(
    (cells(0) + that.cells(0)) % p,
    (cells(1) + that.cells(1)) % p,
    (cells(2) + that.cells(2) + cells(3) * that.cells(0)) % p,
    (cells(3) + that.cells(3)) % p,
    (cells(4) + that.cells(4) + cells(1) * that.cells(3)) % p,
    (cells(5) + that.cells(5) + cells(1) * that.cells(2) + cells(4) * that.cells(0)) % p
  ))
}

object Hosh {
  def apply(blob: Array[Byte]): Hosh = new Hosh(Some(blob), Array.fill(6)(0))

  def apply(cells: Array[BigInt]): Hosh = new Hosh(None, cells)
}
