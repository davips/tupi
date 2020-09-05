package runtime

import scala.collection.mutable

case class LMap[T](m: Map[String, () => T] = Map[String, () => T]()) {
  val cache = mutable.Map.empty[String, Option[T]]

  def put(k: String, v: => T): LMap[T] = LMap(m + (k -> (() => v)))

  def get(k: String): Option[T] = {
    cache.getOrElse(k, m.get(k).map(_ ()))
  }

  def +(other: LMap[T]): LMap[T] = LMap(m ++ other.m)

  def apply(k: String): T = get(k).get
}

object LMapTest extends App {
  var m = LMap[Int]()
  m = m.put("a", 3)
  m = m.put("b", m("a") * 5)
  println(m("b"))
  println(m("b"))
  println(m("a"))
  println(m("a"))
  println(m("a"))
}