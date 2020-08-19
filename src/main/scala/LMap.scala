import scala.collection.mutable

case class LMap[T](m: Map[String, () => T] = Map[String, () => T]()) {
  val cache = mutable.Map.empty[String, T]

  def put(k: String, v: => T): LMap[T] = LMap(m + (k -> (() => v)))

  def get(k: String): T = cache.getOrElseUpdate(k, m(k)())
}

object LMapTest extends App {
  var m = LMap[Int]()
  m = m.put("a", 3)
  m = m.put("b", m.get("a") * 5)
  println(m.get("b"))
  println(m.get("b"))
  println(m.get("a"))
  println(m.get("a"))
  println(m.get("a"))
}