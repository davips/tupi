import scala.collection.mutable

case class LMap[T](m: mutable.Map[String, () => T] = mutable.Map[String, () => T]()) {
  val cache = mutable.Map.empty[String,T]
  def put(k: String, v: => T): Unit = {
    m += k -> (() => v)
  }

  def get(k: String): T = cache.getOrElseUpdate(k, m(k)())
}

//private val m = LMap[Int]()
//m.add("a", 3)
//m.add("b", m.get("a") * 5)
//println(m.get("b"))
//println(m.get("b"))
