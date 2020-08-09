import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}
import scala.util.parsing.input.CharSequenceReader

object Test extends App with JavaTokenParsers with PackratParsers {

  type P[+T] = PackratParser[T]
  private val program = rep1sep("me" | "m", ";") ~ (";" ~> not("m") ~> "me") ^^ {
    case l ~ r => l :+ r
  }

  val txt2 = "m ;me; me; m; me"
  val chars = new CharSequenceReader(txt2)
  lazy val r = parseAll(phrase(program), new PackratReader(chars))
  println(r)
}
