import evaluation.Interpreter
import inference.HM
import parsing.Grammar

import scala.io.Source

object Main extends App {
  val arq = Source.fromFile("test.tupi")
  val txt = arq.getLines().mkString("\n")
  val txt2 = txt.replace("\n", ";\n").replace("(;\n", "(\n").replace(";\n)", "\n)").replace(";\n;", ";").replace(";;", ";").lines().filter(!_.startsWith("~")).toArray().toList.mkString //.lines().map(_.trim).toArray().toList.mkString("\n").replace("\n}", "ŋ}").replace("{\n", "ŋ{").replace(":\n", "ŋ:").replace("\n", ";\n").replace("ŋ:", ":\n").replace("ŋ{", "{\n").replace("ŋ}", "\n}")
  val txt3 = if (txt2.endsWith(";")) txt2.dropRight(1) else txt2
  val st = System.currentTimeMillis()
  val parsed = Grammar.parse(txt3)
  println((System.currentTimeMillis() - st) + "ms")
  println(parsed + "\n")
  if (!parsed.successful) sys.exit()
  if (!HM.check(parsed.get)) sys.exit()
  val re = Interpreter.eval(parsed.get)
  println(re)
}