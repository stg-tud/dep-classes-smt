import smt.smtlib.syntax.SimpleSymbol

object Foo extends App {
  val s = SimpleSymbol("foo")
  println(s.format())
}
