import dcc.entailment.SemanticEntailment
import smt.smtlib.syntax.SimpleSymbol

object Foo extends App {
  val s = SimpleSymbol("foo")
  val sem = new SemanticEntailment(List())
  sem.entails
}