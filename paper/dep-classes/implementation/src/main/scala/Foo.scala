import dcc.entailment.SemanticEntailment
import dcc.syntax.{Id, PathEquivalence}
import smt.smtlib.syntax.SimpleSymbol

object Foo extends App {
  val s = SimpleSymbol("foo")
  val sem = new SemanticEntailment(List())
  sem.entails(List(), PathEquivalence(Id(Symbol("x")), Id(Symbol("x"))))
}