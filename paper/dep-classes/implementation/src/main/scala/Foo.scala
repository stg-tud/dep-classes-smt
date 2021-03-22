import dcc.entailment.SemanticEntailment
import dcc.syntax.{Id, PathEquivalence}
import smt.smtlib.SMTLibScript
import smt.smtlib.syntax.{Assert, SimpleSymbol}

object Foo extends App {
  val s = SimpleSymbol("foo")
  val sem = new SemanticEntailment(List())
  sem.entails(List(), PathEquivalence(Id(Symbol("x")), Id(Symbol("x"))))

  println("------------------------------------------------")

  val scriptMiddle: SMTLibScript = SMTLibScript(Seq(Assert(s)))
  val scriptRight = Seq(Assert(SimpleSymbol("bar")), Assert(SimpleSymbol("foobar")))
  val scriptLeft = Seq(Assert(SimpleSymbol("coo")), Assert(SimpleSymbol("shoo")))

  val scriptJoin: SMTLibScript = scriptMiddle ++ scriptRight
  val scriptFinal: SMTLibScript = scriptLeft ++: scriptJoin

  println(scriptFinal.format())
}