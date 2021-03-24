import dcc.entailment.SemanticEntailment
import dcc.syntax.{AbstractMethodDeclaration, ConstraintEntailment, Id, PathEquivalence, Type}
import smt.smtlib.SMTLibScript
import smt.smtlib.syntax.{Assert, SimpleSymbol}

object Foo extends App {
  val s = SimpleSymbol("foo")
  val sem = new SemanticEntailment(List())
  sem.entails(List(), PathEquivalence(Id(Symbol("x")), Id(Symbol("x"))))

  println("------------------------------------------------")

  println(sem.axioms(Nil).format())

  println("------------------------------------------------")

  val constraintEntailments: List[ConstraintEntailment] = List(
    ConstraintEntailment(Id('x), Nil, PathEquivalence(Id(Symbol("x")), Id(Symbol("x")))),
    AbstractMethodDeclaration(Id('foo), Id('x), Nil, Type(Id('y), Nil)),
    ConstraintEntailment(Id('y), Nil, PathEquivalence(Id(Symbol("x")), Id(Symbol("x")))),
    ConstraintEntailment(Id('z), Nil, PathEquivalence(Id(Symbol("x")), Id(Symbol("x"))))
  ).filter(_.isInstanceOf[ConstraintEntailment]).map(_.asInstanceOf[ConstraintEntailment])
  constraintEntailments.foreach(println)
}