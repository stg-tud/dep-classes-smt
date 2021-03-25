import dcc.entailment.SemanticEntailment
import dcc.syntax._
import smt.smtlib.syntax._

object Foo extends App {
  val s = SimpleSymbol("foo")
  val sem = new SemanticEntailment(List(
    ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Zero))), InstanceOf(Id('x), Id('Nat))),
    AbstractMethodDeclaration(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Nat))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat))))),
    ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Succ)), InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat))), InstanceOf(Id('x), Id('Nat))),
  ))
  sem.entails(List(), PathEquivalence(Id(Symbol("x")), Id(Symbol("x"))))

  println("------------------------------------------------")

  println(sem.axioms(List(
    PathEquivalence(Id(Symbol("x")), Id(Symbol("y"))),
    PathEquivalence(FieldPath(Id(Symbol("z")), Id(Symbol("f"))), Id(Symbol("x"))),
    PathEquivalence(FieldPath(FieldPath(Id(Symbol("z")), Id(Symbol("g"))), Id(Symbol("f"))), Id(Symbol("y"))),
  )).format())

  println("------------------------------------------------")
}