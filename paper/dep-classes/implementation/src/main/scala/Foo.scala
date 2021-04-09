import dcc.entailment.SemanticEntailment
import dcc.syntax._
import dcc.types.Type
import smt.smtlib.syntax._

object Foo extends App {
  val s = SimpleSymbol("foo")
  val sem = new SemanticEntailment(List(
    ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Zero))), InstanceOf(Id('x), Id('Nat))),
    AbstractMethodDeclaration(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Nat))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat))))),
    ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Succ)), InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat))), InstanceOf(Id('x), Id('Nat))),
  ))
  sem.entails(Nil, PathEquivalence(Id(Symbol("y")), Id(Symbol("y"))))

  println("------------------------------------------------")

  val sem2 = new SemanticEntailment(Nil)
  sem2.entails(Nil, PathEquivalence(Id(Symbol("y")), Id(Symbol("y"))))
}