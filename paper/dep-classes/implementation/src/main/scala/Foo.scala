import dcc.entailment.SemanticEntailment
import dcc.syntax.{AbstractMethodDeclaration, ConstraintEntailment, FieldPath, Id, InstanceOf, PathEquivalence, Type}
import smt.smtlib.SMTLibScript
import smt.smtlib.syntax.{Assert, SimpleSymbol}

object Foo extends App {
  val s = SimpleSymbol("foo")
  val sem = new SemanticEntailment(List(
    ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Zero))), InstanceOf(Id('x), Id('Nat))),
    AbstractMethodDeclaration(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Nat))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat))))),
    ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Succ)), InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat))), InstanceOf(Id('x), Id('Nat))),
  ))
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