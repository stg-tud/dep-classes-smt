import dcc.entailment.SemanticEntailment
import dcc.program.NaturalNumbers
import dcc.syntax._
import dcc.syntax.Implicit._
import dcc.types.{FaithfulAdaptionChecker, IntegratedSubsumptionChecker, Type}
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

  println("------------------------------------------------")

  // TODO: is there a way to force the program if the entailment to be the same as the program of the checker/interpreter?
  val sem3 = new SemanticEntailment(NaturalNumbers.program)
  val checker = new IntegratedSubsumptionChecker(NaturalNumbers.program, sem3)

  println(checker.typecheck) // TODO returns false, one check is sat. investigate (x::Zero |- x::Succ is not valid, sat result makes sense, further investigate the problem)

  println("------------------------------------------------")

  val newChecker  = new FaithfulAdaptionChecker(NaturalNumbers.program, sem3)
  val result = newChecker.typeOf(List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero")), FieldAccess("x", "p"))
  println(result)
}