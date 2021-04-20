import dcc.entailment.SemanticEntailment
import dcc.program.NaturalNumbers
import dcc.syntax._
import dcc.syntax.Implicit._
import dcc.types.{FaithfulAdaptionChecker, IntegratedSubsumptionChecker}
import smt.smtlib.syntax.Numeral

object Foo extends App {
  // TODO: is there a way to force the program if the entailment to be the same as the program of the checker/interpreter?
  val sem3 = new SemanticEntailment(NaturalNumbers.program)
//  val checker = new IntegratedSubsumptionChecker(NaturalNumbers.program, sem3)
//
//  println(checker.typecheck) // TODO returns false, one check is sat. investigate (x::Zero |- x::Succ is not valid, sat result makes sense, further investigate the problem)
//
//  println("------------------------------------------------")

  // TODO: move this to a dedicated test
  val newChecker  = new FaithfulAdaptionChecker(NaturalNumbers.program, sem3)
  val result = newChecker.typeOf(List(InstanceOf("x", "Nat")), "x")
//  val result = newChecker.typeOf(Nil, "x") // error
//  val result = newChecker.typeOf(List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero")), FieldAccess("x", "p"))
//  val result = newChecker.typeOf(List(InstanceOf("x", "Zero")), MethodCall("prev", "x"))
//  val result = newChecker.typeOf(List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero")), MethodCall("prev", "x"))
//  val result = newChecker.typeOf(List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero")), MethodCall("prev", FieldAccess("x", "p")))
//  val result = newChecker.typeOf(Nil, ObjectConstruction("Zero", Nil))
//  val result = newChecker.typeOf(List(InstanceOf("x", "Zero")), ObjectConstruction("Succ", List(("p", "x")))) // unbound x in result type, is this a problem? no, as x is in the context
//  val result = newChecker.typeOf(Nil, ObjectConstruction("Succ", List(("p", ObjectConstruction("Zero", Nil)))))
//  val result = newChecker.typeOf(Nil, ObjectConstruction("Succ", List(("p", ObjectConstruction("Succ", List(("p", ObjectConstruction("Zero", Nil))))))) )
//  val result = newChecker.typeOf(Nil, ObjectConstruction("Nat", Nil))  // Error: Nat
  println(result.getOrElse(result.swap.getOrElse()))

  val n = Numeral(1000000000000000000l)
  println(n.format())
}