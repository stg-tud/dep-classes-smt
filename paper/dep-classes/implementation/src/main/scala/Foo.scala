import dcc.entailment.SemanticEntailment
import dcc.program.NaturalNumbers
import dcc.syntax._
import dcc.syntax.Implicit._
import dcc.types.{FaithfulAdaptionChecker, IntegratedSubsumptionChecker}

object Foo extends App {
  // TODO: is there a way to force the program if the entailment to be the same as the program of the checker/interpreter?
  val sem3 = new SemanticEntailment(NaturalNumbers.program)
  val checker = new IntegratedSubsumptionChecker(NaturalNumbers.program, sem3)

  println(checker.typecheck) // TODO returns false, one check is sat. investigate (x::Zero |- x::Succ is not valid, sat result makes sense, further investigate the problem)

  println("------------------------------------------------")

  val newChecker  = new FaithfulAdaptionChecker(NaturalNumbers.program, sem3)
//  val result = newChecker.typeOf(List(InstanceOf("x", "Nat")), "x")
//  val result = newChecker.typeOf(Nil, "x") // error
  val result = newChecker.typeOf(List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero")), FieldAccess("x", "p"))
  println(result.getOrElse(result.swap.getOrElse()))
}