import dcc.entailment.SemanticEntailment
import dcc.program.NaturalNumbers
import dcc.syntax._
import dcc.syntax.Implicit._
import dcc.syntax.Program.Program
import dcc.types.{FaithfulAdaptionChecker, IntegratedSubsumptionChecker, Type}

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
//  val result = newChecker.typeOf(List(InstanceOf("x", "Nat")), "x")
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
//  println(result.getOrElse(result.swap.getOrElse()))

//  println(newChecker.typeCheck(List(InstanceOf("x", "Nat")), "x", Type("y", List(PathEquivalence("x", "y")))))

  val program: Program = List(
    ConstructorDeclaration("Zero", "x", Nil),
    ConstraintEntailment("x", List(InstanceOf("x", "Zero")), InstanceOf("x", "Nat")),
    ConstructorDeclaration("Succ", "x", List(InstanceOf(FieldPath("x", "p"), "Nat"))),
    ConstraintEntailment("x", List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Nat")), InstanceOf("x", "Nat")),
    AbstractMethodDeclaration("prev", "x", List(InstanceOf("x", "Nat")), Type("y", List(InstanceOf("y", "Nat")))),
    MethodImplementation("prev", "x", List(InstanceOf("x", "Zero")), Type("y", List(InstanceOf("y", "Nat"))),
      "x"),
    MethodImplementation("prev", "x", List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Nat")), Type("y", List(InstanceOf("y", "Nat"))),
      FieldAccess("x", "p"))
  )

//  println(newChecker.typeCheck(
//    MethodImplementation("prev", "x", List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Nat")), Type("y", List(InstanceOf("y", "Nat"))),
//      FieldAccess("x", "p"))
//  ))

//  program.foreach(d => println(s"$d === ${newChecker.typeCheck(d)}"))

  println(newChecker.typeCheck)
}