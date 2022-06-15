package dcc.program

import dcc.syntax.{AbstractMethodDeclaration, ConstraintEntailment, ConstructorDeclaration, FieldPath, InstanceOf, MethodImplementation, ObjectConstruction, PathEquivalence}
import dcc.syntax.Program.Program
import dcc.syntax.Implicit.StringToId
import dcc.types.Type

object WitnessPathDependent {
  val program: Program = List(
    // Constructors
    ConstructorDeclaration("A", "x",  List()),
    ConstructorDeclaration("B", "x",  List()),
    ConstraintEntailment("x", List(InstanceOf("x", "A")), InstanceOf("x", "Param")),
    ConstraintEntailment("x", List(InstanceOf("x", "B")), InstanceOf("x", "Param")),
    ConstructorDeclaration("Witness", "x",  List(InstanceOf(FieldPath("x", "choice"), "Param"))),
    ConstructorDeclaration("Result", "x", List()),
    ConstructorDeclaration("Pair", "x", List(InstanceOf(FieldPath("x", "witness"), "Witness"), InstanceOf(FieldPath("x", "param"), "Param"))),
    // method over witness argument
    AbstractMethodDeclaration("m", "x",
      List(InstanceOf("x", "Pair"), InstanceOf(FieldPath("x", "witness"), "Witness"), InstanceOf(FieldPath("x", "param"), "Param"), PathEquivalence(FieldPath(FieldPath("x", "witness"), "choice"), FieldPath("x", "param"))),
      Type("y", Set(InstanceOf("y", "Result")))),
    // m(x. x.witness :: Witness, x.param :: Param, x.witness.choice==x.param)
    MethodImplementation("m", "x",
      List(InstanceOf("x", "Pair"), InstanceOf(FieldPath("x", "witness"), "Witness"), InstanceOf(FieldPath("x", "param"), "Param"), PathEquivalence(FieldPath(FieldPath("x", "witness"), "choice"), FieldPath("x", "param"))),
      Type("y", Set(InstanceOf("y", "Result"))),
      ObjectConstruction("Result", List())
    )
  )
}
