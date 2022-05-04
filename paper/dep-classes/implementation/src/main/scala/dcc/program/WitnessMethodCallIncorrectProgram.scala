package dcc.program

import dcc.syntax.{AbstractMethodDeclaration, ConstraintEntailment, ConstructorDeclaration, FieldPath, InstanceOf, MethodImplementation, ObjectConstruction}
import dcc.syntax.Program.Program
import dcc.syntax.Implicit.StringToId
import dcc.types.Type

object WitnessMethodCallIncorrectProgram {
  val program: Program = List(
    ConstructorDeclaration("A", "x",  List()),
    ConstructorDeclaration("B", "x",  List()),
    ConstraintEntailment("x", List(InstanceOf("x", "A")), InstanceOf("x", "Param")),
    ConstraintEntailment("x", List(InstanceOf("x", "B")), InstanceOf("x", "Param")),
    ConstructorDeclaration("Witness", "x",  List(InstanceOf(FieldPath("x", "choice"), "Param"))),
    ConstructorDeclaration("Result", "x", List()),
    // witness method
    AbstractMethodDeclaration("property", "x", List(InstanceOf("x", "Param")), Type("y", Set(InstanceOf("y", "Witness"), InstanceOf(FieldPath("y", "choice"), "Param")))),
    MethodImplementation("property", "x", List(InstanceOf("x", "A")), Type("y", Set(InstanceOf("y", "Witness"), InstanceOf(FieldPath("y", "choice"), "A"))),
      ObjectConstruction("Witness", List(("choice", ObjectConstruction("A", List()))))
    ),
    MethodImplementation("property", "x", List(InstanceOf("x", "B")), Type("y", Set(InstanceOf("y", "Witness"), InstanceOf(FieldPath("y", "choice"), "B"))),
      ObjectConstruction("Witness", List(("choice", ObjectConstruction("B", List()))))
    ),
    // method over witness argument
    AbstractMethodDeclaration("m", "x", List(InstanceOf("x", "Witness"), InstanceOf(FieldPath("x", "choice"), "A")), Type("y", Set(InstanceOf("y", "Result")))),
    MethodImplementation("m", "x", List(InstanceOf("x", "Witness"), InstanceOf(FieldPath("x", "choice"), "A")), Type("y", Set(InstanceOf("y", "Result"))),
      ObjectConstruction("Result", List())
    )
  )
}
