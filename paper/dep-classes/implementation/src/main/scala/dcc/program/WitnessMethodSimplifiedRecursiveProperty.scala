package dcc.program

import dcc.syntax.{AbstractMethodDeclaration, ConstraintEntailment, ConstructorDeclaration, FieldAccess, FieldPath, InstanceOf, MethodCall, MethodImplementation, ObjectConstruction}
import dcc.syntax.Program.Program
import dcc.syntax.Implicit.StringToId
import dcc.types.Type

object WitnessMethodSimplifiedRecursiveProperty {
  val program: Program = List(
    // Param classes
    ConstructorDeclaration("A", "x",  List()),
    ConstructorDeclaration("B", "x",  List()),
    ConstraintEntailment("x", List(InstanceOf("x", "A")), InstanceOf("x", "Param")),
    ConstraintEntailment("x", List(InstanceOf("x", "B")), InstanceOf("x", "Param")),
    // recursive data-structure
    ConstructorDeclaration("Nil", "x",  List(InstanceOf(FieldPath("x", "data"), "Param"))),
    ConstructorDeclaration("Cons", "x",  List(InstanceOf(FieldPath("x", "tl"), "List"))),
    ConstraintEntailment("x", List(InstanceOf("x", "Nil"), InstanceOf(FieldPath("x", "data"), "Param")), InstanceOf("x", "List")),
    ConstraintEntailment("x", List(InstanceOf("x", "Cons"), InstanceOf(FieldPath("x", "tl"), "List")), InstanceOf("x", "List")),
    // Witness class
    ConstructorDeclaration("Witness", "x",  List(InstanceOf(FieldPath("x", "choice"), "Param"))),
    // Result class
    ConstructorDeclaration("Result", "x", List()),
    // property witness method
    AbstractMethodDeclaration("property", "x", List(InstanceOf("x", "List")), Type("y", Set(InstanceOf("y", "Witness"), InstanceOf(FieldPath("y", "choice"), "Param")))),
    MethodImplementation("property", // case A
      "x", List(InstanceOf("x", "Nil"), InstanceOf(FieldPath("x", "data"), "A")),
      Type("y", Set(InstanceOf("y", "Witness"), InstanceOf(FieldPath("y", "choice"), "Param"))),
      ObjectConstruction("Witness", List(("choice", ObjectConstruction("A", List()))))
    ),
    MethodImplementation("property", // case B
      "x", List(InstanceOf("x", "Nil"), InstanceOf(FieldPath("x", "data"), "B")),
      Type("y", Set(InstanceOf("y", "Witness"), InstanceOf(FieldPath("y", "choice"), "Param"))),
      ObjectConstruction("Witness", List(("choice", ObjectConstruction("B", List()))))
    ),
    MethodImplementation("property", // case recursive call
      "x", List(InstanceOf("x", "Cons"), InstanceOf(FieldPath("x", "tl"), "List")),
      Type("y", Set(InstanceOf("y", "Witness"), InstanceOf(FieldPath("y", "choice"), "Param"))),
      MethodCall("property", FieldAccess("x", "tl"))
    ),
    // method over witness argument
    AbstractMethodDeclaration("m", "x", List(InstanceOf("x", "Witness"), InstanceOf(FieldPath("x", "choice"), "A")), Type("y", Set(InstanceOf("y", "Result")))),
    MethodImplementation("m", "x", List(InstanceOf("x", "Witness"), InstanceOf(FieldPath("x", "choice"), "A")), Type("y", Set(InstanceOf("y", "Result"))),
      ObjectConstruction("Result", List())
    )
  )
}
