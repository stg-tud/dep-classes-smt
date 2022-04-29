package dcc.program

import dcc.syntax.{AbstractMethodDeclaration, ConstraintEntailment, ConstructorDeclaration, FieldAccess, FieldPath, InstanceOf, MethodImplementation, ObjectConstruction}
import dcc.syntax.Program.Program
import dcc.types.Type
import dcc.syntax.Implicit.StringToId

// TODO: add trait program
object NaturalNumbers {
  val program: Program = List(
    ConstructorDeclaration("Zero", "x", Nil),
    ConstraintEntailment("x", List(InstanceOf("x", "Zero")), InstanceOf("x", "Nat")),
    ConstructorDeclaration("Succ", "x", List(InstanceOf(FieldPath("x", "p"), "Nat"))),
    // Additional constructors with a flag. (for testing purposes)
//    ConstructorDeclaration("Zero", "x", List(InstanceOf("flag", "Zero"))),
//    ConstructorDeclaration("Succ", "x", List(InstanceOf(FieldPath("x", "p"), "Nat"), InstanceOf(FieldPath("x", "flag"), "Zero"))),
    ConstraintEntailment("x", List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Nat")), InstanceOf("x", "Nat")),
    AbstractMethodDeclaration("prev", "x", List(InstanceOf("x", "Nat")), Type("y", Set(InstanceOf("y", "Nat")))),
//    MethodImplementation("prev", "x", List(InstanceOf("x", "Zero")), Type("y", Set(InstanceOf("y", "Nat"))),
//      ObjectConstruction("Zero", Nil)),
    MethodImplementation("prev", "x", List(InstanceOf("x", "Zero")), Type("y", Set(InstanceOf("y", "Nat"))),
      "x"),
    MethodImplementation("prev", "x", List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Nat")), Type("y", Set(InstanceOf("y", "Nat"))),
      FieldAccess("x", "p"))
  )
}
