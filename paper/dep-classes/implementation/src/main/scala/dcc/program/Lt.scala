package dcc.program

import dcc.syntax.{AbstractMethodDeclaration, ConstraintEntailment, ConstructorDeclaration, FieldAccess, FieldPath, InstanceOf, MethodCall, MethodImplementation, ObjectConstruction}
import dcc.syntax.Program.Program
import dcc.types.Type
import dcc.syntax.Implicit.StringToId

// Witness test
object Lt {
  val program: Program = List(
    // Bool
    ConstructorDeclaration("True", "b", Nil),
    ConstructorDeclaration("False", "b", Nil),
    ConstraintEntailment("b", List(InstanceOf("b", "True")), InstanceOf("b", "Bool")),
    ConstraintEntailment("b", List(InstanceOf("b", "False")), InstanceOf("b", "Bool")),
    // Nat
    ConstructorDeclaration("Zero", "x", Nil),
    ConstraintEntailment("x", List(InstanceOf("x", "Zero")), InstanceOf("x", "Nat")),
    ConstructorDeclaration("Succ", "x", List(InstanceOf(FieldPath("x", "p"), "Nat"))),
    ConstraintEntailment("x", List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Nat")), InstanceOf("x", "Nat")),
    AbstractMethodDeclaration("prev", "x", List(InstanceOf("x", "Nat")), Type("y", List(InstanceOf("y", "Nat")))),
    MethodImplementation("prev", "x", List(InstanceOf("x", "Zero")), Type("y", List(InstanceOf("y", "Nat"))),
      "x"),
    MethodImplementation("prev", "x", List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Nat")), Type("y", List(InstanceOf("y", "Nat"))),
      FieldAccess("x", "p")),
    // Lt
    ConstructorDeclaration("Lt", "a", List(
      InstanceOf(FieldPath("a", "n1"), "Nat"),
      InstanceOf(FieldPath("a", "n2"), "Nat"),
      InstanceOf(FieldPath("a", "b"), "Bool")
    )),
//    AbstractMethodDeclaration("lt", "a", List(InstanceOf(FieldPath("a", "n1"), "Nat"), InstanceOf(FieldPath("a", "n2"), "Nat")), Type("y", List(InstanceOf("y", "Lt")))),
//    // nat < zero
//    MethodImplementation("lt", "a",
//      List(InstanceOf(FieldPath("a", "n1"), "Nat"), InstanceOf(FieldPath("a", "n2"), "Zero")),
//      Type("y", List(InstanceOf("y", "Lt"))),
//      ObjectConstruction("Lt", List(
//        ("n1", FieldAccess("a", "n1")),
//        ("n2", FieldAccess("a", "n2")),
//        ("b", ObjectConstruction("False", List()))
//      ))
//    ),
//    // zero < succ
//    MethodImplementation("lt", "a",
//      List(InstanceOf(FieldPath("a", "n1"), "Zero"), InstanceOf(FieldPath("a", "n2"), "Succ")),
//      Type("y", List(InstanceOf("y", "Lt"))),
//      ObjectConstruction("Lt", List(
//        ("n1", FieldAccess("a", "n1")),
//        ("n2", FieldAccess("a", "n2")),
//        ("b", ObjectConstruction("True", List()))
//      ))
//    ),
//    // succ < succ
//    MethodImplementation("lt", "a",
//      List(InstanceOf(FieldPath("a", "n1"), "Succ"), InstanceOf(FieldPath("a", "n2"), "Succ")),
//      Type("y", List(InstanceOf("y", "Lt"))),
//      MethodCall("lt", ObjectConstruction("Lt", List(
//        ("n1", FieldAccess(FieldAccess("a", "n1"), "p")),
//        ("n2", FieldAccess(FieldAccess("a", "n2"), "p")),
//        ("b", ObjectConstruction("False", List()))
//      )))
//    )
    // Lt (argument Lt type)
    AbstractMethodDeclaration("lt",
      "a", List(InstanceOf("a", "Lt"), InstanceOf(FieldPath("a", "n1"), "Nat"), InstanceOf(FieldPath("a", "n2"), "Nat")),
      Type("y", List(InstanceOf("y", "Lt")))),
    // nat < zero
    MethodImplementation("lt", "a",
      List(InstanceOf("a", "Lt"), InstanceOf(FieldPath("a", "n1"), "Nat"), InstanceOf(FieldPath("a", "n2"), "Zero")),
      Type("y", List(InstanceOf("y", "Lt"))),
      ObjectConstruction("Lt", List(
        ("n1", FieldAccess("a", "n1")),
        ("n2", FieldAccess("a", "n2")),
        ("b", ObjectConstruction("False", List()))
      ))
    ),
    // zero < succ
    MethodImplementation("lt", "a",
      List(InstanceOf("a", "Lt"), InstanceOf(FieldPath("a", "n1"), "Zero"), InstanceOf(FieldPath("a", "n2"), "Succ")),
      Type("y", List(InstanceOf("y", "Lt"))),
      ObjectConstruction("Lt", List(
        ("n1", FieldAccess("a", "n1")),
        ("n2", FieldAccess("a", "n2")),
        ("b", ObjectConstruction("True", List()))
      ))
    ),
    // succ < succ
    MethodImplementation("lt", "a",
      List(InstanceOf("a", "Lt"), InstanceOf(FieldPath("a", "n1"), "Succ"), InstanceOf(FieldPath("a", "n2"), "Succ")),
      Type("y", List(InstanceOf("y", "Lt"))),
      MethodCall("lt", ObjectConstruction("Lt", List(
        ("n1", FieldAccess(FieldAccess("a", "n1"), "p")),
        ("n2", FieldAccess(FieldAccess("a", "n2"), "p")),
        ("b", ObjectConstruction("False", List()))
      )))
    )
  )
}