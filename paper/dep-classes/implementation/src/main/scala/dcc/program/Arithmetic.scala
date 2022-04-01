package dcc.program

import dcc.syntax.{AbstractMethodDeclaration, ConstraintEntailment, ConstructorDeclaration, FieldAccess, FieldPath, InstanceOf, MethodCall, MethodImplementation, ObjectConstruction}
import dcc.syntax.Program.Program
import dcc.syntax.Implicit.StringToId
import dcc.types.Type

// depclsj1 -> test -> Arithm
object Arithmetic {
  val program: Program = List(
    ConstructorDeclaration("Zero", "x", Nil),
    ConstructorDeclaration("Succ", "x", List(InstanceOf(FieldPath("x", "p"), "Nat"))),
    ConstructorDeclaration("Neg", "x", List(InstanceOf(FieldPath("x", "n"), "Succ"))),
    ConstraintEntailment("x", List(InstanceOf("x", "Zero")), InstanceOf("x", "Nat")),
    ConstraintEntailment("x", List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Nat")), InstanceOf("x", "Nat")),
    ConstraintEntailment("x", List(InstanceOf("x", "Neg"), InstanceOf(FieldPath("x", "n"), "Succ")), InstanceOf("x", "Nat")),
    ConstraintEntailment("x", List(InstanceOf("x", "Nat")), InstanceOf("x", "Int")),
    MethodImplementation("n0", "x", Nil, Type("y", List(InstanceOf("y", "Zero"))), ObjectConstruction("Zero", Nil)),
    AbstractMethodDeclaration("neg", "n", List(InstanceOf("n", "Int")), Type("y", List(InstanceOf("y", "Int")))),
    MethodImplementation("neg", "x", List(InstanceOf("x", "Zero")), Type("y", List(InstanceOf("y", "Int"))), MethodCall("n0", "x")),
    MethodImplementation("neg", "x", List(InstanceOf("x", "Succ")), Type("y", List(InstanceOf("y", "Int"))), ObjectConstruction("Neg", List(("n", "x")))),
    MethodImplementation("neg", "x", List(InstanceOf("x", "Neg")), Type("y", List(InstanceOf("y", "Int"))), FieldAccess("x", "n"))
  )
}
