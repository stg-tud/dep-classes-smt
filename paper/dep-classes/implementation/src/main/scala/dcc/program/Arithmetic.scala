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
    // constants
    MethodImplementation("n0", "x", Nil, Type("y", List(InstanceOf("y", "Zero"))), ObjectConstruction("Zero", Nil)),
    MethodImplementation("n1", "x", Nil, Type("y", List(InstanceOf("y", "Succ"), InstanceOf(FieldPath("y", "p"), "Zero"))), ObjectConstruction("Succ", List(("p", ObjectConstruction("Zero", Nil))))),
//    MethodImplementation("n1", "x", Nil, Type("y", List(InstanceOf("y", "Succ"), InstanceOf(FieldPath("y", "p"), "Zero"))), ObjectConstruction("Succ", List(("p", MethodCall("n0", "x"))))), // not possible, since x not in context (empty constraints) TODO: add no argument method calls?
    // neg method
    AbstractMethodDeclaration("neg", "x", List(InstanceOf("x", "Int")), Type("y", List(InstanceOf("y", "Int")))),
    MethodImplementation("neg", "x", List(InstanceOf("x", "Zero")), Type("y", List(InstanceOf("y", "Int"))), MethodCall("n0", "x")),
    MethodImplementation("neg", "x", List(InstanceOf("x", "Succ")), Type("y", List(InstanceOf("y", "Int"))), ObjectConstruction("Neg", List(("n", "x")))),
    MethodImplementation("neg", "x", List(InstanceOf("x", "Neg"), InstanceOf(FieldPath("x", "n"), "Nat")), Type("y", List(InstanceOf("y", "Int"))), FieldAccess("x", "n")),
    // prev method
    AbstractMethodDeclaration("prev", "x", List(InstanceOf("x", "Int")), Type("y", List(InstanceOf("y", "Int")))),
    MethodImplementation("prev", "x", List(InstanceOf("x", "Zero")), Type("y", List(InstanceOf("y", "Int"))), MethodCall("neg", MethodCall("n1", "x"))),
    MethodImplementation("prev", "x", List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Nat")), Type("y", List(InstanceOf("y", "Int"))), FieldAccess("x", "p")),
    MethodImplementation("prev", "x", List(InstanceOf("x", "Neg"), InstanceOf(FieldPath("x", "n"), "Nat")), Type("y", List(InstanceOf("y", "Int"))), MethodCall("neg", ObjectConstruction("Succ", List(("p", FieldAccess("x", "n")))))),
    // succ method
    AbstractMethodDeclaration("succ", "x", List(InstanceOf("x", "Int")), Type("y", List(InstanceOf("y", "Int")))),
    MethodImplementation("succ", "x", List(InstanceOf("x", "Nat")), Type("y", List(InstanceOf("y", "Int"))), ObjectConstruction("Succ", List(("p", "x")))),
    MethodImplementation("succ", "x", List(InstanceOf("x", "Neg"), InstanceOf(FieldPath("x", "n"), "Succ"), InstanceOf(FieldPath(FieldPath("x", "n"), "p"), "Zero")), Type("y", List(InstanceOf("y", "Int"))), MethodCall("n0", "x")),
    MethodImplementation("succ", "x", List(InstanceOf("x", "Neg"), InstanceOf(FieldPath("x", "n"), "Succ"), InstanceOf(FieldPath(FieldPath("x", "n"), "p"), "Succ"), InstanceOf(FieldPath(FieldPath(FieldPath("x", "n"), "p"), "p"), "Nat")), Type("y", List(InstanceOf("y", "Int"))), ObjectConstruction("Neg", List(("n", FieldAccess(FieldAccess("x", "n"), "p")))))
  )
}
