package dcc.types

import dcc.program.NaturalNumbers
import dcc.syntax.{AbstractMethodDeclaration, ConstraintEntailment, ConstructorDeclaration, FieldAccess, FieldPath, InstanceOf, MethodImplementation, ObjectConstruction}
import dcc.syntax.Implicit.StringToId
import org.scalatest.funsuite.AnyFunSuite

class TestTypeCheckDeclarations extends AnyFunSuite {
  private val checker = FaithfulAdaptionChecker(NaturalNumbers.program)

  test ("type check constructor declaration: Zero") {
    assert(checker.typeCheck(ConstructorDeclaration("Zero", "x", Nil)))
  }

  test ("type check constructor declaration: Succ") {
    assert(checker.typeCheck(ConstructorDeclaration("Succ", "x", List(InstanceOf(FieldPath("x", "p"), "Nat")))))
  }

  test ("type check entailment: Zero => Nat") {
    assert(checker.typeCheck(ConstraintEntailment("x", List(InstanceOf("x", "Zero")), InstanceOf("x", "Nat"))))
  }

  test ("type check entailment: Succ => Nat") {
    assert(checker.typeCheck(ConstraintEntailment("x", List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Nat")), InstanceOf("x", "Nat"))))
  }

  test ("type check abstract method declaration: prev") {
    assert(checker.typeCheck(AbstractMethodDeclaration("prev", "x", List(InstanceOf("x", "Nat")), Type("y", List(InstanceOf("y", "Nat"))))))
  }

  test ("type check method implementation: prev Zero case") {
    assert(checker.typeCheck(MethodImplementation("prev", "x", List(InstanceOf("x", "Zero")), Type("y", List(InstanceOf("y", "Nat"))),
      "x")))
  }

  test ("type check method implementation: prev Succ case") {
    assert(checker.typeCheck(MethodImplementation("prev", "x", List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Nat")), Type("y", List(InstanceOf("y", "Nat"))),
      FieldAccess("x", "p"))))
  }

  test ("type check method implementation: prev new Zero") {
    assert(checker.typeCheck(MethodImplementation("prev", "x", List(InstanceOf("x", "Zero")), Type("y", List(InstanceOf("y", "Nat"))),
      ObjectConstruction("Zero", Nil))))
  }
}
