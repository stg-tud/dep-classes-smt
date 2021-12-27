package dcc.types

import dcc.entailment.EntailmentSort
import dcc.program.NaturalNumbers
import dcc.syntax.{FieldAccess, FieldPath, InstanceOf, MethodCall, ObjectConstruction, PathEquivalence}
import dcc.syntax.Implicit.StringToId
import org.scalatest.funsuite.AnyFunSuite

class TestTypeCheckExpressions extends AnyFunSuite {
  private val checker = SomeInferenceChecker(NaturalNumbers.program, EntailmentSort.SimplifiedSemantic)

  test ("check bound variable") {
    assert(checker.typeCheck(List(InstanceOf("x", "Nat")), "x", Type("y", List(PathEquivalence("x", "y")))))
  }

  test ("check unbound variable") {
    assert(!checker.typeCheck(Nil, "x", Type("y", List(PathEquivalence("y", "x"))))) // error
  }

  test ("check field access") {
    assert(checker.typeCheck(List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero")), FieldAccess("x", "p"), Type("y", List(InstanceOf("y", "Nat")))))
  }

  test ("check method call 1") {
    assert(checker.typeCheck(List(InstanceOf("x", "Zero")), MethodCall("prev", "x"), Type("y", List(InstanceOf("y", "Nat")))))
  }

  test ("check method call 2") {
    assert(checker.typeCheck(List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero")), MethodCall("prev", "x"), Type("y", List(InstanceOf("y", "Nat")))))
  }

  test ("check method call 3") {
   assert(checker.typeCheck(List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero")), MethodCall("prev", FieldAccess("x", "p")), Type("y", List(InstanceOf("y", "Nat")))))
  }

  test ("check 'new Zero' is Nat") {
    assert(checker.typeCheck(Nil, ObjectConstruction("Zero", Nil), Type("y", List(InstanceOf("y", "Nat")))))
  }

  test ("check 'new Succ(x)' is Nat") {
    assert(checker.typeCheck(List(InstanceOf("x", "Zero")), ObjectConstruction("Succ", List(("p", "x"))), Type("y", List(InstanceOf("y", "Nat")))))
  }

  test ("check 'new Succ(new Zero)' is Nat") {
    assert(checker.typeCheck(Nil, ObjectConstruction("Succ", List(("p", ObjectConstruction("Zero", Nil)))), Type("y", List(InstanceOf("y", "Nat")))))
  }

  test ("check 'new Succ(new Succ(new Zero))' is Nat") {
    assert(checker.typeCheck(Nil, ObjectConstruction("Succ", List(("p", ObjectConstruction("Succ", List(("p", ObjectConstruction("Zero", Nil))))))), Type("y", List(InstanceOf("y", "Nat")))))
  }

  test ("check 'new Nat") {
    assert(!checker.typeCheck(Nil, ObjectConstruction("Nat", Nil), Type("irgendwas", List(InstanceOf("irgendwas", "Nat")))))  // Error: Nat
  }
}
