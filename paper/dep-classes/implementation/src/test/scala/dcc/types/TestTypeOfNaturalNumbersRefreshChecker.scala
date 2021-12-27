package dcc.types

import dcc.entailment.EntailmentSort
import dcc.program.{Empty, NaturalNumbers}
import dcc.syntax.{FieldAccess, FieldPath, InstanceOf, InstantiatedBy, MethodCall, ObjectConstruction, PathEquivalence}
import dcc.syntax.Implicit.StringToId
import dcc.types.TypeTests.{testTypeError, testTypeOk}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

class TestTypeOfNaturalNumbersRefreshChecker extends AnyFunSuite with BeforeAndAfterEach {
  private var checker: Checker = new SomeInferenceChecker(Empty.program, EntailmentSort.GroundPathDepthLimit)

  override protected def beforeEach(): Unit = {
    checker = new SomeInferenceChecker(NaturalNumbers.program, EntailmentSort.GroundPathDepthLimit, debug = 1)
  }

  test ("type of bound variable") {
    val result = checker.typeOf(List(InstanceOf("x", "Nat")), "x")
    testTypeOk(result, Type("y", List(PathEquivalence("y", "x"))))
  }

  test ("type of unbound variable") {
    val result = checker.typeOf(Nil, "x") // error
    testTypeError(result, "variable 'x' is not available in context")
  }

  test ("type of field access") {
    val result = checker.typeOf(List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero")), FieldAccess("x", "p"))
    testTypeOk(result, Type("y", List(InstanceOf("y", "Zero"), InstanceOf("y", "Nat"))))
  }

  test ("type of method call 1") {
    val result = checker.typeOf(List(InstanceOf("x", "Zero")), MethodCall("prev", "x"))
    testTypeOk(result, Type("y", List(InstanceOf("y", "Nat"))))
  }

  test ("type of method call 2") {
    val result = checker.typeOf(List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero")), MethodCall("prev", "x"))
    testTypeOk(result, Type("y", List(InstanceOf("y", "Nat"))))
  }

  test ("type of method call 3") {
    val result = checker.typeOf(List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero")), MethodCall("prev", FieldAccess("x", "p")))
    testTypeOk(result, Type("y", List(InstanceOf("y", "Nat"))))
  }

  test ("type of 'new Zero'") {
    val result = checker.typeOf(Nil, ObjectConstruction("Zero", Nil))
    testTypeOk(result, Type("y", List(InstantiatedBy("y", "Zero"))))
  }

  test ("type of 'new Succ(x)") {
    val result = checker.typeOf(List(InstanceOf("x", "Zero")), ObjectConstruction("Succ", List(("p", "x")))) // unbound x in result type, is this a problem? no, as x is in the context
    testTypeOk(result, Type("y", List(InstantiatedBy("y", "Succ"), PathEquivalence(FieldPath("y", "p"), "x"))))
  }

  test ("type of 'new Succ(new Zero)") {
    val result = checker.typeOf(Nil, ObjectConstruction("Succ", List(("p", ObjectConstruction("Zero", Nil)))))
    testTypeOk(result, Type("y", List(InstantiatedBy("y", "Succ"), InstantiatedBy(FieldPath("y", "p"), "Zero"))))
  }

  test ("type of 'new Succ(new Succ(new Zero))") {
    val result = checker.typeOf(Nil, ObjectConstruction("Succ", List(("p", ObjectConstruction("Succ", List(("p", ObjectConstruction("Zero", Nil))))))) )
    testTypeOk(result, Type("y", List(InstantiatedBy("y", "Succ"), InstantiatedBy(FieldPath("y", "p"), "Succ"), InstantiatedBy(FieldPath(FieldPath("y", "p"), "p"), "Zero") )))
  }

  test ("type of 'new Nat") {
    val result = checker.typeOf(Nil, ObjectConstruction("Nat", Nil))  // Error: Nat
    testTypeError(result, "No constructor found for class 'Nat'")
  }
}
