package dcc.types

import dcc.entailment.SemanticEntailment
import dcc.program.{Empty, NaturalNumbers}
import dcc.syntax.{FieldAccess, FieldPath, InstanceOf, InstantiatedBy, MethodCall, ObjectConstruction, PathEquivalence}
import dcc.syntax.Implicit.StringToId
import dcc.types.TypeTests.{testTypeError, testTypeOk}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

class TestTypeOfNaturalNumbersRefreshChecker extends AnyFunSuite with BeforeAndAfterEach {
  // TODO: refactor newChecker → checker
  var newChecker: Checker = new FaithfulAdaptionChecker(Empty.program, new SemanticEntailment(Empty.program))

  override protected def beforeEach(): Unit = {
    newChecker = new FaithfulAdaptionChecker(NaturalNumbers.program, new SemanticEntailment(NaturalNumbers.program))
  }

  test ("type of bound variable") {
    val result = newChecker.typeOf(List(InstanceOf("x", "Nat")), "x")
    testTypeOk(result, Type("y", List(PathEquivalence("y", "x"))))
  }

  test ("type of unbound variable") {
    val result = newChecker.typeOf(Nil, "x") // error
    testTypeError(result, "variable 'x' is not available in context")
  }

  test ("type of field access") {
    val result = newChecker.typeOf(List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero")), FieldAccess("x", "p"))
    testTypeOk(result, Type("y", List(InstanceOf("y", "Zero"), InstanceOf("y", "Nat"))))
  }

  test ("type of method call 1") {
    val result = newChecker.typeOf(List(InstanceOf("x", "Zero")), MethodCall("prev", "x"))
    testTypeOk(result, Type("y", List(InstanceOf("y", "Nat"))))
  }

  test ("type of method call 2") {
    val result = newChecker.typeOf(List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero")), MethodCall("prev", "x"))
    testTypeOk(result, Type("y", List(InstanceOf("y", "Nat"))))
  }

  test ("type of method call 3") {
    val result = newChecker.typeOf(List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero")), MethodCall("prev", FieldAccess("x", "p")))
    testTypeOk(result, Type("y", List(InstanceOf("y", "Nat"))))
  }

  test ("type of 'new Zero'") {
    val result = newChecker.typeOf(Nil, ObjectConstruction("Zero", Nil))
    testTypeOk(result, Type("y", List(InstantiatedBy("y", "Zero"))))
  }

  test ("type of 'new Succ(x)") {
    val result = newChecker.typeOf(List(InstanceOf("x", "Zero")), ObjectConstruction("Succ", List(("p", "x")))) // unbound x in result type, is this a problem? no, as x is in the context
    testTypeOk(result, Type("y", List(InstantiatedBy("y", "Succ"), PathEquivalence(FieldPath("y", "p"), "x"))))
  }

  test ("type of 'new Succ(new Zero)") {
    val result = newChecker.typeOf(Nil, ObjectConstruction("Succ", List(("p", ObjectConstruction("Zero", Nil)))))
    testTypeOk(result, Type("y", List(InstantiatedBy("y", "Succ"), InstantiatedBy(FieldPath("y", "p"), "Zero"))))
  }

  test ("type of 'new Succ(new Succ(new Zero))") {
    val result = newChecker.typeOf(Nil, ObjectConstruction("Succ", List(("p", ObjectConstruction("Succ", List(("p", ObjectConstruction("Zero", Nil))))))) )
    testTypeOk(result, Type("y", List(InstantiatedBy("y", "Succ"), InstantiatedBy(FieldPath("y", "p"), "Succ"), InstantiatedBy(FieldPath(FieldPath("y", "p"), "p"), "Zero") )))
  }

  test ("type of 'new Nat") {
    val result = newChecker.typeOf(Nil, ObjectConstruction("Nat", Nil))  // Error: Nat
    testTypeError(result, "No constructor found for class 'Nat'")
  }
}
