package dcc

import dcc.syntax._
import dcc.Util._
import org.scalatest.{FunSuite, PrivateMethodTester}

class TestDCC extends FunSuite with PrivateMethodTester {
  test("renameIdInPath") {
    val p = FieldPath(FieldPath(Id('x), Id('f)), Id('f1))
    val q = Id('x)

    assert(renameIdInPath(Id('x), Id('y), p) == FieldPath(FieldPath(Id('y), Id('f)), Id('f1)))
    assert(renameIdInPath(Id('z), Id('y), p) == FieldPath(FieldPath(Id('x), Id('f)), Id('f1)))
    assert(renameIdInPath(Id('x), Id('y), q) == Id('y))
    assert(renameIdInPath(Id('y), Id('x), q) == Id('x))
  }

  test("renameIdInConstraint") {
    val p = FieldPath(FieldPath(Id('x), Id('f)), Id('f1))
    val q = Id('x)
    val r = Id('z)

    assert(renameIdInConstraint(Id('x), Id('y), PathEquivalence(p, q)) ==
            PathEquivalence(FieldPath(FieldPath(Id('y), Id('f)), Id('f1)), Id('y)))
    assert(renameIdInConstraint(Id('z), Id('y), PathEquivalence(p, r)) ==
            PathEquivalence(FieldPath(FieldPath(Id('x), Id('f)), Id('f1)), Id('y)))
    assert(renameIdInConstraint(Id('y), Id('k), PathEquivalence(q, r)) ==
            PathEquivalence(Id('x), Id('z)))
    assert(renameIdInConstraint(Id('x), Id('y), InstanceOf(q, Id('cls))) ==
            InstanceOf(Id('y), Id('cls)))
    assert(renameIdInConstraint(Id('x), Id('y), InstanceOf(r, Id('cls))) ==
            InstanceOf(Id('z), Id('cls)))
    assert(renameIdInConstraint(Id('x), Id('y), InstantiatedBy(q, Id('cls))) ==
            InstantiatedBy(Id('y), Id('cls)))
    assert(renameIdInConstraint(Id('x), Id('y), InstantiatedBy(r, Id('cls))) ==
            InstantiatedBy(Id('z), Id('cls)))
  }

  test("alpha conversion") {
    val cs = List(
      PathEquivalence(Id('x), Id('z)),
      PathEquivalence(FieldPath(FieldPath(Id('y), Id('f)), Id('f1)), Id('x)),
      InstanceOf(Id('x), Id('cls)),
      InstantiatedBy(Id('z), Id('cls))
    )

    val expected = List(
      PathEquivalence(Id('m), Id('z)),
      PathEquivalence(FieldPath(FieldPath(Id('y), Id('f)), Id('f1)), Id('m)),
      InstanceOf(Id('m), Id('cls)),
      InstantiatedBy(Id('z), Id('cls))
    )

    val actual = alphaConversion(Id('x), Id('m), cs)

    assert(actual == expected)
  }
}
