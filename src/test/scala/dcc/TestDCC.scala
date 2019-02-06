package dcc

import dcc.syntax.Program.Program
import dcc.syntax._
import org.scalatest.{FunSuite, PrivateMethodTester}

class TestDCC extends FunSuite with PrivateMethodTester {
  test("renameIdInPath") {
    val dcc = new DCC(List())

    val p = FieldPath(FieldPath(Id('x), Id('f)), Id('f1))
    val q = Id('x)

    val rename = PrivateMethod[Path]('renameIdInPath)

    assert((dcc invokePrivate rename(Id('x), Id('y), p)) == FieldPath(FieldPath(Id('y), Id('f)), Id('f1)))
    assert((dcc invokePrivate rename(Id('z), Id('y), p)) == FieldPath(FieldPath(Id('x), Id('f)), Id('f1)))
    assert((dcc invokePrivate rename(Id('x), Id('y), q)) == Id('y))
    assert((dcc invokePrivate rename(Id('y), Id('x), q)) == Id('x))
  }

  test("renameIdInConstraint") {
    val dcc = new DCC(List())

    val p = FieldPath(FieldPath(Id('x), Id('f)), Id('f1))
    val q = Id('x)
    val r = Id('z)


    val rename = PrivateMethod[Constraint]('renameIdInConstraint)

    assert((dcc invokePrivate rename(Id('x), Id('y), PathEquivalence(p, q))) ==
            PathEquivalence(FieldPath(FieldPath(Id('y), Id('f)), Id('f1)), Id('y)))
    assert((dcc invokePrivate rename(Id('z), Id('y), PathEquivalence(p, r))) ==
            PathEquivalence(FieldPath(FieldPath(Id('x), Id('f)), Id('f1)), Id('y)))
    assert((dcc invokePrivate rename(Id('y), Id('k), PathEquivalence(q, r))) ==
            PathEquivalence(Id('x), Id('z)))
    assert((dcc invokePrivate rename(Id('x), Id('y), InstanceOf(q, Id('cls)))) ==
            InstanceOf(Id('y), Id('cls)))
    assert((dcc invokePrivate rename(Id('x), Id('y), InstanceOf(r, Id('cls)))) ==
            InstanceOf(Id('z), Id('cls)))
    assert((dcc invokePrivate rename(Id('x), Id('y), InstantiatedBy(q, Id('cls)))) ==
            InstantiatedBy(Id('y), Id('cls)))
    assert((dcc invokePrivate rename(Id('x), Id('y), InstantiatedBy(r, Id('cls)))) ==
            InstantiatedBy(Id('z), Id('cls)))
  }

  test("alpha conversion") {
    val dcc = new DCC(List())
    val rename = PrivateMethod[List[Constraint]]('alphaConversion)

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

    val actual = dcc invokePrivate rename(Id('x), Id('m), cs)

    assert(actual == expected)
  }

  val naturalNumbers: Program = List(
    ConstructorDeclaration(Id('Zero), Id('x), Nil),
    ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Zero))), InstanceOf(Id('x), Id('Nat))),
    ConstructorDeclaration(Id('Succ), Id('x), List(InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat)))),
    ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Succ)), InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat))), InstanceOf(Id('x), Id('Nat))),
    AbstractMethodDeclaration(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Nat))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat))))),
    MethodImplementation(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Zero))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat)))),
      ObjectConstruction(Id('Zero), Nil)),
    MethodImplementation(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Succ)), InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat)))),
      FieldAccess(Id('x), Id('p)))
  )

  test("interp new object") {
    val dcc = new DCC(naturalNumbers)
    val (h1, e1) = dcc.interp(Map.empty, ObjectConstruction(Id('Zero), Nil))
    val (h2, e2) = dcc.interp(Map.empty, ObjectConstruction(Id('Succ), List((Id('p), ObjectConstruction(Id('Zero), Nil)))))

    assert(h1.size == 1)
    assert(h1(Id('x1)) == (Id('Zero), List()))
    assert(e1 == Id('x1))

    assert(h2.size == 2)
    assert(h2(Id('x2)) == (Id('Zero), List()))
    assert(h2(Id('x3)) == (Id('Succ), List((Id('p), Id('x2)))))
    assert(e2 == Id('x3))
  }
}
