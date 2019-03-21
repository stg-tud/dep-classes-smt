package dcc

import dcc.syntax._
import dcc.syntax.Program.Program
import org.scalatest.FunSuite

class TestInterpreter extends FunSuite {
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

  test("new Zero") {
    val dcc = new DCC(naturalNumbers)
    val (h1, e1) = dcc.interp(Map.empty, ObjectConstruction(Id('Zero), Nil))

    assert(h1.size == 1)
    assert(h1(Id('x1)) == (Id('Zero), List()))
    assert(e1 == Id('x1))
  }

  test("new Succ(Zero)") {
    val dcc = new DCC(naturalNumbers)
    val (h1, e1) = dcc.interp(Map.empty, ObjectConstruction(Id('Zero), Nil))
    val (h2, e2) = dcc.interp(Map.empty, ObjectConstruction(Id('Succ), List((Id('p), ObjectConstruction(Id('Zero), Nil)))))

    assert(h1.size == 1)
    assert(h1(Id('x1)) == (Id('Zero), List()))
    assert(e1 == Id('x1))

    // TODO: help solver with instantiating program entailments
    assert(h2.size == 2)
    assert(h2(Id('x2)) == (Id('Zero), List()))
    assert(h2(Id('x3)) == (Id('Succ), List((Id('p), Id('x2)))))
    assert(e2 == Id('x3))
  }
}
