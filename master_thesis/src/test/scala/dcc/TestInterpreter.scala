package dcc

import dcc.syntax._
import dcc.syntax.Program.Program
import org.scalatest.FunSuite

class TestInterpreter extends FunSuite {
  type Obj = (Id, List[(Id, Id)])
  type Heap = Map[Id, Obj]

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
    val (h2, e2) = dcc.interp(Map.empty, ObjectConstruction(Id('Succ), List((Id('p), ObjectConstruction(Id('Zero), Nil)))))

    assert(h2.size == 2)
    assert(h2(Id('x2)) == (Id('Zero), List()))
    assert(h2(Id('x3)) == (Id('Succ), List((Id('p), Id('x2)))))
    assert(e2 == Id('x3))
  }

  test("new two") {
    val dcc = new DCC(naturalNumbers)
    val (h1, e1) = dcc.interp(Map.empty, ObjectConstruction(Id('Zero), Nil))
    val (h2, e2) = dcc.interp(h1, ObjectConstruction(Id('Succ), List((Id('p), e1))))
    val (h3, e3) = dcc.interp(h2, ObjectConstruction(Id('Succ), List((Id('p), e2))), preOptimize = true)


    assert(h1.size == 1)
    assert(h1(Id('x1)) == (Id('Zero), List()))
    assert(e1 == Id('x1))

    assert(h2.size == 2)
    assert(h2(Id('x1)) == (Id('Zero), List()))
    assert(h2(Id('x2)) == (Id('Succ), List((Id('p), Id('x1)))))
    assert(e2 == Id('x2))

    assert(h3.size == 3)
    assert(h3(Id('x1)) == (Id('Zero), List()))
    assert(h3(Id('x2)) == (Id('Succ), List((Id('p), Id('x1)))))
    assert(h3(Id('x3)) == (Id('Succ), List((Id('p), Id('x2)))))
  }

  test("prev(Zero)") {
    val dcc = new DCC(naturalNumbers)
    val (h1, e1) = dcc.interp(Map.empty, MethodCall(Id('prev), ObjectConstruction(Id('Zero), Nil)))

    assert(h1.size == 2)
    h1.forall{
      case (_, (Id('Zero), Nil)) => true
      case _ => false
    }
    assert(e1 == Id('x2))
  }

  test("prev(Succ)") {
    val dcc = new DCC(naturalNumbers)
    val h: Heap = Map(
      Id('x) -> (Id('Zero), Nil),
      Id('y) -> (Id('Succ), List((Id('p), Id('x)))))
    val e = MethodCall(Id('prev), Id('y))

    val (h1, e1) = dcc.interp(h, e)

    assert(h == h1)
    assert(e1 == Id('x))
  }
}
