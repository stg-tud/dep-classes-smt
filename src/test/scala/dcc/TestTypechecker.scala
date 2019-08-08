package dcc

import dcc.syntax._
import dcc.syntax.Program.Program
import org.scalatest.FunSuite
//import syntax.Util._

class TestTypechecker extends FunSuite {
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

  test ("wf natural numbers") {
    val dcc = new DCC(naturalNumbers)

    assert(dcc.typecheck(skipGen = false))
  }

//  test ( "typeass prev(Succ)") {
//    val dcc = new DCC(naturalNumbers)
//
//      val h = Map(
//        Id('x) -> (Id('Zero), Nil),
//        Id('y) -> (Id('Succ), List((Id('p), Id('x)))))
//
//    val types = dcc.typeassignment(List(
//      InstanceOf('x, 'Zero),
//      InstanceOf('y, 'Succ),
//      InstanceOf(FieldPath('y, 'p), 'Nat),
//      PathEquivalence(FieldPath('y, 'p), 'x)
//    ), MethodCall('prev, 'y))
//
//    types.foreach(println)
//  }
}
