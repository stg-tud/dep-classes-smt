package dcc

import dcc.syntax.Program.Program
import dcc.syntax.{Constraint, ConstructorDeclaration, FieldPath, Id, InstantiatedBy, PathEquivalence}

import scala.annotation.tailrec

object DCC {
  type Obj = (Id, List[(Id, Id)])
  type Heap = Map[Id, Obj]

  // Heap Constraints
  def HC(heap: Heap): List[Constraint] = heap.flatMap{case (x, o) => OC(x, o)}.toList
  //heap.map{case (x, o) => OC(x, o)}.flatten.toList

  // Object Constraints
  def OC(x: Id, o: Obj): List[Constraint] = o match {
    case (cls, fields) =>
      val init = InstantiatedBy(x, cls)
      val fieldCs = fields.map{case (f, v) => PathEquivalence(FieldPath(x, f), v)}

      init :: fieldCs
  }

  @tailrec
  def classInProgram(Cls: Id, program: Program): Option[(Id, List[Constraint])] = program match {
    case Nil => None
    case ConstructorDeclaration(Cls, x, a) :: _ => Some(x, a)
    case _ :: rst => classInProgram(Cls, rst)
  }

//  // Method Type
//  private def mType(m: Id, x: Id, y: Id): List[(List[Constraint], List[Constraint])] =
//    P.foldRight(Nil: List[(List[Constraint], List[Constraint])]){
//      case (AbstractMethodDeclaration(`m`, `x`, a, Type(`y`, b)), rst) => (a, b) :: rst
//      case (MethodImplementation(`m`, `x`, a, Type(`y`, b), _), rst) => (a, b) :: rst
//      case (_, rst) => rst}
//
//  // MType where the bound variables of declared argument and return type constraints are
//  // substituted with given variables
//  private def mTypeSubst(m: Id, x: Id, y: Id): List[(List[Constraint], List[Constraint])] =
//    P.foldRight(Nil: List[(List[Constraint], List[Constraint])]){
//      case (AbstractMethodDeclaration(`m`, xDecl, a, Type(yDecl, b)), rst) =>
//        (substitute(xDecl, x, a), substitute(yDecl, y, b)) :: rst
//      case (MethodImplementation(`m`, xImpl, a, Type(yImpl, b), _), rst) =>
//        (substitute(xImpl, x, a), substitute(yImpl, y, b)) :: rst
//      case (_, rst) => rst}
}
