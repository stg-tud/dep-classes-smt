package dcc.types

import dcc.Util.substitute
import dcc.syntax.{AbstractMethodDeclaration, Constraint, Declaration, Expression, Id, MethodImplementation}
import dcc.syntax.Program.Program

trait Checker {
  val program: Program

  def typeOf(context: List[Constraint], expression: Expression): Type
  def typecheck(context: List[Constraint], expression: Expression, typ: Type): Boolean
  def typecheck(declaration: Declaration): Boolean
  // TODO: rename 'typecheckProgram'?
  def typecheck: Boolean

  // Method Type
  def mType(m: Id, x: Id, y: Id): List[(List[Constraint], List[Constraint])] =
    program.foldRight(Nil: List[(List[Constraint], List[Constraint])]){
      case (AbstractMethodDeclaration(`m`, `x`, a, Type(`y`, b)), rst) => (a, b) :: rst
      case (MethodImplementation(`m`, `x`, a, Type(`y`, b), _), rst) => (a, b) :: rst
      case (_, rst) => rst}

  // MType where the bound variables of declared argument and return type constraints are
  // substituted with given variables
  def mTypeSubst(m: Id, x: Id, y: Id): List[(List[Constraint], List[Constraint])] =
    program.foldRight(Nil: List[(List[Constraint], List[Constraint])]){
      case (AbstractMethodDeclaration(`m`, xDecl, a, Type(yDecl, b)), rst) =>
        (substitute(xDecl, x, a), substitute(yDecl, y, b)) :: rst
      case (MethodImplementation(`m`, xImpl, a, Type(yImpl, b), _), rst) =>
        (substitute(xImpl, x, a), substitute(yImpl, y, b)) :: rst
      case (_, rst) => rst}

  def methods: List[Id] = program.flatMap(methodName).distinct

  private def methodName(d: Declaration): Option[Id] = d match {
    case AbstractMethodDeclaration(m, _, _, _) => Some(m)
    case MethodImplementation(m, _, _, _, _) => Some(m)
    case _ => None
  }
}
