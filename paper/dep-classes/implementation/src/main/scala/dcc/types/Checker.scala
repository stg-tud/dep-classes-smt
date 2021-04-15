package dcc.types

import dcc.Util.substitute
import dcc.syntax.{AbstractMethodDeclaration, Constraint, ConstraintEntailment, ConstructorDeclaration, Declaration, Expression, Id, InstanceOf, MethodImplementation}
import dcc.syntax.Program.Program

import scala.language.postfixOps

trait Checker {
  val program: Program

  def typeOf(context: List[Constraint], expression: Expression): Either[Type, String]
  def typecheck(context: List[Constraint], expression: Expression, typ: Type): Boolean
  def typecheck(declaration: Declaration): Boolean
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
      case (AbstractMethodDeclaration(`m`, xDeclaration, a, Type(yDeclaration, b)), rst) =>
        (substitute(xDeclaration, x, a), substitute(yDeclaration, y, b)) :: rst
      case (MethodImplementation(`m`, xImpl, a, Type(yImpl, b), _), rst) =>
        (substitute(xImpl, x, a), substitute(yImpl, y, b)) :: rst
      case (_, rst) => rst}

  def classes: List[Id] = (program flatMap className) distinct

  // TODO: add search for instance of for constraint entailment second param?
  // TODO: search for class names in all constraints?
  private def className(d: Declaration): Option[Id] = d match {
    case ConstructorDeclaration(cls, _, _) => Some(cls)
    case ConstraintEntailment(_, _, InstanceOf(_, cls)) => Some(cls)
    case _ => None
  }

  def methods: List[Id] = program flatMap methodName distinct

  private def methodName(d: Declaration): Option[Id] = d match {
    case AbstractMethodDeclaration(m, _, _, _) => Some(m)
    case MethodImplementation(m, _, _, _, _) => Some(m)
    case _ => None
  }
}
