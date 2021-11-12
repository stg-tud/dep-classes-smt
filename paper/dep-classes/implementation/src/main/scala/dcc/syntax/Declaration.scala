package dcc.syntax

import Util._
import dcc.types.Type

sealed trait Declaration

case class ConstructorDeclaration(cls: Id, x: Id, as: List[Constraint]) extends Declaration {
  override def toString: String = as match {
    case Nil => s"$cls($x. ϵ)"
    case _ => s"$cls($x. ${commaSeparate(as.map(_.toString))})"
  }
}

case class MethodImplementation(m: Id, x: Id, as: List[Constraint], t: Type, e: Expression) extends Declaration {
  override def toString: String =
    s"$m($x. ${commaSeparate(as)}): $t := $e"
}

case class AbstractMethodDeclaration(m: Id, x: Id, as: List[Constraint], t: Type) extends Declaration {
  override def toString: String =
    s"$m($x. ${commaSeparate(as)}): $t"
}

case class ConstraintEntailment(x: Id, as: List[Constraint], a: Constraint) extends Declaration {
  override def toString: String =
    s"∀$x. ${commaSeparate(as.map(_.toString))} => $a"
}

import scala.language.postfixOps

// TODO: change program to be a trait?
object Program {
  type Program = List[Declaration]

  // All valid field names must be introduced via constructors.
  def DefinedFields(p: Program): List[Id] = p flatMap {
    case ConstructorDeclaration(_, _, as) => extractFieldNames(as)
    case _ => Nil
  }

  private def extractFieldNames(constraints: List[Constraint]): List[Id] = constraints flatMap {
    case PathEquivalence(p, q) => p.fields ++ q.fields
    case InstanceOf(p, _) => p.fields
    case InstantiatedBy(p, _) => p.fields
  } distinct

  // A valid class name is either introduces through a constructor or as the conclusion of a constraint entailment
  def DefinedClasses(p: Program): List[Id] = p flatMap {
    case ConstructorDeclaration(cls, _, _) => cls :: Nil
    case ConstraintEntailment(_, _, InstanceOf(_, cls)) => cls :: Nil
    case _ => Nil
  } distinct

  def GetMatchingConstraintEntailments(program: Program, cls: Id): List[ConstraintEntailment] = program match {
    case Nil => Nil
    case ConstraintEntailment(x, ctx, InstanceOf(y, cls1)) :: rest if x==y && cls1==cls => ConstraintEntailment(x, ctx, InstanceOf(x, cls)) :: GetMatchingConstraintEntailments(rest, cls)
    case _ :: rest => GetMatchingConstraintEntailments(rest, cls)
  }
}