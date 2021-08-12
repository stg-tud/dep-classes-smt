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

object Program {
  type Program = List[Declaration]

  def DefinedFieldNames(p: Program): List[String] = p flatMap {
    case ConstructorDeclaration(_, _, as) => extractFieldNames(as)
    case _ => Nil
  }

  private def extractFieldNames(constraints: List[Constraint]): List[String] = constraints flatMap {
    case PathEquivalence(p, q) => p.fieldNames ++ q.fieldNames
    case InstanceOf(p, _) => p.fieldNames
    case InstantiatedBy(p, _) => p.fieldNames
  } distinct

  def DefinedClassNames(p: Program): List[String] = p flatMap {
    case ConstructorDeclaration(cls, _, _) => cls.name.name :: Nil
    case ConstraintEntailment(_, _, InstanceOf(_, cls)) => cls.name.name :: Nil
    case _ => Nil
  } distinct
}