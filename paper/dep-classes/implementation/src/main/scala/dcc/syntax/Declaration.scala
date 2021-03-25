package dcc.syntax

import Util._

trait Declaration

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

object Program {
  type Program = List[Declaration]
}