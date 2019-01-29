package dcc.syntax

import Util._

trait Declaration

//case class ConstructorDeclaration(C: ClassName, x: VariableName, constraints: List[Constraint]) extends Declaration
//case class MethodImplementation(m: MethodName, x: VariableName, cs: List[Constraint], t: Type, e: Expression) extends Declaration
//case class AbstractMethodDeclaration(m: MethodName, x: VariableName, cs: List[Constraint], t: Type) extends Declaration
//case class ConstraintEntailment(x: VariableName, cs: List[Constraint], c: Constraint) extends Declaration

case class ConstructorDeclaration(C: Id, x: Id, as: List[Constraint]) extends Declaration {
  override def toString: String = as match {
    case Nil => s"$C($x. ϵ)"
    case _ => s"$C($x. ${commaSeparate(as.map(_.toString))})"
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
    s"∀${x}. ${commaSeparate(as.map(_.toString))} => ${a}"
}

object Program {
  type Program = List[Declaration]
}