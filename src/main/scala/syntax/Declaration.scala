package syntax

import Type._

trait Declaration

//case class ConstructorDeclaration(C: ClassName, x: VariableName, constraints: List[Constraint]) extends Declaration
//case class MethodImplementation(m: MethodName, x: VariableName, cs: List[Constraint], t: Type, e: Expression) extends Declaration
//case class AbstractMethodDeclaration(m: MethodName, x: VariableName, cs: List[Constraint], t: Type) extends Declaration
//case class ConstraintEntailment(x: VariableName, cs: List[Constraint], c: Constraint) extends Declaration

case class ConstructorDeclaration(C: Id, x: Id, constraints: List[Constraint]) extends Declaration
case class MethodImplementation(m: Id, x: Id, constraints: List[Constraint], t: Type, e: Expression) extends Declaration
case class AbstractMethodDeclaration(m: Id, x: Id, constraints: List[Constraint], t: Type) extends Declaration
case class ConstraintEntailment(x: Id, constraints: List[Constraint], constraint: Constraint) extends Declaration

object Program {
  type Program = List[Declaration]
}