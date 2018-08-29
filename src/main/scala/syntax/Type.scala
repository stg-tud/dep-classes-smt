package syntax

//case class Type(x: VariableName, constraints: List[Constraint])
object Type {
//  type Type = (VariableName, List[Constraint])
  type Type = (Id, List[Constraint])
}