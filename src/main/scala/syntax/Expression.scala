package syntax

trait Expression

//case class FieldAccess(e: Expression, f: FieldName) extends Expression
//case class ObjectConstruction(C: ClassName, args: List[(FieldName, Expression)]) extends Expression // use two lists or use tuple?
//case class MethodCall(m: MethodName, e: Expression) extends Expression

case class FieldAccess(e: Expression, f: Id) extends Expression
case class ObjectConstruction(C: Id, args: List[(Id, Expression)]) extends Expression // use two lists or use tuple?
case class MethodCall(m: Id, e: Expression) extends Expression

// also: VariableName extends Path for var access