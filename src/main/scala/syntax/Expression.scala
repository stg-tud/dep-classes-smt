package syntax

trait Expression

//case class FieldAccess(e: Expression, f: FieldName) extends Expression
//case class ObjectConstruction(C: ClassName, args: List[(FieldName, Expression)]) extends Expression // use two lists or use tuple?
//case class MethodCall(m: MethodName, e: Expression) extends Expression

case class FieldAccess(e: Expression, f: Id) extends Expression {
  override def toString: String = s"$e.$f"
}

case class ObjectConstruction(C: Id, args: List[(Id, Expression)]) extends Expression { // use two lists or use tuple?
  override def toString: String =
    s"new $C(${args.foldRight("")((x, rst) => s"${x._1} ≡ ${x._2}, ${rst}").dropRight(2)})"
}

case class MethodCall(m: Id, e: Expression) extends Expression {
  override def toString: String = s"$m($e)"
}

// also: VariableName extends Path for var access