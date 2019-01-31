package dcc.syntax

trait Expression

case class FieldAccess(e: Expression, f: Id) extends Expression {
  override def toString: String = s"$e.$f"
}

case class ObjectConstruction(C: Id, args: List[(Id, Expression)]) extends Expression { // use two lists or use tuple?
  override def toString: String = {
    s"new $C(${args.foldRight("") { case ((x, e), rst) => s"$x ≡ $e, $rst" }.dropRight(2)})"
  }
}

case class MethodCall(m: Id, e: Expression) extends Expression {
  override def toString: String = s"$m($e)"
}

// also: VariableName extends Path for var access