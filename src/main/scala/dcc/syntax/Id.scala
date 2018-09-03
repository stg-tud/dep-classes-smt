package dcc.syntax

//trait Identifier
//
//case class VariableName(x: Symbol) extends Identifier with Path with Expression
//case class FieldName(f: Symbol) extends Identifier
//case class ClassName(C: Symbol) extends Identifier
//case class MethodName(m: Symbol) extends Identifier

case class Id(name: Symbol) extends Path with Expression {
  override def toString: String = name.toString().drop(1)
}