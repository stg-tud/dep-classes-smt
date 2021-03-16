package dcc.syntax

case class Id(name: Symbol) extends Path with Expression {
  override def toString: String = name.name
}