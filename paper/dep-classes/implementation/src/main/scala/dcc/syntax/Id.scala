package dcc.syntax

case class Id(name: Symbol) extends Path with Expression {
  override def toString: String = name.name
  override def baseName: String = toString
  override def prefixBaseName(prefix: String): Path = Id(Symbol(prefix + name.name))
  override def fieldNames: List[String] = Nil
  override def depth: Int = 0

  def +(right: Id): Id = Id(Symbol(name.name + right.name.name))
}