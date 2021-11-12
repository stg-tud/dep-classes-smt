package dcc.syntax

trait Path {
  def base: Id
  def baseName: String
  def prefixBaseName(prefix: String): Path
  def fields: List[Id]
  def depth: Int
}

case class FieldPath(p: Path, f: Id) extends Path {
  override def toString: String = s"$p.$f"
  override def base: Id = p.base
  override def baseName: String = base.baseName
  override def prefixBaseName(prefix: String): Path = FieldPath(p.prefixBaseName(prefix), f)
  override def fields: List[Id] = f :: p.fields
  override def depth: Int = 1+p.depth
}

// also: VariableName extends Path