package dcc.syntax

trait Path {
  def baseName: String
  def prefixBaseName(prefix: String): Path
  def fieldNames: List[String]
  def depth: Int
}

case class FieldPath(p: Path, f: Id) extends Path {
  override def toString: String = s"$p.$f"
  override def baseName: String = p.baseName
  override def prefixBaseName(prefix: String): Path = FieldPath(p.prefixBaseName(prefix), f)
  override def fieldNames: List[String] = f.toString :: p.fieldNames
  override def depth: Int = 1+p.depth
}

// also: VariableName extends Path