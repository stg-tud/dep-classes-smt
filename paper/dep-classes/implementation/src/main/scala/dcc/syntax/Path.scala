package dcc.syntax

trait Path {
  def baseName: String
  def fieldNames: List[String]
  def depth: Int
}

case class FieldPath(p: Path, f: Id) extends Path {
  override def toString: String = s"$p.$f"
  override def baseName: String = p.baseName
  override def fieldNames: List[String] = f.toString :: p.fieldNames
  override def depth: Int = 1+p.depth
}

// also: VariableName extends Path