package dcc.syntax

trait Path {
  def baseName: String
  def fieldNames: List[String]
}

case class FieldPath(p: Path, f: Id) extends Path {
  override def toString: String = s"$p.$f"
  override def baseName: String = p.baseName
  override def fieldNames: List[String] = f.toString :: p.fieldNames
}

// also: VariableName extends Path