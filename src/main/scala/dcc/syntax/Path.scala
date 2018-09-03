package dcc.syntax

trait Path

//case class FieldPath(p: Path, f: FieldName) extends Path
case class FieldPath(p: Path, f: Id) extends Path {
  override def toString: String = s"$p.$f"
}

// also: VariableName extends Path