package syntax

trait Path

//case class FieldPath(p: Path, f: FieldName) extends Path
case class FieldPath(p: Path, f: Id) extends Path

// also: VariableName extends Path