package syntax

trait Constraint

//case class PathEquivalence(p: Path, q: Path) extends Constraint
//case class InstanceOf(p: Path, C: ClassName) extends Constraint
//case class InstantiatedBy(p: Path, C: ClassName)

case class PathEquivalence(p: Path, q: Path) extends Constraint
case class InstanceOf(p: Path, C: Id) extends Constraint
case class InstantiatedBy(p: Path, C: Id)