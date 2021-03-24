package dcc.syntax

sealed trait Constraint

case class PathEquivalence(p: Path, q: Path) extends Constraint {
  override def toString: String = s"$p ≡ $q"
}
case class InstanceOf(p: Path, cls: Id) extends Constraint {
  override def toString: String = s"$p :: $cls"
}
case class InstantiatedBy(p: Path, cls: Id) extends Constraint {
  override def toString: String = s"$p.cls ≡ $cls"
}