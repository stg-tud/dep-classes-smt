package dcc.syntax

sealed trait Constraint {
  def maxPathDepth: Int
  def containedPaths: List[Path]
}

case class PathEquivalence(p: Path, q: Path) extends Constraint {
  def isReflexive: Boolean = p == q

  override def maxPathDepth: Int = p.depth max q.depth
  override def containedPaths: List[Path] = List(p, q)
  override def toString: String = s"$p ≡ $q"
}
case class InstanceOf(p: Path, cls: Id) extends Constraint {
  override def maxPathDepth: Int = p.depth
  override def containedPaths: List[Path] = List(p)
  override def toString: String = s"$p :: $cls"
}
case class InstantiatedBy(p: Path, cls: Id) extends Constraint {
  override def maxPathDepth: Int = p.depth
  override def containedPaths: List[Path] = List(p)
  override def toString: String = s"$p.cls ≡ $cls"
}