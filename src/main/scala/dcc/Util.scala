package dcc

import dcc.syntax._

object Util {
  def renameIdInPath(x: Id, y: Id, p: Path): Path = p match {
    case `x` => y
    case z@Id(_) => z
    case FieldPath(q, f) => FieldPath(renameIdInPath(x, y, q), f)
  }

  def renameIdInConstraint(x: Id, y: Id, c: Constraint): Constraint = c match {
    case PathEquivalence(p, q) => PathEquivalence(renameIdInPath(x, y, p), renameIdInPath(x, y, q))
    case InstanceOf(p, cls) => InstanceOf(renameIdInPath(x, y, p), cls)
    case InstantiatedBy(p, cls) => InstantiatedBy(renameIdInPath(x, y, p), cls)
  }

  def alphaConversion(x: Id, y: Id, cs: List[Constraint]): List[Constraint] =
    cs.map(renameIdInConstraint(x, y, _))
}
