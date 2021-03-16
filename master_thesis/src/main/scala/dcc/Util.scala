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

  def substitute(x: Id, p: Path, q: Path): Path = q match {
    case `x` => p
    case y@Id(_) => y
    case FieldPath(q1, f) => FieldPath(substitute(x, p, q1), f)
  }

  def substitute(x: Id, path: Path, c: Constraint): Constraint = c match {
    case PathEquivalence(p, q) => PathEquivalence(substitute(x, path, p), substitute(x, path, q))
    case InstanceOf(p, cls) => InstanceOf(substitute(x, path, p), cls)
    case InstantiatedBy(p, cls) => InstantiatedBy(substitute(x, path, p), cls)
  }

  def substitute(x: Id, p: Path, cs: List[Constraint]): List[Constraint] =
    cs.map(substitute(x, p, _))

  def alphaRename(x: Id, y: Id, expr: Expression): Expression = expr match {
    case `x` => y
    case z@Id(_) => z
    case FieldAccess(e, f) => FieldAccess(alphaRename(x, y, e), f)
    case ObjectConstruction(cls, args) => ObjectConstruction(cls, args.map{
      case (z, e) => (z, alphaRename(x, y, e))
    })
    case MethodCall(m, e) => MethodCall(m, alphaRename(x, y, e))
  }
}