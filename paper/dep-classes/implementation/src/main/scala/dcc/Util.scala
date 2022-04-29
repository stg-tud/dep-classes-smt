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

  // TODO: change argument order? currently weird to use
  def substitute(x: Id, replace: Path, source: Path): Path = source match {
    case `x` => replace
    case Id(_) => source
    case FieldPath(p, f) => FieldPath(substitute(x, replace, p), f)
  }

  def substitute(x: Id, path: Path, c: Constraint): Constraint = c match {
    case PathEquivalence(p, q) => PathEquivalence(substitute(x, path, p), substitute(x, path, q))
    case InstanceOf(p, cls) => InstanceOf(substitute(x, path, p), cls)
    case InstantiatedBy(p, cls) => InstantiatedBy(substitute(x, path, p), cls)
  }

  def substitute(x: Id, p: Path, cs: List[Constraint]): List[Constraint] =
    cs.map(substitute(x, p, _))

  // TODO: fuse with substitute (change all constraints to be Sets instead of Lists?)
  def substituteSet(x: Id, p: Path, cs: Set[Constraint]): Set[Constraint] =
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

  def prefixedSubstitute(prefix: String)(source: Path, target: Id, replace: Path): Path = substitute(Id(Symbol(prefix))+target, replace, source)
}