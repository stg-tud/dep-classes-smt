package dcc.syntax

import smtlib.solver.Axioms
import smtlib.syntax.Term

trait Constraint

//case class PathEquivalence(p: Path, q: Path) extends Constraint
//case class InstanceOf(p: Path, C: ClassName) extends Constraint
//case class InstantiatedBy(p: Path, C: ClassName)

case class PathEquivalence(p: Path, q: Path) extends Constraint {
  override def toString: String = s"$p ≡ $q"
}
case class InstanceOf(p: Path, C: Id) extends Constraint {
  override def toString: String = s"$p :: $C"
}
case class InstantiatedBy(p: Path, C: Id) extends Constraint {
  override def toString: String = s"$p.cls ≡ $C"
}