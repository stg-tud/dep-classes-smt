package dcc.syntax

import smtlib.solver.Axioms
import smtlib.syntax.{Apply, Term}

object SMTLibConverter {
  def convertConstraint(c: Constraint): Term = c match {
    case PathEquivalence(p, q) => Axioms.pathEq(convertPath(p), convertPath(q))
    case InstanceOf(p, cls) => Axioms.instanceOf(convertPath(p), cls.toString)
    case InstantiatedBy(p, cls) => Axioms.instantiatedBy(convertPath(p), cls.toString)
  }

  def convertPath(p: Path): Term = Axioms.path(p.toString)
}
