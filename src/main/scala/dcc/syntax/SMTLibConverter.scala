package dcc.syntax

import dcc.syntax.Program.Program
import smtlib.solver.Axioms
import smtlib.syntax.{Apply, SMTLibString, SimpleSymbol, Term}

object SMTLibConverter {
  def convertConstraint(c: Constraint): Term = c match {
    case PathEquivalence(p, q) => Axioms.pathEq(convertPath(p), convertPath(q))
    case InstanceOf(p, cls) => Axioms.instanceOf(convertPath(p), cls.toString)
    case InstantiatedBy(p, cls) => Axioms.instantiatedBy(convertPath(p), cls.toString)
  }

  def convertPath(p: Path): Term = Axioms.path(p.toString)

  def convertProgramEntailments(p: Program): List[Term] = p match {
    case Nil => Nil
    case ConstraintEntailment(x, as, a) :: rst =>
      val ctx: Seq[Term] = as.map(convertConstraint)
      val c: Term = convertConstraint(a)

      val entailment = Apply(SimpleSymbol("in-program"), Seq(SMTLibString(x.toString), makeList(ctx), c))

      entailment :: convertProgramEntailments(rst)
    case _ :: rst => convertProgramEntailments(rst)
  }

  def convertEntailment(ctx: List[Constraint], c: Constraint): Term = {
    val ctxSMTLib: Term = SMTLibConverter.makeList(ctx.map(SMTLibConverter.convertConstraint))
    val cSMTLib: Term = SMTLibConverter.convertConstraint(c)

    Apply(SimpleSymbol("entails"), Seq(ctxSMTLib, cSMTLib))
  }

  private def makeList(terms: Seq[Term]): Term = terms match {
    case Nil => SimpleSymbol("nil")
    case t :: rst => Apply(SimpleSymbol("insert"), Seq(t, makeList(rst)))
  }
}
