package dcc.entailment.smt

import DCCStrings.{FunctionInstanceOf, FunctionInstantiatedBy, FunctionPathEquivalence, FunctionSubstitution}
import com.github.gnush.smt.smtlib.syntax.{Apply, Term}

object DCCSugar {
  case class PathEq(lhs: Term, rhs: Term) extends Term {
    def apply(): Term = Apply(FunctionPathEquivalence, Seq(lhs, rhs))

    override def format: String = this().format
    override def pretty: String = s"${lhs.pretty} ≡ ${rhs.pretty}"
  }

  case class InstOf(path: Term, cls: Term) extends Term {
    def apply(): Term = Apply(FunctionInstanceOf, Seq(path, cls))

    override def format: String = this().format
    override def pretty: String = s"${path.pretty} :: ${cls.pretty}"
  }

  case class InstBy(path: Term, cls: Term) extends Term {
    def apply(): Term = Apply(FunctionInstantiatedBy, Seq(path, cls))

    override def format: String = this().format
    override def pretty: String = s"${path.pretty}.cls = ${cls.pretty}"
  }

  case class Subst(source: Term, target: Term, replace: Term) extends Term {
    def apply(): Term = Apply(FunctionSubstitution, Seq(source, target, replace))

    override def format: String = this().format
    override def pretty: String = s"${source.pretty}{{${target.pretty} ↦ ${replace.pretty}}}"
  }

  case class SubstEq(source: Term, target: Term, replace: Term, result: Term) extends Term {
    def apply(): Term = Apply(FunctionSubstitution, Seq(source, target, replace, result))

    override def format: String = this().format
    override def pretty: String = s"${source.pretty}{{${target.pretty} ↦ ${replace.pretty}}} = ${result.pretty}"
  }
}