package smtlib.syntax

import smtlib.{SMTLibCommand, SMTLibFormatter}

trait Term extends SMTLibCommand

case object True extends Term {
  override def format(): String = "true"
}

case object False extends Term {
  override def format(): String = "false"
}

// spec-constant
case class BaseTerm(s: String) extends Term {
  override def format(): String = s
}

case class QualifiedIdentifier(id: String, sort: Option[Sort] = None) extends Term {
  override def format(): String = sort match {
    case None => id
    case Some(s) => s"(as $id ${s.format()})"
  }
}

case class Apply(id: QualifiedIdentifier, terms: Seq[Term]) extends Term {
  require(terms.nonEmpty)

  override def format(): String = s"(${id.format()} ${SMTLibFormatter.format(terms)})"
}

case class VarBinding(symbol: String, term: Term) extends SMTLibFormatter {
  override def format(): String = s"($symbol ${term.format()})"
}

case class Let(bindings: Seq[VarBinding], term: Term) extends Term {
  override def format(): String = s"(let (${SMTLibFormatter.format(bindings)}) ${term.format()})"
}

case class SortedVar(symbol: String, sort: Sort) extends SMTLibFormatter {
  override def format(): String = s"($symbol ${sort.format()})"
}

case class Forall(vars: Seq[SortedVar], term: Term) extends Term {
  override def format(): String = s"(forall (${SMTLibFormatter.format(vars)}) ${term.format()})"
}

case class Exists(vars: Seq[SortedVar], term: Term) extends Term {
  override def format(): String = s"(exists (${SMTLibFormatter.format(vars)}) ${term.format()})"
}

case class Pattern(symbol: String, symbols: Seq[String] = Nil) extends SMTLibFormatter {
  override def format(): String = symbols match {
    case Nil => symbol
    case _ => s"($symbol ${symbols.foldRight(""){(x, xs) => s"$x $xs"}.dropRight(1)})"
  }
}
case class MatchCase(pattern: Pattern, term: Term) extends SMTLibFormatter {
  override def format(): String = s"(${pattern.format()} ${term.format()})"
}

case class Match(term: Term, cases: Seq[MatchCase]) extends Term {
  override def format(): String = s"(match ${term.format()} (${SMTLibFormatter.format(cases)}))"
}

case class Annotate(term: Term, attributes: Seq[String]) extends Term {
  override def format(): String = s"(! ${term.format()} ${attributes.foldRight(""){(x, xs) => s"$x $xs"}.dropRight(1)})"
}