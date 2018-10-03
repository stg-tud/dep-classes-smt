package smtlib.syntax

import smtlib.{SMTLibCommand, SMTLibFormatter}

trait Term extends SMTLibCommand

trait QualifiedIdentifier extends Term

case class IdentifierAs(identifier: Identifier, sort: Sort) extends QualifiedIdentifier {
  override def format(): String = s"(as ${identifier.format()} ${sort.format()})"
}

case class Apply(id: QualifiedIdentifier, terms: Seq[Term]) extends Term {
  require(terms.nonEmpty)

  override def format(): String = s"(${id.format()} ${SMTLibFormatter.format(terms)})"
}

case class VarBinding(symbol: SMTLibSymbol, term: Term) extends SMTLibFormatter {
  override def format(): String = s"(${symbol.format()} ${term.format()})"
}

case class Let(bindings: Seq[VarBinding], term: Term) extends Term {
  override def format(): String = s"(let (${SMTLibFormatter.format(bindings)}) ${term.format()})"
}

case class SortedVar(symbol: SMTLibSymbol, sort: Sort) extends SMTLibFormatter {
  override def format(): String = s"(${symbol.format()} ${sort.format()})"
}

case class Forall(vars: Seq[SortedVar], term: Term) extends Term {
  override def format(): String = s"(forall (${SMTLibFormatter.format(vars)}) ${term.format()})"
}

case class Exists(vars: Seq[SortedVar], term: Term) extends Term {
  override def format(): String = s"(exists (${SMTLibFormatter.format(vars)}) ${term.format()})"
}

case class Pattern(symbol: SMTLibSymbol, symbols: Seq[SMTLibSymbol] = Nil) extends SMTLibFormatter {
  override def format(): String = symbols match {
    case Nil => symbol.format()
    case _ => s"(${symbol.format()} ${SMTLibFormatter.format(symbols)})"
  }
}
case class MatchCase(pattern: Pattern, term: Term) extends SMTLibFormatter {
  override def format(): String = s"(${pattern.format()} ${term.format()})"
}

case class Match(term: Term, cases: Seq[MatchCase]) extends Term {
  override def format(): String = s"(match ${term.format()} (${SMTLibFormatter.format(cases)}))"
}

case class Annotate(term: Term, attributes: Seq[Attribute]) extends Term {
  override def format(): String = s"(! ${term.format()} ${SMTLibFormatter.format(attributes)})"
}