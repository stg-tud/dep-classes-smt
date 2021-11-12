package smt.smtlib.syntax

import smt.smtlib.SMTLibFormatter

trait Term extends SMTLibFormatter

trait QualifiedIdentifier extends Term

case class IdentifierAs(identifier: Identifier, sort: Sort) extends QualifiedIdentifier {
  override def format: String = s"(as ${identifier.format} ${sort.format})"
  override def pretty: String = s"${identifier.pretty} as ${sort.pretty}"
}

case class Apply(id: QualifiedIdentifier, terms: Seq[Term]) extends Term {
  require(terms.nonEmpty, s"argument terms must not be empty (during calling ${id.pretty})")

  override def format: String = s"(${id.format} ${SMTLibFormatter.format(terms)})"
  override def pretty: String = id match {
    // Pretty printing predefined for functions from Bool, as Bool exists in every theory
    case SimpleSymbol("not") if terms.size==1 => s"¬${terms.head.pretty}"
    case SimpleSymbol("=>") if terms.size==2 => s"(${terms.head.pretty} → ${terms.last.pretty})"
    case SimpleSymbol("ite") if terms.size==3 => s"if(${terms.head.pretty}) ${terms.tail.head.pretty} else ${terms.last.pretty})"
    case SimpleSymbol("and") => s"(${SMTLibFormatter.pretty(terms, " ∧ ")})"
    case SimpleSymbol("or") => s"(${SMTLibFormatter.pretty(terms, " ∨ ")})"
    case SimpleSymbol("xor") => s"(${SMTLibFormatter.pretty(terms, " ⊕ ")})"
    case SimpleSymbol("=") => s"(${SMTLibFormatter.pretty(terms, " = ")})"
    case _ => s"${id.pretty}(${SMTLibFormatter.pretty(terms, ", ")})"
  }
}

case class VarBinding(symbol: SMTLibSymbol, term: Term) extends SMTLibFormatter {
  override def format: String = s"(${symbol.format} ${term.format})"
  override def pretty: String = s"${symbol.pretty} = ${term.pretty}"
}

case class Let(bindings: Seq[VarBinding], term: Term) extends Term {
  override def format: String = s"(let (${SMTLibFormatter.format(bindings)}) ${term.format})"
  override def pretty: String = s"let\n\t${SMTLibFormatter.pretty(bindings, "\n\t")}in\n\t${term.pretty}"
}

case class SortedVar(symbol: SMTLibSymbol, sort: Sort) extends SMTLibFormatter {
  override def format: String = s"(${symbol.format} ${sort.format})"
  override def pretty: String = s"${symbol.pretty}: ${sort.pretty}"
}

case class Forall(vars: Seq[SortedVar], term: Term) extends Term {
  override def format: String = s"(forall (${SMTLibFormatter.format(vars)}) ${term.format})"
  override def pretty: String = s"∀${SMTLibFormatter.pretty(vars, ", ")}. ${term.pretty}"
}

case class Exists(vars: Seq[SortedVar], term: Term) extends Term {
  override def format: String = s"(exists (${SMTLibFormatter.format(vars)}) ${term.format})"
  override def pretty: String = s"∃${SMTLibFormatter.pretty(vars, ", ")}. ${term.pretty}"
}

case class Pattern(symbol: SMTLibSymbol, symbols: Seq[SMTLibSymbol] = Nil) extends SMTLibFormatter {
  override def format: String = symbols match {
    case Nil => symbol.format
    case _ => s"(${symbol.format} ${SMTLibFormatter.format(symbols)})"
  }
  override def pretty: String = symbols match {
    case Nil => symbol.pretty
    case _   => s"${symbol.pretty}(${SMTLibFormatter.pretty(symbols, ", ")})"
  }
}
case class MatchCase(pattern: Pattern, term: Term) extends SMTLibFormatter {
  override def format: String = s"(${pattern.format} ${term.format})"
  override def pretty: String = s"\tcase ${pattern.pretty} => ${term.pretty}"
}

case class Match(term: Term, cases: Seq[MatchCase]) extends Term {
  override def format: String = s"(match ${term.format} (${SMTLibFormatter.format(cases)}))"
  override def pretty: String = s"${term.pretty} match {\n${SMTLibFormatter.pretty(cases, "\n")}\n}"
}

case class Annotate(term: Term, attributes: Seq[Attribute]) extends Term {
  override def format: String = s"(! ${term.format} ${SMTLibFormatter.format(attributes)})"
  override def pretty: String = s"@${SMTLibFormatter.pretty(attributes, ", ")}\n${term.pretty}"
}