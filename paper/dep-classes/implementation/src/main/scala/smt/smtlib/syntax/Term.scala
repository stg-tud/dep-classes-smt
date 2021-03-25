package smt.smtlib.syntax

import smt.smtlib.SMTLibFormatter

trait Term extends SMTLibFormatter

trait QualifiedIdentifier extends Term

case class IdentifierAs(identifier: Identifier, sort: Sort) extends QualifiedIdentifier {
  override def format(): String = s"(as ${identifier.format()} ${sort.format()})"
}

case class Apply(id: QualifiedIdentifier, terms: Seq[Term]) extends Term {
  require(terms.nonEmpty, "argument terms must not be empty")

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

// Sugar for core
object Primitives {
  val True: Term = SimpleSymbol("true")
  val False: Term = SimpleSymbol("false")
}

//object True {
//  def apply(): Term = SimpleSymbol("true")
//}
//
//object False {
//  def apply(): Term = SimpleSymbol("false")
//}

object Not {
  def apply(term: Term): Term = Apply(SimpleSymbol("not"), Seq(term))
}

object Op1 {
  def apply(operation: QualifiedIdentifier, argument: Term): Term = Apply(operation, Seq(argument))
}

object Op2 {
  def apply(identifier: QualifiedIdentifier, left: Term, right: Term): Term = Apply(identifier, Seq(left, right))
}

object Op3 {
  def apply(identifier: QualifiedIdentifier, left: Term, middle: Term, right: Term): Term = Apply(identifier, Seq(left, middle, right))
}

object Implies {
  def apply(left: Term, right: Term): Term = Op2(SimpleSymbol("=>"), left, right)
}

object And {
  def apply(left: Term, right: Term): Term = Op2(SimpleSymbol("and"), left, right)
}

object Or {
  def apply(left: Term, right: Term): Term = Op2(SimpleSymbol("or"), left, right)
}

object Xor {
  def apply(left: Term, right: Term): Term = Op2(SimpleSymbol("xor"), left, right)
}

object Eq {
  def apply(left: Term, right: Term): Term = Op2(SimpleSymbol("="), left, right)
}

object Distinct {
  def apply(left: Term, right: Term): Term = Op2(SimpleSymbol("distinct"), left, right)
}

object Ite {
  def apply(i: Term, t: Term, e: Term): Term = Apply(SimpleSymbol("ite"), Seq(i, t, e))
}