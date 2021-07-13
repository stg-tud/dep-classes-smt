package smt.smtlib.syntax

import smt.smtlib.{SMTLibCommand, SMTLibFormatter}

case class Assert(term: Term) extends SMTLibCommand {
  override def format: String = s"(assert ${term.format})"
  override def pretty: String = term.pretty
}

case object CheckSat extends SMTLibCommand {
  override def format: String = "(check-sat)"
  override def pretty: String = "check-sat"
}

case class CheckSatAssuming(propLiteral: PropLiteral) extends SMTLibCommand {
  override def format: String = s"(check-sat-assuming (${propLiteral.format}))"
  override def pretty: String = s"assume ${propLiteral.pretty}"
}

// Aux
trait PropLiteral extends SMTLibFormatter

case class NotLiteral(symbol: SMTLibSymbol) extends PropLiteral {
  override def format: String = s"(not ${symbol.format})"
  override def pretty: String = s"¬${symbol.pretty}"
}