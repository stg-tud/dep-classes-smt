package smtlib.syntax

import smtlib.{SMTLibCommand, SMTLibFormatter}

case class Assert(term: Term) extends SMTLibCommand {
  override def format(): String = s"(assert ${term.format()})"
}

case object CheckSat extends SMTLibCommand {
  override def format(): String = "(check-sat)"
}

case class CheckSatAssuming(propLiteral: PropLiteral) extends SMTLibCommand {
  override def format(): String = s"(check-sat-assuming (${propLiteral.format()}))"
}

// Aux
trait PropLiteral extends SMTLibFormatter

// TODO: possible clash with Not sugar object?
case class NotLiteral(symbol: SMTLibSymbol) extends PropLiteral {
  override def format(): String = s"(not ${symbol.format()})"
}