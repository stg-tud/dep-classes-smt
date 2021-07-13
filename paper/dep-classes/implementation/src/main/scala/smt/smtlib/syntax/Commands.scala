package smt.smtlib.syntax

import smt.smtlib.SMTLibCommand

// Commands not in another category

case object Exit extends SMTLibCommand {
  override def format: String = s"(exit)"
  override def pretty: String = s"exit"
}

case class Pop(numeral: Numeral) extends SMTLibCommand {
  override def format: String = s"(pop ${numeral.format})"
  override def pretty: String = s"pop ${numeral.pretty}"
}

case class Push(numeral: Numeral) extends SMTLibCommand {
  override def format: String = s"(push ${numeral.format})"
  override def pretty: String = s"push ${numeral.pretty}"
}

case object Reset extends SMTLibCommand {
  override def format: String = "(reset)"
  override def pretty: String = "reset"
}

case object ResetAssertions extends SMTLibCommand {
  override def format: String = s"(reset-assertions)"
  override def pretty: String = "reset assertions"
}

case class SetInfo(attribute: Attribute) extends SMTLibCommand {
  override def format: String = s"(set-info ${attribute.format})"
  override def pretty: String = s"set info ${attribute.pretty}"
}

case class SetLogic(symbol: SMTLibSymbol) extends SMTLibCommand {
  override def format: String = s"(set-logic ${symbol.format})"
  override def pretty: String = s"set logic ${symbol.pretty}"
}

case class SetOption(option: Option) extends SMTLibCommand {
  override def format: String = s"(set-option ${option.format})"
  override def pretty: String = s"set option ${option.pretty}"
}