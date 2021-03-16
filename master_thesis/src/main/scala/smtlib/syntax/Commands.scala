package smtlib.syntax

import smtlib.SMTLibCommand

// Commands not in another category

case object Exit extends SMTLibCommand {
  override def format(): String = s"(exit)"
}

case class Pop(numeral: Numeral) extends SMTLibCommand {
  override def format(): String = s"(pop ${numeral.format()})"
}

case class Push(numeral: Numeral) extends SMTLibCommand {
  override def format(): String = s"(push ${numeral.format()})"
}

case object Reset extends SMTLibCommand {
  override def format(): String = s"(reset)"
}

case object ResetAssertions extends SMTLibCommand {
  override def format(): String = s"(reset-assertions)"
}

case class SetInfo(attribute: Attribute) extends SMTLibCommand {
  override def format(): String = s"(set-info ${attribute.format()})"
}

case class SetLogic(symbol: SMTLibSymbol) extends SMTLibCommand {
  override def format(): String = s"(set-logic ${symbol.format()})"
}

case class SetOption(option: Option) extends SMTLibCommand {
  override def format(): String = s"(set-option ${option.format()})"
}