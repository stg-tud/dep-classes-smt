package smt.smtlib.syntax

import smt.smtlib.SMTLibCommand

case object GetAssertions extends SMTLibCommand {
  override def format: String = "(get-assertions)"
}

case object GetAssignments extends SMTLibCommand {
  override def format: String = "(get-assignments)"
}

case class GetInfo(infoFlag: InfoFlag) extends SMTLibCommand {
  override def format: String = s"(get-info ${infoFlag.format})"
}

case object GetModel extends SMTLibCommand {
  override def format: String = "(get-model)"
}

case class GetOption(keyword: Keyword) extends SMTLibCommand {
  override def format: String = s"(get-option ${keyword.format})"
}

case object GetProof extends SMTLibCommand {
  override def format: String = "(get-proof)"
}

case object GetUnsatAssumptions extends SMTLibCommand {
  override def format: String = "(get-unsat-assumptions)"
}

case object GetUnsatCore extends SMTLibCommand {
  override def format: String = "(get-unsat-core)"
}

case class GetValue(term: Term) extends SMTLibCommand {
  override def format: String = s"(get-term ${term.format})"
}