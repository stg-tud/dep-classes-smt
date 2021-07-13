package smt.smtlib.syntax

import smt.smtlib.SMTLibCommand

case object GetAssertions extends SMTLibCommand {
  override def format: String = "(get-assertions)"
  override def pretty: String = "get assertions"
}

case object GetAssignments extends SMTLibCommand {
  override def format: String = "(get-assignments)"
  override def pretty: String = "get assignments"
}

case class GetInfo(infoFlag: InfoFlag) extends SMTLibCommand {
  override def format: String = s"(get-info ${infoFlag.format})"
  override def pretty: String = s"get info ${infoFlag.pretty}"
}

case object GetModel extends SMTLibCommand {
  override def format: String = "(get-model)"
  override def pretty: String = "get model"
}

case class GetOption(keyword: Keyword) extends SMTLibCommand {
  override def format: String = s"(get-option ${keyword.format})"
  override def pretty: String = s"get option ${keyword.pretty}"
}

case object GetProof extends SMTLibCommand {
  override def format: String = "(get-proof)"
  override def pretty: String = "get proof"
}

case object GetUnsatAssumptions extends SMTLibCommand {
  override def format: String = "(get-unsat-assumptions)"
  override def pretty: String = "get unsat assumptions"
}

case object GetUnsatCore extends SMTLibCommand {
  override def format: String = "(get-unsat-core)"
  override def pretty: String = "get unsat core"
}

case class GetValue(term: Term) extends SMTLibCommand {
  override def format: String = s"(get-term ${term.format})"
  override def pretty: String = s"get term ${term.pretty}"
}