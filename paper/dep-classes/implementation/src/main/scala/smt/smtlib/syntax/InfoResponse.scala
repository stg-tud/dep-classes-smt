package smt.smtlib.syntax

import smt.smtlib.SMTLibFormatter

trait InfoResponse extends SMTLibFormatter {
  override def pretty: String = format
}

case class AssertionStackLevelsResponse(numeral: Numeral) extends InfoResponse {
  override def format: String = s":assertion-stack-levels ${numeral.format}"
}

case class AuthorsResponse(string: SMTLibString) extends InfoResponse {
  override def format: String = s":authors ${string.format}"
}

case class ErrorBehaviorResponse(behavior: ErrorBehavior) extends InfoResponse {
  override def format: String = s":error-behavior ${behavior.format}"
}

case class NameResponse(string: SMTLibString) extends InfoResponse {
  override def format: String = s":name ${string.format}"
}

case class ReasonUnknownResponse(reason: ReasonUnknown) extends InfoResponse {
  override def format: String = s":reason-unknown ${reason.format}"
}

case class VersionResponse(string: SMTLibString) extends InfoResponse {
  override def format: String = s":version ${string.format}"
}

// Aux
sealed trait ErrorBehavior extends SMTLibFormatter {
  override def pretty: String = format
}

case object ImmediateExit extends ErrorBehavior {
  override def format: String = "immediate-exit"
}

case object ContinuedExecution extends ErrorBehavior {
  override def format: String = "continued-execution"
}

trait ReasonUnknown extends SMTLibFormatter

case object Memout extends ReasonUnknown {
  override def format: String = "memout"
  override def pretty: String = format
}

case object Incomplete extends ReasonUnknown {
  override def format: String = "incomplete"
  override def pretty: String = format
}