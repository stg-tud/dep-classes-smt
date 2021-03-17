package smt.smtlib.syntax

import smt.smtlib.SMTLibFormatter

trait InfoResponse extends SMTLibFormatter

case class AssertionStackLevelsResponse(numeral: Numeral) extends InfoResponse {
  override def format(): String = s":assertion-stack-levels ${numeral.format()}"
}

case class AuthorsResponse(string: SMTLibString) extends InfoResponse {
  override def format(): String = s":authors ${string.format()}"
}

case class ErrorBehaviorResponse(behavior: ErrorBehavior) extends InfoResponse {
  override def format(): String = s":error-behavior ${behavior.format()}"
}

case class NameResponse(string: SMTLibString) extends InfoResponse {
  override def format(): String = s":name ${string.format()}"
}

case class ReasonUnknownResponse(reason: ReasonUnknown) extends InfoResponse {
  override def format(): String = s":reason-unknown ${reason.format()}"
}

case class VersionResponse(string: SMTLibString) extends InfoResponse {
  override def format(): String = s":version ${string.format()}"
}

// Aux
trait ErrorBehavior extends SMTLibFormatter

case object ImmediateExit extends ErrorBehavior {
  override def format(): String = "immediate-exit"
}

case object ContiniuedExecution extends ErrorBehavior {
  override def format(): String = "continued-execution"
}

trait ReasonUnknown extends SMTLibFormatter

case object Memout extends ReasonUnknown {
  override def format(): String = "memout"
}

case object Incomplete extends ReasonUnknown {
  override def format(): String = "incomplete"
}