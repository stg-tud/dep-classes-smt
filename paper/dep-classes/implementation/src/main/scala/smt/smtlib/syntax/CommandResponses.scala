package smt.smtlib.syntax

import smt.smtlib.{SMTLibFormatter, SMTLibResponse}

sealed trait SpecificSuccessResponse extends GeneralResponse

sealed trait GeneralResponse extends SMTLibResponse

case object Success extends GeneralResponse {
  override def format: String = "success"
}

case object Unsupported extends GeneralResponse {
  override def format: String = "unsupported"
}

case class ErrorResponse(string: SMTLibString) extends GeneralResponse {
  override def format: String = s"(error ${string.format})"
}

sealed trait CheckSatResponse extends SpecificSuccessResponse

case object Sat extends CheckSatResponse {
  override def format: String = "sat"
}

case object Unsat extends CheckSatResponse {
  override def format: String = "unsat"
}

case object Unknown extends CheckSatResponse {
  override def format: String = "unknown"
}

trait EchoResponse extends SpecificSuccessResponse

case class GetAssertionResponse(terms: Seq[Term]) extends SpecificSuccessResponse {
  override def format: String = s"(${SMTLibFormatter.format(terms)})"
}

case class GetAssignmentResponse(tValuationPairs: Seq[TValuationPair]) extends SpecificSuccessResponse {
  override def format: String = s"(${SMTLibFormatter.format(tValuationPairs)})"
}

case class GetInfoResponse(responses: Seq[InfoResponse]) extends SpecificSuccessResponse {
  override def format: String = s"(${SMTLibFormatter.format(responses)})"
}

case class GetModelResponse(responses: Seq[ModelResponse]) extends SpecificSuccessResponse {
  override def format: String = s"(${SMTLibFormatter.format(responses)})"
}

trait GetOptionResponse extends SpecificSuccessResponse

trait GetProofResponse extends SpecificSuccessResponse

case class GetUnsatAssumptionsResponse(symbols: Seq[SMTLibSymbol]) extends SpecificSuccessResponse {
  override def format: String = s"(${SMTLibFormatter.format(symbols)})"
}

case class GetUnsatCoreResponse(symbols: Seq[SMTLibSymbol]) extends SpecificSuccessResponse {
  override def format: String = s"(${SMTLibFormatter.format(symbols)})"
}

case class GetValueResponse(valuationPairs: Seq[ValuationPair]) extends SpecificSuccessResponse {
  override def format: String = s"(${SMTLibFormatter.format(valuationPairs)})"
}

// Aux
trait ModelResponse extends SMTLibFormatter

case class ValuationPair(left: Term, right: Term) extends SMTLibFormatter {
  override def format: String = s"(${left.format} ${right.format})"
  override def pretty: String = s"(${left.pretty}, ${right.pretty})"
}

//case class TValuationPair(symbol: SMTLibSymbol, b: BValue) extends SMTLibFormatter {
//  override def format: String = s"(${symbol.format()} ${b.format()})"
//}
case class TValuationPair(symbol: SMTLibSymbol, b: Boolean) extends SMTLibFormatter {
  override def format: String = s"(${symbol.format} $b)"
  override def pretty: String = s"(${symbol.pretty}, $b)"
}