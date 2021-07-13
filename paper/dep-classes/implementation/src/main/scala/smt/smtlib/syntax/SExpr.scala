package smt.smtlib.syntax

import smt.smtlib.SMTLibFormatter

trait SExpr extends SMTLibFormatter

case class SExprs(expressions: Seq[SExpr] = Nil) extends SExpr with ReasonUnknown with GetProofResponse {
  override def format: String = s"(${SMTLibFormatter.format(expressions)})"
  override def pretty: String = s"${SMTLibFormatter.pretty(expressions, ", ")}"
}

// TODO: remove SMTLibFormatter? SExpr... inherits from it anyways
trait SpecConstant extends SMTLibFormatter with SExpr with AttributeValue with Term

//case class SpecConstant(s: String) extends SMTLibFormatter with SExpr {
//  override def format: String = s
//}