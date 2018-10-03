package smtlib.syntax

import smtlib.SMTLibFormatter

trait SExpr extends SMTLibFormatter

case class SExprs(exprs: Seq[SExpr] = Nil) extends SExpr {
  override def format(): String = s"(${SMTLibFormatter.format(exprs)})"
}

// TODO: remove SMTLibFormatter? SExpr... inherits fromt it anyways
trait SpecConstant extends SMTLibFormatter with SExpr with AttributeValue

//case class SpecConstant(s: String) extends SMTLibFormatter with SExpr {
//  override def format(): String = s
//}