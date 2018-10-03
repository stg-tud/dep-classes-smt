package smtlib.syntax

import smtlib.SMTLibFormatter


trait Identifier extends SMTLibFormatter

case class ComposedIdentifier(symbol: SMTLibSymbol, indices: Seq[Index]) extends Identifier {
  override def format(): String = s"(_ ${symbol.format()} ${SMTLibFormatter.format(indices)})"
}

trait Index extends SMTLibFormatter