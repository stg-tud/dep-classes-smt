package smt.smtlib.syntax

import smt.smtlib.SMTLibFormatter

// TODO: long?
case class Numeral(num: Int) extends SMTLibFormatter with SpecConstant with Index {
  override def format(): String = num.toString
}

case class Decimal(dec: Double) extends SMTLibFormatter with SpecConstant {
  override def format(): String = dec.toString
}

case class Hexadecimal(hex: String) extends SMTLibFormatter with SpecConstant {
  require(hex.forall(c => c.isDigit || aToF(c)), "Hexadecimals: only digits or letters from a/A to f/F")
//  require(hex.filter(c => !c.isDigit && !aToF(c)).isEmpty, "Hexadecimals: only digits or letters from a/A to f/F")

//  def this(hex: Int) = this(hex.toHexString)

  override def format(): String = s"#x${hex.toUpperCase}"

  private def aToF(c: Char): Boolean = c.toUpper match {
    case 'A' => true
    case 'B' => true
    case 'C' => true
    case 'D' => true
    case 'E' => true
    case 'F' => true
    case  _  => false
  }
}
object Hexadecimal {
  def apply(hex: Int): Hexadecimal = Hexadecimal(hex.toHexString)
}

case class Binary(bin: String) extends SMTLibFormatter with SpecConstant {
  require(bin.forall(c => c == '0' || c == '1'), "Binary: only digits 0 or 1")
//  require(bin.filter(c => c != '0' && c != '1').isEmpty, "Binary: only digits 0 or 1")
  override def format(): String = s"#b$bin"
}

object Binary {
  def apply(bin: Int): Binary = Binary(bin.toBinaryString)
}

// TODO: requires that s is of the form |[chars]|? (test this)
//  instead assume s without the delimiters and add them in the formatting? (test this )
case class SMTLibString(s: String) extends SMTLibFormatter with SpecConstant with EchoResponse {
  // "\"" ++ s ++ "\""
  override def format(): String = s""""$s""""
}

trait SMTLibSymbol extends SMTLibFormatter with SExpr with Index with Identifier with AttributeValue with PropLiteral

case class SimpleSymbol(symbol: String) extends SMTLibSymbol {
  require(symbol.nonEmpty &&
    !symbol.charAt(0).isDigit &&
    symbol.forall(c => c.isLetterOrDigit || isAllowedChar(c)),
  s"SimpleSymbol $symbol: nonempty, doesnt start with digit and only contains letters, digits and ~ ! @ $$ % ^ & * _ - + = < > . ?")

  override def format(): String = symbol
  def +(right: SimpleSymbol): SimpleSymbol = SimpleSymbol(symbol+right.symbol)

  private def isAllowedChar(c: Char): Boolean = c match {
    case '~' => true
    case '!' => true
    case '@' => true
    case '$' => true
    case '%' => true
    case '^' => true
    case '&' => true
    case '*' => true
    case '_' => true
    case '-' => true
    case '+' => true
    case '=' => true
    case '<' => true
    case '>' => true
    case '.' => true
    case '?' => true
    case '/' => true
    case  _  => false
  }
}

case class QuotedSymbol(symbol: String) extends SMTLibSymbol {
  require(symbol.forall(c => c != '|' || c != '\\'),
  s"QuotedSymbol $symbol: must not contain | or \\")

  override def format(): String = s"|$symbol|"
}

case class Keyword(symbol: String) extends SMTLibFormatter with SExpr with Attribute with InfoFlag {
  require(SimpleSymbol(symbol).symbol == symbol)
  override def format(): String = s":$symbol"
}