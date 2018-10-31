package smtlib.syntax

import smtlib.SMTLibFormatter

// TODO: long?
case class Numeral(num: Int) extends SMTLibFormatter with SpecConstant with Index {
  override def format(): String = num.toString
}

case class Decimal(dec: Double) extends SMTLibFormatter with SpecConstant {
  override def format(): String = dec.toString
}

case class Hexadecimal(hex: String) extends SMTLibFormatter with SpecConstant {
  require(hex.filter(c => !c.isDigit && !aToF(c)).isEmpty, "Hexadecimals: only digits or letters from a/A to f/F")

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
  require(bin.filter(c => c != '0' && c != '1').isEmpty, "Binary: only digits 0 or 1")
  override def format(): String = s"#b$bin"
}

object Binary {
  def apply(bin: Int): Binary = Binary(bin.toBinaryString)
}

case class SMTLibString(s: String) extends SMTLibFormatter with SpecConstant with EchoResponse {
  override def format(): String = s
}

trait SMTLibSymbol extends SMTLibFormatter with SExpr with Index with Identifier with AttributeValue with PropLiteral

case class SimpleSymbol(symbol: String) extends SMTLibSymbol {
  require(symbol.nonEmpty &&
    //!symbol.charAt(0).isDigit && TODO: relaxed beaucse of Sorts("List", "1") for the time being, how to generate this otherwise? AttributeValues("List", 1), but is no sort
    symbol.filter(c => !c.isLetterOrDigit && !isAllowedChar(c)).isEmpty,
  s"SimpleSymbol $symbol: nonempty, doesnt start with digit and only contains letters, digits and ~ ! @ $$ % ^ & * _ - + = < > . ?")

  override def format(): String = symbol

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
  require(symbol.filter(c => c == '|' && c == '\\').isEmpty,
  s"QuotedSymbol $symbol: must not contain | or \\")

  override def format(): String = s"|$symbol|"
}

case class Keyword(symbol: String) extends SMTLibFormatter with SExpr with Attribute with InfoFlag {
  require(SimpleSymbol(symbol).symbol == symbol)
  override def format(): String = s":$symbol"
}