package smtlib.syntax

import smtlib.SMTLibFormatter

trait Lexicon {

}

// TODO: long?
case class Numeral(num: Int) extends SMTLibFormatter {
  override def format(): String = num.toString
}

case class Decimal(dec: Double) extends SMTLibFormatter {
  override def format(): String = dec.toString
}

case class Hexadecimal(hex: String) extends SMTLibFormatter {
  // TODO: only a to f or A to F
  require(hex.filter(c => !c.isDigit && !aToF(c)).isEmpty, "Hexadecimals: only digits or letters from a/A to f/F")
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

case class Binary(bin: String) extends SMTLibFormatter {
  require(bin.filter(c => c != '0' && c != '1').isEmpty, "Binary: only digits 0 or 1")
  override def format(): String = s"#b$bin"
}

case class StringLiteral(s: String) extends SMTLibFormatter {
  override def format(): String = s
}