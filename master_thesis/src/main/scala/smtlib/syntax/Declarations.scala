package smtlib.syntax

import smtlib.{SMTLibCommand, SMTLibFormatter}

case class DeclareConst(symbol: SMTLibSymbol, sort: Sort) extends SMTLibCommand {
  override def format(): String = s"(declare-const ${symbol.format()} ${sort.format()})"
}

case class DeclareDatatype(symbol: SMTLibSymbol, datatype: DatatypeDec) extends SMTLibCommand {
  override def format(): String = s"(declare-datatype ${symbol.format()} ${datatype.format()})"
}

case class DeclareDatatypes(sorts: Seq[SortDec], datatypes: Seq[DatatypeDec]) extends SMTLibCommand {
  override def format(): String = s"(declare-datatypes (${SMTLibFormatter.format(sorts)}) (${SMTLibFormatter.format(datatypes)}))"
}

case class DeclareFun(symbol: SMTLibSymbol, argSorts: Seq[Sort], retSort: Sort) extends SMTLibCommand {
  override def format(): String = s"(declare-fun ${symbol.format()} (${SMTLibFormatter.format(argSorts)}) ${retSort.format()})"
}

case class DeclareSort(symbol: SMTLibSymbol, numeral: Numeral) extends SMTLibCommand {
  override def format(): String = s"(declare-sort ${symbol.format()} ${numeral.format()})"
}

// Aux
case class SortDec(symbol: SMTLibSymbol, numeral: Numeral) extends SMTLibFormatter {
  override def format(): String = s"(${symbol.format()} ${numeral.format()})"
}

case class SelectorDec(symbol: SMTLibSymbol, sort: Sort) extends SMTLibFormatter {
  override def format(): String = s"(${symbol.format()} ${sort.format()})"
}

case class ConstructorDec(symbol: SMTLibSymbol, selectors: Seq[SelectorDec]) extends SMTLibFormatter {
  override def format(): String = if (selectors.isEmpty) s"(${symbol.format()})" else s"(${symbol.format()} ${SMTLibFormatter.format(selectors)})"
}

trait DatatypeDec extends SMTLibFormatter

case class ConstructorDatatype(constructors: Seq[ConstructorDec]) extends DatatypeDec {
  override def format(): String = s"(${SMTLibFormatter.format(constructors)})"
}

case class ParDatatype(symbols: Seq[SMTLibSymbol], constructors: Seq[ConstructorDec]) extends DatatypeDec {
  override def format(): String = s"(par (${SMTLibFormatter.format(symbols)}) (${SMTLibFormatter.format(constructors)}))"
}