package smt.smtlib.syntax

import smt.smtlib.{SMTLibCommand, SMTLibFormatter}

case class DeclareConst(symbol: SMTLibSymbol, sort: Sort) extends SMTLibCommand {
  override def format: String = s"(declare-const ${symbol.format} ${sort.format})"
  override def pretty: String = s"val ${symbol.pretty}: ${sort.pretty}"
}

case class DeclareDatatype(symbol: SMTLibSymbol, datatype: DatatypeDec) extends SMTLibCommand {
  override def format: String = s"(declare-datatype ${symbol.format} ${datatype.format})"
  override def pretty: String = s"type ${symbol.pretty} = ${datatype.pretty}"
}

case class DeclareDatatypes(sorts: Seq[SortDec], datatypes: Seq[DatatypeDec]) extends SMTLibCommand {
  override def format: String = s"(declare-datatypes (${SMTLibFormatter.format(sorts)}) (${SMTLibFormatter.format(datatypes)}))"
  override def pretty: String = {
    if (sorts.size == datatypes.size)
      "datatype:\n" + sorts.zip(datatypes).foldRight("") { case ((sort, datatype), xs) => s"\ttype ${sort.pretty} = ${datatype.pretty}\n$xs" }
    else
      ""
  }
}

case class DeclareFun(symbol: SMTLibSymbol, argSorts: Seq[Sort], retSort: Sort) extends SMTLibCommand {
  override def format: String = s"(declare-fun ${symbol.format} (${SMTLibFormatter.format(argSorts)}) ${retSort.format})"
  override def pretty: String = s"${symbol.pretty}: ${SMTLibFormatter.pretty(argSorts, " × ")} → ${retSort.pretty}"
}

case class DeclareSort(symbol: SMTLibSymbol, numeral: Numeral) extends SMTLibCommand {
  override def format: String = s"(declare-sort ${symbol.format} ${numeral.format})"
  override def pretty: String = s"sort ${symbol.pretty} with arity ${numeral.pretty}" // TODO is arity correct?
}

// Aux
case class SortDec(symbol: SMTLibSymbol, numeral: Numeral) extends SMTLibFormatter {
  override def format: String = s"(${symbol.format} ${numeral.format})"
  override def pretty: String = s"${symbol.pretty} ${numeral.pretty}"
}

case class SelectorDec(symbol: SMTLibSymbol, sort: Sort) extends SMTLibFormatter {
  override def format: String = s"(${symbol.format} ${sort.format})"
  override def pretty: String = s"${symbol.pretty}: ${sort.pretty}"
}

case class ConstructorDec(symbol: SMTLibSymbol, selectors: Seq[SelectorDec]) extends SMTLibFormatter {
  override def format: String = if (selectors.isEmpty) s"(${symbol.format})" else s"(${symbol.format} ${SMTLibFormatter.format(selectors)})"
  override def pretty: String = if (selectors.isEmpty) s"${symbol.pretty}" else s"${symbol.pretty}(${SMTLibFormatter.pretty(selectors, ", ")})"
}

trait DatatypeDec extends SMTLibFormatter

case class ConstructorDatatype(constructors: Seq[ConstructorDec]) extends DatatypeDec {
  override def format: String = s"(${SMTLibFormatter.format(constructors)})"
  override def pretty: String = s"{${SMTLibFormatter.pretty(constructors, ", ")}}"
}

case class ParDatatype(symbols: Seq[SMTLibSymbol], constructors: Seq[ConstructorDec]) extends DatatypeDec {
  override def format: String = s"(par (${SMTLibFormatter.format(symbols)}) (${SMTLibFormatter.format(constructors)}))"
  override def pretty: String = s"${SMTLibFormatter.pretty(symbols, ", ")} → ${SMTLibFormatter.pretty(constructors, ", ")}"
}