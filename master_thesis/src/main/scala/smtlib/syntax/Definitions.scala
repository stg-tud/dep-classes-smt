package smtlib.syntax

import smtlib.{SMTLibCommand, SMTLibFormatter}

case class DefineFun(fun: FunctionDef) extends SMTLibCommand with ModelResponse {
  override def format(): String = s"(define-fun ${fun.format()})"
}

case class DefineFunRec(fun: FunctionDef) extends SMTLibCommand with ModelResponse {
  override def format(): String = s"(define-fun-rec ${fun.format()})"
}

case class DefineFunsRec(declarations: Seq[FunctionDec], terms: Seq[Term]) extends SMTLibCommand with ModelResponse {
  override def format(): String = s"(define-funs-rec (${SMTLibFormatter.format(declarations)}) (${SMTLibFormatter.format(terms)}))"
}

case class DefineSort(symbol: SMTLibSymbol, symbols: Seq[SMTLibSymbol], sort: Sort) extends SMTLibCommand {
  override def format(): String = s"(define-sort ${symbol.format()} (${SMTLibFormatter.format(symbols)}) ${sort.format()})"
}

// Aux
case class FunctionDec(symbol: SMTLibSymbol, args: Seq[SortedVar], ret: Sort) extends SMTLibFormatter {
  override def format(): String = s"(${symbol.format()} (${SMTLibFormatter.format(args)}) ${ret.format()})"
}

case class FunctionDef(symbol: SMTLibSymbol, args: Seq[SortedVar], ret: Sort, body: Term) extends SMTLibFormatter {
  override def format(): String = s"${symbol.format()} (${SMTLibFormatter.format(args)}) ${ret.format()} ${body.format()}"
}