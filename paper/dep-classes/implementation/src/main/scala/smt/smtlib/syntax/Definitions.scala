package smt.smtlib.syntax

import smt.smtlib.{SMTLibCommand, SMTLibFormatter}

case class DefineFun(fun: FunctionDef) extends SMTLibCommand with ModelResponse {
  override def format: String = s"(define-fun ${fun.format})"
  override def pretty: String = fun.pretty
}

case class DefineFunRec(fun: FunctionDef) extends SMTLibCommand with ModelResponse {
  override def format: String = s"(define-fun-rec ${fun.format})"
  override def pretty: String = fun.pretty
}

case class DefineFunsRec(declarations: Seq[FunctionDec], terms: Seq[Term]) extends SMTLibCommand with ModelResponse {
  override def format: String = s"(define-funs-rec (${SMTLibFormatter.format(declarations)}) (${SMTLibFormatter.format(terms)}))"
  override def pretty: String =
    if (declarations.size == terms.size)
      declarations.zip(terms).foldRight(""){ case ((declaration, body), xs) => s"${declaration.pretty} ≔ ${body.pretty}\n$xs" }
    else
      ""
}

case class DefineSort(symbol: SMTLibSymbol, symbols: Seq[SMTLibSymbol], sort: Sort) extends SMTLibCommand {
  override def format: String = s"(define-sort ${symbol.format} (${SMTLibFormatter.format(symbols)}) ${sort.format})"
  override def pretty: String = s"${symbol.pretty} (${SMTLibFormatter.pretty(symbols, ", ")}: ${sort.pretty})"
}

// Aux
case class FunctionDec(symbol: SMTLibSymbol, args: Seq[SortedVar], ret: Sort) extends SMTLibFormatter {
  override def format: String = s"(${symbol.format} (${SMTLibFormatter.format(args)}) ${ret.format})"
  override def pretty: String = s"def ${symbol.pretty}(${SMTLibFormatter.pretty(args, ", ")}): ${ret.pretty}"
}

case class FunctionDef(symbol: SMTLibSymbol, args: Seq[SortedVar], ret: Sort, body: Term) extends SMTLibFormatter {
  override def format: String = s"${symbol.format} (${SMTLibFormatter.format(args)}) ${ret.format} ${body.format}"
  override def pretty: String = s"def ${symbol.pretty}(${SMTLibFormatter.pretty(args, ", ")}): ${ret.pretty} ≔ ${body.pretty}"
}