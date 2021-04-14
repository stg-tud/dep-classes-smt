package smt.smtlib.syntax

object Sugar {
  def Op(name: String)(args: Term*): Term = Apply(SimpleSymbol(name), args)
  def Op(name: SMTLibSymbol)(args: Term*): Term = Apply(name, args)
}