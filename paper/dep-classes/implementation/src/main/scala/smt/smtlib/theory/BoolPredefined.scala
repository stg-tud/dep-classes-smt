package smt.smtlib.theory

import smt.smtlib.syntax.{Apply, SimpleSymbol, Sort, Term}

// Bool exists in every theory
object BoolPredefined {
  // Sorts
  val Bool: Sort = SimpleSymbol("Bool")

  // Primitives
  val True: Term = SimpleSymbol("true")
  val False: Term = SimpleSymbol("false")

  // TODO use !, =>, &&, ||, ^ as function names? make them infix (would require to put it into trait Term?)?
  // TODO: safety check for sequence arguments if empty return true?
  // Operators
  def Not(term: Term): Term          = Apply(SimpleSymbol("not"), Seq(term))
  def Implies(l: Term, r:Term): Term = Apply(SimpleSymbol("=>"),  Seq(l, r))
  def And(args: Term*): Term = {
    if (args.isEmpty)
      throw new IllegalArgumentException("invalid function application, arguments missing") // TODO: or return True?
    else
      Apply(SimpleSymbol("and"), args)
  }
  def Or(args: Term*): Term = {
    if (args.isEmpty)
      throw new IllegalArgumentException("invalid function application, arguments missing") // TODO: or return True?
    else
      Apply(SimpleSymbol("or"), args)
  }
  def Xor(args: Term*): Term = {
    if (args.isEmpty)
      throw new IllegalArgumentException("invalid function application, arguments missing") // TODO: or return True?
    else
      Apply(SimpleSymbol("xor"), args)
  }
  def Eq(args: Term*): Term = {
    if (args.isEmpty)
      throw new IllegalArgumentException("invalid function application, arguments missing") // TODO: or return True?
    else
      Apply(SimpleSymbol("="), args)
  }
  def Distinct(args: Term*): Term = {
    if (args.isEmpty)
      throw new IllegalArgumentException("invalid function application, arguments missing") // TODO: or return True?
    else
      Apply(SimpleSymbol("distinct"), args)
  }
  def Ite(condition: Term, ifTrue: Term, ifFalse: Term): Term = Apply(SimpleSymbol("ite"), Seq(condition, ifTrue, ifFalse))
}