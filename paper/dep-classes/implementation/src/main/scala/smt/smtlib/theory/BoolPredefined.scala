package smt.smtlib.theory

import smt.smtlib.syntax.{Apply, SimpleSymbol, Sort, Term}

// TODO: move functions into case classes to enable pretty printing?
//  - the downside of this is that it's different elements in the syntax tree
// Bool exists in every theory
object BoolPredefined {
  // Sorts
  val Bool: Sort = SimpleSymbol("Bool")

//  // Primitives
//  val True: Term = SimpleSymbol("true")
//  val False: Term = SimpleSymbol("false")

  case object True extends Term {
    def apply(): Term = SimpleSymbol("true")

    override def format: String = True().format
    override def pretty: String = "⊤"
  }

  case object False extends Term {
    def apply(): Term = SimpleSymbol("false")

    override def format: String = False().format
    override def pretty: String = "⊥"
  }

  // TODO use !, =>, &&, ||, ^ as function names? make them infix (would require to put it into trait Term?)?
  // Operators
  def Not(term: Term): Term          = Apply(SimpleSymbol("not"), Seq(term))
  def Implies(l: Term, r:Term): Term = Apply(SimpleSymbol("=>"),  Seq(l, r))
  def And(args: Term*): Term = Apply(SimpleSymbol("and"), args)
  def Or(args: Term*): Term = Apply(SimpleSymbol("or"), args)
  def Xor(args: Term*): Term = Apply(SimpleSymbol("xor"), args)
  def Eq(args: Term*): Term = Apply(SimpleSymbol("="), args)
  def Distinct(args: Term*): Term = Apply(SimpleSymbol("distinct"), args)
  def Ite(condition: Term, ifTrue: Term, ifFalse: Term): Term = Apply(SimpleSymbol("ite"), Seq(condition, ifTrue, ifFalse))
}