package smt.smtlib.theory

import smt.smtlib.syntax.{Apply, QualifiedIdentifier, SimpleSymbol, Sort, Term}
import smt.smtlib.theory.BoolPredefined.And

import scala.annotation.showAsInfix

// Bool exists in every theory
object BoolPredefined {
  // Sorts
  val Bool: Sort = SimpleSymbol("Bool")

  // Primitives
  val True: Term = SimpleSymbol("true")
  val False: Term = SimpleSymbol("false")

  // TODO use !, =>, &&, ||, ^ as function names? make them infix (would require to put it into trait Term?)?
  // Operators
  def Not(term: Term): Term                 = Apply(SimpleSymbol("not"), Seq(term))
  def Implies(left: Term, right:Term): Term = Apply(SimpleSymbol("=>"),  Seq(left, right))
  def And(left: Term, right:Term): Term     = Apply(SimpleSymbol("and"), Seq(left, right))
  def Or(left: Term, right:Term): Term      = Apply(SimpleSymbol("or"),  Seq(left, right))
  def Xor(left: Term, right:Term): Term     = Apply(SimpleSymbol("xor"), Seq(left, right))
  def Eq(left: Term, right:Term): Term      = Apply(SimpleSymbol("="),   Seq(left, right))
  def Distinct(args: Term*): Term           = Apply(SimpleSymbol("distinct"), args)
  def Ite(condition: Term, ifTrue: Term, ifFalse: Term): Term = Apply(SimpleSymbol("ite"), Seq(condition, ifTrue, ifFalse))
}