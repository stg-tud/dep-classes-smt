package smt.smtlib.theory

import smt.smtlib.SMTLibFormatter
import smt.smtlib.syntax.{Apply, SimpleSymbol, Sort, Term}

// TODO: revert case classes to be defs again (as pretty printing for bool is already in Term.Apply)?
// Bool exists in every theory
object BoolPredefined {
  // Sorts
  val Bool: Sort = SimpleSymbol("Bool")

  // Primitives
  case object True extends Term {
    def apply(): Term = SimpleSymbol("true")

    override def format: String = this().format
    override def pretty: String = "⊤"
  }

  case object False extends Term {
    def apply(): Term = SimpleSymbol("false")

    override def format: String = this().format
    override def pretty: String = "⊥"
  }

  // Operators
  def Distinct(args: Term*): Term = Apply(SimpleSymbol("distinct"), args)

  case class Not(term: Term) extends Term {
    def apply(): Term = Apply(SimpleSymbol("not"), Seq(term))

    override def format: String = this().format //s"(not ${term.format})"
    override def pretty: String = s"¬${term.pretty}"
  }

  case class Implies(lhs: Term, rhs: Term) extends Term {
    def apply(): Term = Apply(SimpleSymbol("=>"),  Seq(lhs, rhs))

    override def format: String = this().format
    override def pretty: String = s"(${lhs.pretty} → ${rhs.pretty})"
  }

  case class And(args: Term*) extends Term {
    def apply(): Term = Apply(SimpleSymbol("and"), args)

    override def format: String = this().format
    override def pretty: String = infix("∧")(args:_*)
  }

  case class Or(args: Term*) extends Term {
    def apply(): Term = Apply(SimpleSymbol("or"), args)

    override def format: String = this().format
    override def pretty: String = infix("∨")(args:_*)
  }

  case class Xor(args: Term*) extends Term {
    def apply(): Term = Apply(SimpleSymbol("xor"), args)

    override def format: String = this().format
    override def pretty: String = infix("⊕")(args:_*)
  }

  case class Eq(args: Term*) extends Term {
    def apply(): Term = Apply(SimpleSymbol("="), args)

    override def format: String = this().format
    override def pretty: String = infix("=")(args:_*)
  }

  case class Ite(condition: Term, ifTrue: Term, ifFalse: Term) extends Term {
    def apply(): Term = Apply(SimpleSymbol("ite"), Seq(condition, ifTrue, ifFalse))

    override def format: String = this().format
    override def pretty: String = s"if(${condition.pretty}) ${ifTrue.pretty} else ${ifFalse.pretty})" // TODO: line breaks after if() and before else?
  }

  private def infix(operator: String)(args: Term*): String = s"(${SMTLibFormatter.pretty(args, s" $operator ")})"
}