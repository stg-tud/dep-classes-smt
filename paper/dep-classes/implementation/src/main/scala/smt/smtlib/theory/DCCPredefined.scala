package smt.smtlib.theory

import smt.smtlib.syntax.{Apply, SMTLibSymbol, SimpleSymbol, Sort, Term}

object DCCPredefined {
  // Sort names
  val SortNameClass: String = "Class"
  val SortNameVariable: String = "Variable"
  val SortNamePath: String = "Path"

  // Sorts
  val SortClass: Sort = SimpleSymbol(SortNameClass)
  val SortVariable: Sort = SimpleSymbol(SortNameVariable)
  val SortPath: Sort = SimpleSymbol(SortNamePath)

  // Function names
  val FunctionPathEquivalence: SMTLibSymbol = SimpleSymbol("path-equivalence")
  val FunctionInstanceOf: SMTLibSymbol = SimpleSymbol("instance-of")
  val FunctionInstantiatedBy: SMTLibSymbol = SimpleSymbol("instantiated-by")
  val FunctionSubstitution: SMTLibSymbol = SimpleSymbol("substitute")

  case class PathEq(lhs: Term, rhs: Term) extends Term {
    def apply(): Term = Apply(FunctionPathEquivalence, Seq(lhs, rhs))

    override def format: String = this().format
    override def pretty: String = s"${lhs.pretty} ≡ ${rhs.pretty}"
  }

  case class InstOf(path: Term, cls: Term) extends Term {
    def apply(): Term = Apply(FunctionInstanceOf, Seq(path, cls))

    override def format: String = this().format
    override def pretty: String = s"${path.pretty} :: ${cls.pretty}"
  }

  case class InstBy(path: Term, cls: Term) extends Term {
    def apply(): Term = Apply(FunctionInstantiatedBy, Seq(path, cls))

    override def format: String = this().format
    override def pretty: String = s"${path.pretty}.cls = ${cls.pretty}"
  }

  case class Subst(source: Term, target: Term, replace: Term) extends Term {
    def apply(): Term = Apply(FunctionSubstitution, Seq(source, target, replace))

    override def format: String = this().format
    override def pretty: String = s"${source.pretty}{{${target.pretty} ↦ ${replace.pretty}}}"
  }

  case class SubstEq(source: Term, target: Term, replace: Term, result: Term) extends Term {
    def apply(): Term = Apply(FunctionSubstitution, Seq(source, target, replace, result))

    override def format: String = this().format
    override def pretty: String = s"${source.pretty}{{${target.pretty} ↦ ${replace.pretty}}} = ${result.pretty}"
  }
}
