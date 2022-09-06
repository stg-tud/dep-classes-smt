package dcc.entailment.smt

import com.github.gnush.smt.smtlib.syntax.{SMTLibSymbol, SimpleSymbol, Sort}

object DCCStrings {
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
}
