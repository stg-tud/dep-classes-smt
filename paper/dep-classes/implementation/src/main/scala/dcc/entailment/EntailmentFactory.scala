package dcc.entailment

import dcc.entailment.EntailmentSort.EntailmentSort
import dcc.syntax.Program.Program

object EntailmentFactory {
  def apply(cls: EntailmentSort)(program: Program, debug: Int): Entailment = cls match {
    case dcc.entailment.EntailmentSort.Semantic => new SemanticEntailment(program, debug)
    case dcc.entailment.EntailmentSort.SimplifiedSemantic => new SimplifiedSemanticEntailment(program, debug)
  }
}

object EntailmentSort extends Enumeration {
  type EntailmentSort = Value
  val Semantic, SimplifiedSemantic = Value
}