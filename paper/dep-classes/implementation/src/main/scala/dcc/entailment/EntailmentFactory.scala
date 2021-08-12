package dcc.entailment

import dcc.entailment.EntailmentSort.{EntailmentSort, PathDepthLimit, Semantic, SimplifiedSemantic}
import dcc.syntax.Program.Program

object EntailmentFactory {
  def apply(cls: EntailmentSort)(program: Program, debug: Int): Entailment = cls match {
    case Semantic => new SemanticEntailment(program, debug)
    case SimplifiedSemantic => new SimplifiedSemanticEntailment(program, debug)
    case PathDepthLimit => new PathDepthLimitEncoding(program, debug)
  }
}

object EntailmentSort extends Enumeration {
  type EntailmentSort = Value
  val Semantic, SimplifiedSemantic, PathDepthLimit = Value
}