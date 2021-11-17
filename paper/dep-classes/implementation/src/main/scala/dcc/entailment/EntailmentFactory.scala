package dcc.entailment

import dcc.entailment.EntailmentSort.{Algorithmic, AlgorithmicFix1, AlgorithmicFix2, EntailmentSort, GroundPathDepthLimit, PathDepthLimit, Semantic, SimplifiedSemantic}
import dcc.syntax.Program.Program

object EntailmentFactory {
  def apply(cls: EntailmentSort)(program: Program, debug: Int): Entailment = cls match {
    case Semantic => new SemanticEntailment(program, debug)
    case SimplifiedSemantic => new SimplifiedSemanticEntailment(program, debug)
    case PathDepthLimit => new PathDepthLimitEncoding(program, debug)
    case GroundPathDepthLimit => new GroundPathDepthLimitEncoding(program, debug)
    case Algorithmic => new Algorithmic(program, debug)
    case AlgorithmicFix1 => new AlgorithmicFix1(program, debug)
    case AlgorithmicFix2 => new AlgorithmicFix2(program, debug)
  }
}

object EntailmentSort extends Enumeration {
  type EntailmentSort = Value
  val Semantic, SimplifiedSemantic, PathDepthLimit, GroundPathDepthLimit, Algorithmic, AlgorithmicFix1, AlgorithmicFix2 = Value
}