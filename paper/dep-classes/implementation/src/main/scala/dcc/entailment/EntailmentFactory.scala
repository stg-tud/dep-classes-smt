package dcc.entailment

import dcc.entailment.EntailmentSort.{Algorithmic, AlgorithmicFix1, AlgorithmicFix1RandomizedPick, AlgorithmicFix2, AlgorithmicFix2RandomizedPick, EntailmentSort, GroundPathDepthLimit, PathDepthLimit, Semantic, SimplifiedSemantic}
import dcc.entailment.algorithmic.{AlgorithmicSystem, AlgorithmicSystemFix1, AlgorithmicSystemFix1RandomizedPick, AlgorithmicSystemFix2, AlgorithmicSystemFix2RandomizedPick}
import dcc.syntax.Program.Program

object EntailmentFactory {
  def apply(cls: EntailmentSort)(program: Program, debug: Int): Entailment = cls match {
    case Semantic => new SemanticEntailment(program, debug)
    case SimplifiedSemantic => new SimplifiedSemanticEntailment(program, debug)
    case PathDepthLimit => new PathDepthLimitEncoding(program, debug)
    case GroundPathDepthLimit => new GroundPathDepthLimitEncoding(program, debug)
    case Algorithmic => new AlgorithmicSystem(program, debug)
    case AlgorithmicFix1 => new AlgorithmicSystemFix1(program, debug)
    case AlgorithmicFix2 => new AlgorithmicSystemFix2(program, debug)
    case AlgorithmicFix1RandomizedPick => new AlgorithmicSystemFix1RandomizedPick(program, debug)
    case AlgorithmicFix2RandomizedPick => new AlgorithmicSystemFix2RandomizedPick(program, debug)
  }
}

object EntailmentSort extends Enumeration {
  type EntailmentSort = Value
  val Semantic, SimplifiedSemantic, PathDepthLimit, GroundPathDepthLimit, Algorithmic, AlgorithmicFix1, AlgorithmicFix1RandomizedPick, AlgorithmicFix2, AlgorithmicFix2RandomizedPick = Value
}