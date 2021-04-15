package dcc.entailment

import dcc.program.{Empty, NaturalNumbers}
import dcc.syntax.{Constraint, PathEquivalence}
import dcc.syntax.Implicit.StringToId
import org.scalatest.funsuite.AnyFunSuite

class TestSemanticEntailmentEntails extends AnyFunSuite{
  test("p===p") {
    val entailment = new SemanticEntailment(Empty.program)

    assert(entailment.entails(Nil, PathEquivalence("x", "x")))
  }
}
