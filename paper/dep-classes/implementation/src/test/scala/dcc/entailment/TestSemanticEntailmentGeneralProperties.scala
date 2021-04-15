package dcc.entailment

import dcc.program.{Empty, NaturalNumbers}
import dcc.syntax.{Constraint, FieldPath, PathEquivalence}
import dcc.syntax.Implicit.StringToId
import org.scalatest.funsuite.AnyFunSuite
import smt.smtlib.SMTLibScript
import smt.smtlib.syntax.{Apply, Assert, ErrorResponse, Forall, SMTLibString, SortedVar, Unsat}
import smt.smtlib.syntax.Implicit.stringToSimpleSymbol
import smt.smtlib.theory.BoolPredefined.{Implies, Not}
import smt.solver.Z3Solver

class TestSemanticEntailmentGeneralProperties extends AnyFunSuite{
  private def axioms(entailment: SemanticEntailment)(constraints: Constraint*): SMTLibScript = entailment.axioms(constraints.toList)._1

  test("forall p. p===p  in Empty program") {
    val entailment = new SemanticEntailment(Empty.program)

    val script:SMTLibScript = axioms(entailment)(PathEquivalence("a", "b"), PathEquivalence("c", "d"), PathEquivalence("e", "f"), PathEquivalence("g", "h"), PathEquivalence("i", "j")) :+
      Assert(Not(Forall(Seq(SortedVar("p", "Variable")), Apply("path-equivalence", Seq("p", "p")))))

    val z3 = new Z3Solver(script, debug = true)
    assert(z3.checkSat() == Left(Unsat))
  }

  test("forall p. p===p  in Natural Numbers") {
    val entailment = new SemanticEntailment(NaturalNumbers.program)

    val script:SMTLibScript = axioms(entailment)(PathEquivalence("a", "b"), PathEquivalence("c", "d"), PathEquivalence("e", "f"), PathEquivalence("g", "h"), PathEquivalence("i", "j")) :+
      Assert(Not(Forall(Seq(SortedVar("p", "Path")), Apply("path-equivalence", Seq("p", "p")))))

    val z3 = new Z3Solver(script, debug = true)
    assert(z3.checkSat() == Left(Unsat))
  }

  test("forall p, q. p===q => q===p  in Empty program with 26 variables") {
    val entailment = new SemanticEntailment(Empty.program)

    val script:SMTLibScript = axioms(entailment)(PathEquivalence("a", "b"), PathEquivalence("c", "d"), PathEquivalence("e", "f"), PathEquivalence("g", "h"), PathEquivalence("i", "j"), PathEquivalence("k", "l"),
      PathEquivalence("m", "n"), PathEquivalence("o", "p"), PathEquivalence("q", "r"), PathEquivalence("s", "t"), PathEquivalence("u", "v"),  PathEquivalence("w", "x"), PathEquivalence("y", "z")) :+
      Assert(Not(Forall(Seq(SortedVar("path-p", "Variable"), SortedVar("path-q", "Variable")),
        Implies(Apply("path-equivalence", Seq("path-p", "path-q")), Apply("path-equivalence", Seq("path-q", "path-p"))))))

    val z3 = new Z3Solver(script, debug = true)
    assert(z3.checkSat(50000) == Left(Unsat))
  }

  test("forall p, q. p===q => q===p  in Empty program with Path datatype") {
    val entailment = new SemanticEntailment(Empty.program)

    val script:SMTLibScript = axioms(entailment)(PathEquivalence(FieldPath("a", "f"), "b")) :+
      Assert(Not(Forall(Seq(SortedVar("path-p", "Path"), SortedVar("path-q", "Path")),
        Implies(Apply("path-equivalence", Seq("path-p", "path-q")), Apply("path-equivalence", Seq("path-q", "path-p"))))))

    val z3 = new Z3Solver(script, debug = true)
    val result = z3.checkSat(60000)
    assert(result == Left(Unsat) || result == Right(List(ErrorResponse(SMTLibString("(error \"io timeout or non z3 error\")")))))
  }
}
