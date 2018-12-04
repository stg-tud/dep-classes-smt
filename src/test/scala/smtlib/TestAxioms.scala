package smtlib

import org.scalatest.FunSuite
import smtlib.solver.{Axioms, Z3Solver}
import smtlib.syntax._
import smtlib.syntax.Implicit._

class TestAxioms extends FunSuite {
  val options: Seq[SMTLibCommand] = Seq(SetOption(ProduceProofs(true)), SetOption(ProduceUnsatCores(true)))
  val z3 = new Z3Solver(Axioms.all, options, debug=true) // TODO: debug=false

  test ("Axioms.all well-formattet") {
    val (exit, out) = z3.execute()

    assert(exit == 0)
    assert(out.isEmpty)
  }

  test("Axioms.all sat") {
    z3.addCommand(CheckSat)
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 1)
    assert(out.head == Sat.format())
  }

  test("C-Ident") {
    val c = Axioms.pathEq(Axioms.path("x"), Axioms.path("y"))
    val ident = Assert(Not(Axioms.entails(Seq(c), c)))

    z3.addCommands(Seq(ident, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.nonEmpty)
    assert(out.head == Unsat.format())
  }

  test("C-Refl") {
    // !(nil |- x.f = x.f)
    val refl = Assert(Not(Axioms.entails(Seq(), Axioms.pathEq(Axioms.path("x.f"), Axioms.path("x.f")))))

    z3.addCommands(Seq(refl, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.nonEmpty)
    assert(out.head == Unsat.format())
  }
}
