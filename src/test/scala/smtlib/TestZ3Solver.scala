package smtlib

import org.scalatest.FunSuite
import smtlib.solver.Z3Solver
import smtlib.syntax._
import smtlib.syntax.Implicit._

class TestZ3Solver extends FunSuite {
  val const1 = DeclareConst("a", "Int")
  val const2 = DeclareConst("b", "Int")
  val const3 = DeclareConst("c", "Int")
  val ab = Apply("+", Seq("a", "b"))
  val aa = Apply("+", Seq("a", "a"))
  val bb = Apply("+", Seq("b", "b"))
  val abIsC = Eq(ab, "c")
  val aaIsC = Eq(aa, "c")
  val bbIsC = Eq(bb, "c")
  val assert1 = Assert(abIsC)
  val assert2 = Assert(aaIsC)
  val assert3 = Assert(bbIsC)
  val distinct = Assert(Distinct("a", "b"))
  val script = SMTLibScript(Seq(const1, const2, const3, distinct, assert1, assert2, assert3, CheckSat))

  val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq()))

  test("AddCommand") {
    val preSize = z3.commands.size

    z3.addCommand(const1)
    z3.addCommand(const2)
    z3.addCommand(const3)

    assert(preSize + 3 == z3.commands.size)
    assert(z3.commands(preSize) == const1)
    assert(z3.commands(preSize+1) == const2)
    assert(z3.commands(preSize+2) == const3)
  }

  test("AddCommands") {
    val preSize = z3.commands.size

    val assert4 = Assert(Not(Eq("a", 0)))

    z3.addCommands(Seq(assert1, assert2, assert3, assert4, CheckSat, GetModel))

    assert(preSize + 6 == z3.commands.size)
    assert(z3.commands(preSize) == assert1)
    assert(z3.commands(preSize+1) == assert2)
    assert(z3.commands(preSize+2) == assert3)
    assert(z3.commands(preSize+3) == assert4)
    assert(z3.commands(preSize+4) == CheckSat)
    assert(z3.commands(preSize+5) == GetModel)
  }

  test("Execute SatFormula") {
    val (status, output) = z3.execute()

    assert(status == 0)
    assert(output.size > 1)
    assert(output.head == "sat")
  }

  test("Flush") {
    z3.flush()
    assert(z3.commands.isEmpty)
  }

  test("Execute UnsatFormula") {
    z3.addScript(script)
    val (status, output) = z3.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "unsat")
  }
}
