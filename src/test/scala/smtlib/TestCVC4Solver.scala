package smtlib

import org.scalatest.FunSuite
import smtlib.solver.CVC4Solver
import smtlib.syntax._
import smtlib.syntax.Implicit._

class TestCVC4Solver extends FunSuite {
  val cvc4 = new CVC4Solver(SMTLibScript(Seq()), debug=true)

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
  val script = SMTLibScript(Seq(const1, const2, const3, distinct, assert1, assert2, assert3))

  test("AddCommand") {
    val preSize = cvc4.commands.size

    cvc4.addCommand(const1)
    cvc4.addCommand(const2)
    cvc4.addCommand(const3)

    assert(preSize + 3 == cvc4.commands.size)
    assert(cvc4.commands(preSize) == const1)
    assert(cvc4.commands(preSize+1) == const2)
    assert(cvc4.commands(preSize+2) == const3)
  }

  test("AddCommands") {
    val preSize = cvc4.commands.size

    val assert4 = Assert(Not(Eq("a", 0)))

    cvc4.addCommands(Seq(assert1, assert2, assert3, assert4))

    assert(preSize + 4 == cvc4.commands.size)
    assert(cvc4.commands(preSize) == assert1)
    assert(cvc4.commands(preSize+1) == assert2)
    assert(cvc4.commands(preSize+2) == assert3)
    assert(cvc4.commands(preSize+3) == assert4)
  }

  test("checksat SatFormula") {
    val sat = cvc4.checksat()

    assert(sat == Sat)
  }

  test("Execute SatFormula") {
    cvc4.addCommand(CheckSat)

    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "sat")
  }

  test("Flush") {
    cvc4.flush()
    assert(cvc4.commands.isEmpty)
  }

  test("checksat UnsatFormula") {
    cvc4.addScript(script)
    val sat = cvc4.checksat()

    assert(sat == Unsat)
  }

  test("Execute UnsatFormula") {
    cvc4.addCommand(CheckSat)

    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "unsat")
  }

  test("Axioms with debugging") {
    val cvc4 = new CVC4Solver(script, debug=true)
    cvc4.addCommand(CheckSat)
    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "unsat")
  }

  // not unknow in cvc4
//  test("Unknown") {
//    val T = DeclareDatatype("T", ConstructorDatatype(Seq(ConstructorDec("NUM", Seq(SelectorDec("n", "Real"))))))
//    val a = DeclareConst("a", "T")
//    val b = DeclareConst("b", "T")
//    val c = DeclareConst("c", "T")
//
//    val isNUMa = Assert(Apply("is-NUM", Seq("a")))
//    val isNUMb = Assert(Apply("is-NUM", Seq("b")))
//    val isNUMc = Assert(Apply("is-NUM", Seq("c")))
//
//    val eq = Assert(Eq("c", Apply("NUM", Seq(Apply("*", Seq(Apply("n", Seq("a")), Apply("n", Seq("b"))))))))
//
//    cvc4.flush()
//    cvc4.addCommands(Seq(T, a, b, c, isNUMa, isNUMb, isNUMc, eq, CheckSat))
//
//    val (status, output) = cvc4.execute()
//
//    assert(status == 0)
//    assert(output.size == 1)
//    assert(output.head == "unknown")
//  }

  test("checksat Unknown") {
    val sat = cvc4.checksat()
    assert(sat == Unknown)
  }
}
