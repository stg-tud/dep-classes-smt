package smtlib

import org.scalatest.FunSuite
import smtlib.solver.CVC4Solver
import smtlib.syntax._
import smtlib.syntax.Implicit._

class TestCVC4Solver extends FunSuite {
  val cvc4 = new CVC4Solver(SMTLibScript(Seq()), debug = true) // TODO: remove debug option

  val const1: SMTLibCommand = DeclareConst("a", "Int")
  val const2: SMTLibCommand = DeclareConst("b", "Int")
  val const3: SMTLibCommand = DeclareConst("c", "Int")
  val ab: Term = Apply("+", Seq("a", "b"))
  val aa: Term = Apply("+", Seq("a", "a"))
  val bb: Term = Apply("+", Seq("b", "b"))
  val abIsC: Term = Eq(ab, "c")
  val aaIsC: Term = Eq(aa, "c")
  val bbIsC: Term = Eq(bb, "c")
  val assert1: SMTLibCommand = Assert(abIsC)
  val assert2: SMTLibCommand = Assert(aaIsC)
  val assert3: SMTLibCommand = Assert(bbIsC)
  val distinct: SMTLibCommand = Assert(Distinct("a", "b"))
  val script: SMTLibScript = SMTLibScript(Seq(const1, const2, const3, distinct, assert1, assert2, assert3))

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
  test("Sat Boxed Real") {
    val T = DeclareDatatype("T", ConstructorDatatype(Seq(ConstructorDec("NUM", Seq(SelectorDec("n", "Real"))))))
    val a = DeclareConst("a", "T")
    val b = DeclareConst("b", "T")
    val c = DeclareConst("c", "T")

    val isNUMa = Assert(Apply("is-NUM", Seq("a")))
    val isNUMb = Assert(Apply("is-NUM", Seq("b")))
    val isNUMc = Assert(Apply("is-NUM", Seq("c")))

    val eq = Assert(Eq("c", Apply("NUM", Seq(Apply("*", Seq(Apply("n", Seq("a")), Apply("n", Seq("b"))))))))

    cvc4.flush()
    cvc4.addCommands(Seq(T, a, b, c, isNUMa, isNUMb, isNUMc, eq, CheckSat))

    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "sat")
  }

  test("checksat Sat Boxed Real") {
    val T = DeclareDatatype("T", ConstructorDatatype(Seq(ConstructorDec("NUM", Seq(SelectorDec("n", "Real"))))))
    val a = DeclareConst("a", "T")
    val b = DeclareConst("b", "T")
    val c = DeclareConst("c", "T")

    val isNUMa = Assert(Apply("is-NUM", Seq("a")))
    val isNUMb = Assert(Apply("is-NUM", Seq("b")))
    val isNUMc = Assert(Apply("is-NUM", Seq("c")))

    val eq = Assert(Eq("c", Apply("NUM", Seq(Apply("*", Seq(Apply("n", Seq("a")), Apply("n", Seq("b"))))))))

    cvc4.flush()
    cvc4.addCommands(Seq(T, a, b, c, isNUMa, isNUMb, isNUMc, eq))

    val sat = cvc4.checksat()
    assert(sat == Sat)
  }

  val pathDatatype: SMTLibCommand = DeclareDatatype("Path", ConstructorDatatype(Seq(
    ConstructorDec("var", Seq(SelectorDec("id", "String"))),
    ConstructorDec("pth", Seq(
      SelectorDec("obj", "Path"),
      SelectorDec("field", "String")
    ))
  )))

  test("Datatype Declaration") {
    cvc4.flush()

    cvc4.addCommand(pathDatatype)
    cvc4.addCommand(CheckSat)

    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "sat")
  }

  val pathConst: SMTLibCommand = DeclareConst("p", "Path")
  val pathContent1: SMTLibCommand = Assert(Eq("p", Apply("var", Seq(SMTLibString("x")))))

  test("Constructor invocation") {
    cvc4.flush()

    cvc4.addCommand(pathDatatype)
    cvc4.addCommand(pathConst)
    cvc4.addCommand(pathContent1)
    cvc4.addCommand(CheckSat)

    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "sat")
  }

  val pathContent2: SMTLibCommand = Assert(Eq("p", Apply("pth", Seq(Apply("var", Seq(SMTLibString("x"))), SMTLibString("f")))))

  test("Constructor equality") {
    cvc4.flush()

    cvc4.addCommand(pathDatatype)
    cvc4.addCommand(pathConst)
    cvc4.addCommand(pathContent1)
    cvc4.addCommand(pathContent2)
    cvc4.addCommand(CheckSat)

    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "unsat")
  }

  val listDatatype: SMTLibCommand = DeclareDatatype("List", ParDatatype(
    Seq( // symbols
      SimpleSymbol("T")
    ),
    Seq( // constructors
      ConstructorDec("nil", Seq()),
      ConstructorDec("cons", Seq(
        SelectorDec("hd", "T"),
        SelectorDec("tl", Sorts("List", Seq("T")))
      ))
    )
  ))

  test("Parameterized Datatype Declaration") {
    cvc4.flush()

    cvc4.addCommand(listDatatype)
    cvc4.addCommand(CheckSat)

    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "sat")
  }

  val l1: SMTLibCommand = DeclareConst("l1", Sorts("List", Seq("Int")))
  val l1Empty: SMTLibCommand = Assert(Eq("l1", IdentifierAs("nil", Sorts("List", Seq("Int")))))

  test("Empty List") {
    cvc4.flush()

    cvc4.addCommand(listDatatype)
    cvc4.addCommand(l1)
    cvc4.addCommand(l1Empty)
    cvc4.addCommand(CheckSat)

    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "sat")
  }

  val l1NonEmpty: SMTLibCommand = Assert(Eq("l1", Apply("cons", Seq(Numeral(1), IdentifierAs("nil", Sorts("List", Seq("Int")))))))

  test("Nonempty List") {
    cvc4.flush()

    cvc4.addCommand(listDatatype)
    cvc4.addCommand(l1)
    cvc4.addCommand(l1NonEmpty)
    cvc4.addCommand(CheckSat)

    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "sat")
  }

  val s1: SMTLibCommand = DeclareConst("s1", Sorts("Set", Seq("Int")))
  val s1Empty: SMTLibCommand = Assert(Eq("s1", IdentifierAs("emptyset", Sorts("Set", Seq("Int")))))

  test("Empty Set") {
    cvc4.flush()

    cvc4.addCommand(s1)
    cvc4.addCommand(s1Empty)
    cvc4.addCommand(CheckSat)

    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "sat")
  }

  val s1Singleton: SMTLibCommand = Assert(Eq("s1", Apply("singleton", Seq(Numeral(1)))))

  test("Singleton Set") {
    cvc4.flush()

    cvc4.addCommand(s1)
    cvc4.addCommand(s1Singleton)
    cvc4.addCommand(CheckSat)

    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "sat")
  }

  test("Set Equality") {
    cvc4.flush()

    cvc4.addCommand(s1)
    cvc4.addCommand(s1Empty)
    cvc4.addCommand(s1Singleton)
    cvc4.addCommand(CheckSat)

    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "unsat")
  }

  val s1Insert1: SMTLibCommand = Assert(Eq("s1", Apply("insert", Seq(Numeral(1), Apply("singleton", Seq(Numeral(2)))))))

  test("Set-Insert singleton") {
    cvc4.flush()

    cvc4.addCommand(s1)
    cvc4.addCommand(s1Insert1)
    cvc4.addCommand(CheckSat)

    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "sat")
  }

  val s1Insert2: SMTLibCommand = Assert(Eq("s1", Apply("insert", Seq(Numeral(2), Apply("insert", Seq(Numeral(1), IdentifierAs("emptyset", Sorts("Set", Seq("Int")))))))))

  test("Set-Insert empty") {
    cvc4.flush()

    cvc4.addCommand(s1)
    cvc4.addCommand(s1Insert2)
    cvc4.addCommand(CheckSat)

    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "sat")
  }

  test("Set Equality 2") {
    cvc4.flush()

    cvc4.addCommand(s1)
    cvc4.addCommand(s1Insert1)
    cvc4.addCommand(s1Insert2)
    cvc4.addCommand(CheckSat)

    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "sat")
  }

  test("Set Union") {
    cvc4.flush()

    cvc4.addCommand(Assert(Eq(
      Apply("union", Seq(Apply("singleton", Seq(Numeral(1))), Apply("singleton", Seq(Numeral(2))))),
      Apply("insert", Seq(Numeral(2), Apply("insert", Seq(Numeral(1), IdentifierAs("emptyset", Sorts("Set", Seq("Int")))))))
    )))
    cvc4.addCommand(CheckSat)

    val (status, output) = cvc4.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "sat")
  }
}
