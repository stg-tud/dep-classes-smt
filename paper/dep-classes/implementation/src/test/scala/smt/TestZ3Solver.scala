package smt

import org.scalatest.funsuite.AnyFunSuite
import solver.Z3Solver
import smtlib._
import smtlib.syntax._
import smtlib.syntax.Implicit._
import smtlib.theory.BoolPredefined._

class TestZ3Solver extends AnyFunSuite {
  private val const1: SMTLibCommand = DeclareConst("a", "Int")
  private val const2: SMTLibCommand = DeclareConst("b", "Int")
  private val const3: SMTLibCommand = DeclareConst("c", "Int")
  private val ab: Term = Apply("+", Seq("a", "b"))
  private val aa: Term = Apply("+", Seq("a", "a"))
  private val bb: Term = Apply("+", Seq("b", "b"))
  private val abIsC: Term = Eq(ab, "c")
  private val aaIsC: Term = Eq(aa, "c")
  private val bbIsC: Term = Eq(bb, "c")
  private val assert1: SMTLibCommand = Assert(abIsC)
  private val assert2: SMTLibCommand = Assert(aaIsC)
  private val assert3: SMTLibCommand = Assert(bbIsC)
  private val assert4: SMTLibCommand = Assert(Not(Eq("a", 0)))
  private val distinct: SMTLibCommand = Assert(Distinct("a", "b"))
  private val commands: SMTLibScript = SMTLibScript(Seq(const1, const2, const3, distinct, assert1, assert2, assert3))
  private val script: SMTLibScript = SMTLibScript(commands.commands :+ CheckSat)

  test("AddCommand") {
    val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq()), debug = false)

    z3.addCommand(const1)
    z3.addCommand(const2)
    z3.addCommand(const3)

    assert(z3.commands.size == 3)
    assert(z3.commands == Seq(const1, const2, const3))
  }

  test("AddCommands") {
    val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq()), debug = false)

    z3.addCommands(assert1, assert2, assert3, assert4)

    assert(z3.commands.size == 4)
    assert(z3.commands == Seq(assert1, assert2, assert3, assert4))
  }

  test("AddScript") {
    val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq()), debug = false)
    z3.addScript(script)

    assert(z3.commands == script.commands)
  }

  test("Flush") {
    val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq()), debug = false)
    z3.addCommands(const1, const2, const3, assert1, assert2, assert3, assert4, CheckSat)
    z3.flush()
    assert(z3.commands.isEmpty)
  }

  test("checkSat SatFormula") {
    val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq(
      const1, const2, const3, assert1, assert2, assert3, assert4)), debug = false)
    val sat = z3.checkSat

    assert(sat == Left(Sat))
  }

  test("Execute SatFormula") {
    val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq(
      const1, const2, const3, assert1, assert2, assert3, assert4, CheckSat
    )), debug = false)

    val (status, output) = z3.execute

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "sat")
  }

  test("Execute UnsatFormula") {
    val z3: Z3Solver = new Z3Solver(script, debug = false)
    val (status, output) = z3.execute

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "unsat")
  }

  test("checkSat UnsatFormula") {
    val z3: Z3Solver = new Z3Solver(commands, debug = false)
    val sat = z3.checkSat

    assert(sat == Left(Unsat))
  }

  test("Axioms with debugging") {
    val z3 = new Z3Solver(script, debug=true)
    val (status, output) = z3.execute

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "unsat")
  }

  test("Unknown or Sat") {
    val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq()), debug = false)

    val T = DeclareDatatype("T", ConstructorDatatype(Seq(ConstructorDec("NUM", Seq(SelectorDec("n", "Real"))))))
    val a = DeclareConst("a", "T")
    val b = DeclareConst("b", "T")
    val c = DeclareConst("c", "T")

    val isNUMa = Assert(Apply("is-NUM", Seq("a")))
    val isNUMb = Assert(Apply("is-NUM", Seq("b")))
    val isNUMc = Assert(Apply("is-NUM", Seq("c")))

    val eq = Assert(Eq("c", Apply("NUM", Seq(Apply("*", Seq(Apply("n", Seq("a")), Apply("n", Seq("b"))))))))

    z3.addCommands(T, a, b, c, isNUMa, isNUMb, isNUMc, eq, CheckSat)

    val (status, output) = z3.execute

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "sat") // z3 4.8.7 can solve this
  }

  test("checkSat Unknown or Sat") {
    val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq()), debug = false)

    val T = DeclareDatatype("T", ConstructorDatatype(Seq(ConstructorDec("NUM", Seq(SelectorDec("n", "Real"))))))
    val a = DeclareConst("a", "T")
    val b = DeclareConst("b", "T")
    val c = DeclareConst("c", "T")

    val isNUMa = Assert(Apply("is-NUM", Seq("a")))
    val isNUMb = Assert(Apply("is-NUM", Seq("b")))
    val isNUMc = Assert(Apply("is-NUM", Seq("c")))

    val eq = Assert(Eq("c", Apply("NUM", Seq(Apply("*", Seq(Apply("n", Seq("a")), Apply("n", Seq("b"))))))))

    z3.addCommands(T, a, b, c, isNUMa, isNUMb, isNUMc, eq, CheckSat)

    val sat = z3.checkSat
    assert(sat == Left(Sat)) // z3 4.8.7 can solve this
  }

  test("(List Int)") {
    val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq()), debug = false)

    val i = DeclareConst("i", "Int")
    val j = DeclareConst("j", "Int")
    val l = DeclareConst("l", Sorts("List", Seq("Int")))

    val content = Assert(Eq(
      "l",
      Apply("insert", Seq("i",
        Apply("insert", Seq("j", "nil"))))
    ))

    z3.addCommands(i, j, l, content)

    val sat = z3.checkSat
    assert(sat == Left(Sat))
  }

  test("compare list") {
    val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq()), debug = false)

    val i = DeclareConst("i", "Int")
    val j = DeclareConst("j", "Int")
    val l = DeclareConst("l", Sorts("List", Seq("Int")))

    val content = Assert(Eq(
      "l",
      Apply("insert", Seq(1,
        Apply("insert", Seq(2, "nil"))))
    ))

    val iIs1 = Assert(Eq("i", 1))
    val jIs3 = Assert(Eq("j", 3))

    val assertion = Assert(Eq(
      "l",
      Apply("insert", Seq("i",
        Apply("insert", Seq("j", "nil"))))
    ))

    z3.addCommand(i)
    z3.addCommand(j)
    z3.addCommand(l)
    z3.addCommand(content)
    z3.addCommand(iIs1)
    z3.addCommand(jIs3)
    z3.addCommand(assertion)

    val sat = z3.checkSat
    assert(sat == Left(Unsat))
  }

  test("nonempty int list") {
    val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq()), debug = false)

    val datatype = DeclareDatatype(
      "lst",
      ConstructorDatatype(Seq(
        ConstructorDec("single", Seq(SelectorDec("content", "Int"))),
        ConstructorDec("multi", Seq(
          SelectorDec("elem", "Int"),
          SelectorDec("rest", "lst"),
        )),
      ))
    )

    val l = DeclareConst("l", "lst")
    val content = Assert(Eq("l", Apply("single", Seq(1))))

    z3.addCommand(datatype)
    z3.addCommand(l)
    z3.addCommand(content)

    val sat = z3.checkSat
    assert(sat == Left(Sat))
  }

  test("nonempty int list let-function") {
    val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq()), debug = false)

    val datatype = DeclareDatatype(
      "lst",
      ConstructorDatatype(Seq(
        ConstructorDec("single", Seq(SelectorDec("content", "Int"))),
        ConstructorDec("multi", Seq(
          SelectorDec("elem", "Int"),
          SelectorDec("rest", "lst"),
        )),
      ))
    )

    val funLet = DefineFun(FunctionDef(
      "first",
      Seq(
        SortedVar("k", "lst")
      ),
      "Int",
      Ite(
        Apply("is-single", Seq("k")),
        Let(
          Seq(VarBinding("i", Apply("content", Seq("k")))),
          "i"
        ),
        Let(
          Seq(
            VarBinding("hd", Apply("elem", Seq("k"))),
            VarBinding("tl", Apply("rest", Seq("k")))
          ),
          "hd"
        )
      )
    ))

    val l1 = DeclareConst("l1", "lst")
    val l2 = DeclareConst("l2", "lst")
    val content1 = Assert(Eq("l1", Apply("single", Seq(2))))
    val content2 = Assert(Eq("l2", Apply("multi", Seq(1, "l1"))))

    val assertion = Assert(Not(And(
      Eq(
        Apply("first", Seq("l1")),
        2
      ),
      Eq(
        Apply("first", Seq("l2")),
        1
      )
    )))

    z3.addCommand(datatype)
    z3.addCommand(funLet)
    z3.addCommand(l1)
    z3.addCommand(content1)
    z3.addCommand(l2)
    z3.addCommand(content2)
    z3.addCommand(assertion)

    val sat = z3.checkSat
    assert(sat == Left(Unsat))
  }

  test("nonempty int list pattern-match-function") {
    val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq()), debug = false)

    val datatype = DeclareDatatype(
      "lst",
      ConstructorDatatype(Seq(
        ConstructorDec("single", Seq(SelectorDec("content", "Int"))),
        ConstructorDec("multi", Seq(
          SelectorDec("elem", "Int"),
          SelectorDec("rest", "lst"),
        )),
      ))
    )

    // (error "line 2 column 43: constructor symbol expected")
    // does work again with z3 4.8.7
    val fun = DefineFun(
      FunctionDef(
        "first",
        Seq(
          SortedVar("k", "lst")
        ),
        "Int",
        Match("k",
          Seq(
            MatchCase(Pattern("single", Seq("content")),
              "content"),
            MatchCase(Pattern("multi", Seq("hd", "tl")),
              "hd")
          )
        )
      )
    )

    val l1 = DeclareConst("l1", "lst")
    val l2 = DeclareConst("l2", "lst")
    val content1 = Assert(Eq("l1", Apply("single", Seq(2))))
    val content2 = Assert(Eq("l2", Apply("multi", Seq(1, "l1"))))

    val assertion = Assert(Not(And(
      Eq(
        Apply("first", Seq("l1")),
        2
      ),
      Eq(
        Apply("first", Seq("l2")),
        1
      )
    )))

    z3.addCommand(datatype)
    z3.addCommand(fun)
    z3.addCommand(l1)
    z3.addCommand(content1)
    z3.addCommand(l2)
    z3.addCommand(content2)
    z3.addCommand(assertion)

    val sat = z3.checkSat
    // TODO: check for error in case of z3 version > 4.7 & < 4.8.7
    assert(sat == Left(Unsat))
  }

  test("list function") {
    val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq()), debug = false)

    val fun = DefineFun(
      FunctionDef(
        "first",
        Seq(
          SortedVar("lst", Sorts("List", Seq("Int"))),
          SortedVar("default", "Int")
        ),
        "Int",
        Ite(
          Apply("is-nil", Seq("lst")),
          "default",
          Let(
            Seq(
              VarBinding("hd", Apply("head", Seq("lst"))),
              VarBinding("tl", Apply("tail", Seq("lst")))
            ),
            "hd"
          )
        )
      )
    )

    //> (error "line 1 column 82: mismatching number of variables supplied to constructor")
    // → nil
//    Match("lst",
//      Seq(
//        MatchCase(Pattern("nil", Seq()),
//          "default"),
//        MatchCase(Pattern("insert", Seq("hd", "tl")),
//          "hd")
//      )
//    )

    val l = DeclareConst("l", Sorts("List", Seq("Int")))
    val content = Assert(Eq(
      "l",
      Apply("insert", Seq(1,
        Apply("insert", Seq(2, "nil"))))
    ))

    val assertion = Assert(Not(And(
        Eq(1, Apply("first", Seq("l", -999))),
        Eq(-999, Apply("first", Seq("nil", -999)))
    )))

    z3.addCommand(fun)
    z3.addCommand(l)
    z3.addCommand(content)
    z3.addCommand(assertion)

    assert(z3.checkSat == Left(Unsat))
  }

  test("test z3 error") {
    val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq()), debug = false)

    z3.addCommands(assert1)

    assert(z3.checkSat == Right(ErrorResponse(SMTLibString("(error \"line 1 column 15: unknown constant a\")")) :: Nil))
  }
}
