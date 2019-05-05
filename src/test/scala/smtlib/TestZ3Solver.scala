package smtlib

import org.scalatest.FunSuite
import smtlib.solver.Z3Solver
import smtlib.syntax._
import smtlib.syntax.Implicit._

// TODO: update tests to function independent
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
  val commands = SMTLibScript(Seq(const1, const2, const3, distinct, assert1, assert2, assert3))
  val script = SMTLibScript(commands.commands :+ CheckSat)

  val z3: Z3Solver = new Z3Solver(SMTLibScript(Seq()), debug = true)

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

    z3.addCommands(Seq(assert1, assert2, assert3, assert4))

    assert(preSize + 4 == z3.commands.size)
    assert(z3.commands(preSize) == assert1)
    assert(z3.commands(preSize+1) == assert2)
    assert(z3.commands(preSize+2) == assert3)
    assert(z3.commands(preSize+3) == assert4)
  }

  test("checksat SatFormula") {
    val sat = z3.checksat()

    assert(sat == Sat)
  }

  test("Execute SatFormula") {
    z3.addCommand(CheckSat)

    val (status, output) = z3.execute()

    assert(status == 0)
    assert(output.size == 1)
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

  test("checksat UnsatFormula") {
    z3.flush()
    z3.addScript(commands)
    val sat = z3.checksat()

    assert(sat == Unsat)
  }

  test("Axioms with debugging") {
    val z3 = new Z3Solver(script, debug=true)
    val (status, output) = z3.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "unsat")
  }

  test("Unknown") {
    val T = DeclareDatatype("T", ConstructorDatatype(Seq(ConstructorDec("NUM", Seq(SelectorDec("n", "Real"))))))
    val a = DeclareConst("a", "T")
    val b = DeclareConst("b", "T")
    val c = DeclareConst("c", "T")

    val isNUMa = Assert(Apply("is-NUM", Seq("a")))
    val isNUMb = Assert(Apply("is-NUM", Seq("b")))
    val isNUMc = Assert(Apply("is-NUM", Seq("c")))

    val eq = Assert(Eq("c", Apply("NUM", Seq(Apply("*", Seq(Apply("n", Seq("a")), Apply("n", Seq("b"))))))))

    z3.flush()
    z3.addCommands(Seq(T, a, b, c, isNUMa, isNUMb, isNUMc, eq, CheckSat))

    val (status, output) = z3.execute()

    assert(status == 0)
    assert(output.size == 1)
    assert(output.head == "unknown")
  }

  test("checksat Unknown") {
    val sat = z3.checksat()
    assert(sat == Unknown)
  }

  test("(List Int)") {
    val i = DeclareConst("i", "Int")
    val j = DeclareConst("j", "Int")
    val l = DeclareConst("l", Sorts("List", Seq("Int")))

    val content = Assert(Eq(
      "l",
      Apply("insert", Seq("i",
        Apply("insert", Seq("j", "nil"))))
    ))

    z3.flush()
    z3.addCommands(Seq(i, j, l, content))

    val sat = z3.checksat()
    assert(sat == Sat)
  }

  test("compare list") {
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

    z3.flush()
    z3.addCommand(i)
    z3.addCommand(j)
    z3.addCommand(l)
    z3.addCommand(content)
    z3.addCommand(iIs1)
    z3.addCommand(jIs3)
    z3.addCommand(assertion)

    val sat = z3.checksat()
    assert(sat == Unsat)
  }

  test("nonempty int list") {
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

    z3.flush()
    z3.addCommand(datatype)
    z3.addCommand(l)
    z3.addCommand(content)

    val sat = z3.checksat()
    assert(sat == Sat)
  }

  test("nonempty int list function") {
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

    z3.flush()
    z3.addCommand(datatype)
    z3.addCommand(funLet)
    z3.addCommand(l1)
    z3.addCommand(content1)
    z3.addCommand(l2)
    z3.addCommand(content2)
    z3.addCommand(assertion)

    val sat = z3.checksat()
    assert(sat == Unsat)
  }

  test("list function") {
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

    z3.flush()
    z3.addCommand(fun)
    z3.addCommand(l)
    z3.addCommand(content)
    z3.addCommand(assertion)

    assert(z3.checksat() == Unsat)
  }

  // TODO: remove timeout part in Z3Solver?
//  test("Timeout") {
//    val (status, output) = z3.execute(0)
//
//    println(status)
//    output.foreach(println(_))
//  }
}
