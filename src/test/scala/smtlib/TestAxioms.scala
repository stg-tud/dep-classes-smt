package smtlib

import dcc.syntax.Program.Program
import dcc.syntax._
import org.scalatest.{FunSuite, PrivateMethodTester}
import smtlib.solver.{Axioms, Z3Solver}
import smtlib.syntax._
import smtlib.syntax.Implicit._

class TestAxioms extends FunSuite with PrivateMethodTester {
  val options: Seq[SMTLibCommand] = Seq(
//    SetOption(KeyValueAttribute(Keyword("smt.mbqi"), SimpleSymbol("true"))),
//    SetOption(KeyValueAttribute(Keyword("smt.mbqi.max_iterations"), Numeral(10000))),
//    SetOption(KeyValueAttribute(Keyword("model.compact"), SimpleSymbol("true"))),
//    SetOption(KeyValueAttribute(Keyword("smt.qi.eager_threshold"), Decimal(100))),
//    SetOption(KeyValueAttribute(Keyword("smt.qi.lazy_threshold"), Decimal(200))),
//    SetOption(KeyValueAttribute(Keyword("smt.qi.max_multi_patterns"), Numeral(4))),
//    SetOption(KeyValueAttribute(Keyword("pi.max_multi_patterns"), Numeral(4))), // breaks stuff
//    SetOption(KeyValueAttribute(Keyword("trace"), SimpleSymbol("true"))),
    SetOption(ProduceProofs(true)),
    SetOption(ProduceUnsatCores(true)))
  val debug = true  // TODO: false
  val z3 = new Z3Solver(Axioms.allDirectClosure, options, debug)

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

  test("Substitute Path 1") {
    val p1 = Axioms.path("x.g.h")
    val p2 = Axioms.path("y.f")
    val x = Axioms.string("x")

    val subst = Apply("subst-path", Seq(p1, x, p2))
    val expected = Axioms.path("y.f.g.h")


    z3.addCommands(Seq(Assert(Not(Eq(subst, expected))), CheckSat))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 1)
    assert(out.head == Unsat.format())
  }

  test("Substitute Path 2") {
    val p1 = Axioms.path("x.g.h")
    val p2 = Axioms.path("y.f")
    val y = Axioms.string("y")

    val subst = Apply("subst-path", Seq(p1, y, p2))
    val expected = Axioms.path("x.g.h")


    z3.addCommands(Seq(Assert(Not(Eq(subst, expected))), CheckSat))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 1)
    assert(out.head == Unsat.format())
  }

  test("Substitute Constraint") {
    val eq = Axioms.pathEq(Axioms.path("x.g.h"), Axioms.path("y.f"))
    val x = Axioms.string("x")
    val p = Axioms.path("z.m.n")

    val subst = Apply("subst-constraint", Seq(eq, x, p))
    val expected = Axioms.pathEq(Axioms.path("z.m.n.g.h"), Axioms.path("y.f"))

    z3.addCommands(Seq(Assert(Not(Eq(subst, expected))), CheckSat))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 1)
    assert(out.head == Unsat.format())
  }

  test("Generalize Path 1") {
    val p1 = Axioms.path("x.f.g.h")
    val p2 = Axioms.path("x.f")
    val y = Axioms.string("y")

    val gen = Apply("generalize-path", Seq(p1, p2, y))
    val expected = Axioms.path("y.g.h")


    z3.addCommands(Seq(Assert(Not(Eq(gen, expected))), CheckSat))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 1)
    assert(out.head == Unsat.format())
  }

  test("Generalize Path 2") {
    val p1 = Axioms.path("x.f.g.h")
    val p2 = Axioms.path("x.h")
    val y = Axioms.string("y")

    val gen = Apply("generalize-path", Seq(p1, p2, y))
    val expected = Axioms.path("x.f.g.h")


    z3.addCommands(Seq(Assert(Not(Eq(gen, expected))), CheckSat))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 1)
    assert(out.head == Unsat.format())
  }

  test("Generalize Constraint") {
    val eq = Axioms.pathEq(Axioms.path("z.m.n.g.h"), Axioms.path("y.f"))
    val p = Axioms.path("z.m.n")
    val x = Axioms.string("x")

    val subst = Apply("generalize-constraint", Seq(eq, p, x))
    val expected = Axioms.pathEq(Axioms.path("x.g.h"), Axioms.path("y.f"))

    z3.addCommands(Seq(Assert(Not(Eq(subst, expected))), CheckSat))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 1)
    assert(out.head == Unsat.format())
  }

  test("generalization is inverse of substitution example") {
    val base = Axioms.path("x.g")

    val x = Axioms.string("x")
    val p = Axioms.path("x.f")

    val assertion = Assert(Not(Eq(base,
      Apply("generalize-path", Seq(
        Apply("subst-path", Seq(base, x, p)), p, x))
    )))

    z3.addCommands(Seq(assertion, CheckSat))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 1)
    assert(out.head == Unsat.format())
  }

//  test("generalization is inverse of substitution formal") {
//    val assertion = Assert(Not(
//      Forall(Seq(SortedVar("base", "Path"), SortedVar("p", "Path"), SortedVar("x", "String")),
//        Eq(
//          "base",
//          Apply("generalize-path", Seq(
//            Apply("subst-path", Seq("base", "x", "p")), "p", "x"))
//        ))))
//
//    z3.addCommands(Seq(assertion, CheckSat))
//    val (exit, out) = z3.execute() // tested with up to 5s timout
//    z3.flush()
//
//    assert(exit == 0)
//    assert(out.size == 1)
//    //assert(out.head == Unsat.format()) // TODO: should be unsat
//    assert(out.head == Unknown.format()) // TODO: but solver cannot verify this property
//  }

  test("preprocess subst rule") {
    val x = SMTLibString("x")
    val p1 = Apply("var", Seq(SMTLibString("x")))
    val p2 = Apply("var", Seq(SMTLibString("y")))

//    val expectedSubstRule = Forall(
//      Seq(
//        SortedVar("a2", "Constraint"),
//        SortedVar("cs", Sorts("List", Seq("Constraint"))),
//        SortedVar("a", "Constraint"),
//        SortedVar("a1", "Constraint")
//      ),
//      Implies(
//        And(
//          Apply("variable", Seq(x)),
//          And(
//            Apply("path-exists", Seq(p1)),
//            And(
//              Apply("path-exists", Seq(p2)),
//              And(
//                Apply("entails", Seq("cs", Apply("path-eq", Seq(p1, p2)))),
//                And(
//                  Apply("subst", Seq("a", x, p2, "a1")),
//                  And(
//                    Apply("generalization", Seq("a2", p1, x, "a")),
//                    Apply("entails", Seq("cs", "a1"))
//                  )
//                )
//              )
//            )
//          )
//        ),
//        Apply("entails", Seq("cs", "a2"))
//      )
//    )
    val expectedSubstRule = Forall(
      Seq(
        SortedVar("a2", "Constraint"),
        SortedVar("cs", Sorts("List", Seq("Constraint")))
      ),
      Let(
        Seq(
          VarBinding("a", Apply("generalize-constraint", Seq("a2", p1, x)))
        ),
        Let(
          Seq(
            VarBinding("a1", Apply("subst-constraint", Seq("a", x, p2)))
          ),
          Implies(
            And(
              Apply("entails", Seq("cs", Apply("path-eq", Seq(p1, p2)))),
              Apply("entails", Seq("cs", "a1"))
            ),
            Apply("entails", Seq("cs", "a2"))
          )
        )
      )
    )

    val expectedAnnotation = Annotate(expectedSubstRule, Seq(KeyValueAttribute(Keyword("named"), "C-Subst_x_x_y")))

    val substRule = Axioms.preprocessSubstRule(x, p1, p2)

    assert(substRule == expectedSubstRule)
    assert(Axioms.annotateSubstRule(substRule, "x", "x", "y") == expectedAnnotation)

    z3.addCommands(Seq(Assert(expectedAnnotation), CheckSat))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 1)
    assert(out.head == "sat")
  }

  test("makePathPairs") {
    val x = Axioms.path("x")
    val y = Axioms.path("y")
    val z = Axioms.path("z")

    val paths = Seq(("x", x), ("y", y), ("z", z))

    val expected = Seq(
      (("x", x), ("x", x)),
      (("x", x), ("y", y)),
      (("x", x), ("z", z)),
      (("y", y), ("x", x)),
      (("y", y), ("y", y)),
      (("y", y), ("z", z)),
      (("z", z), ("x", x)),
      (("z", z), ("y", y)),
      (("z", z), ("z", z))
    )

    val makePathPairs = PrivateMethod[Seq[((String, Term), (String, Term))]]('makePathPairs)

    val actual = Axioms invokePrivate makePathPairs(paths)

    assert(actual.size == expected.size)
    assert(actual.forall(pair => expected.contains(pair)))

    // not interested in ordering
//    assert((Axioms invokePrivate makePathPairs(paths)) == expectedPaths)
  }

  test("Preprocess Subst Rules") {
    val vars = Seq(SMTLibString("x"), SMTLibString("y"))
    val paths = Seq(
      ("x", Apply("var", Seq(SMTLibString("x")))),
      ("y", Apply("var", Seq(SMTLibString("y"))))
    )

    // TODO: handwrite this?
    val expectedRules = Seq(
      Assert(Annotate(
        Axioms.preprocessSubstRule(SMTLibString("x"), Apply("var", Seq(SMTLibString("x"))), Apply("var", Seq(SMTLibString("x")))),
        Seq(KeyValueAttribute(Keyword("named"), "C-Subst_x_x_x"))
      )),
      Assert(Annotate(
        Axioms.preprocessSubstRule(SMTLibString("x"), Apply("var", Seq(SMTLibString("x"))), Apply("var", Seq(SMTLibString("y")))),
        Seq(KeyValueAttribute(Keyword("named"), "C-Subst_x_x_y"))
      )),
      Assert(Annotate(
        Axioms.preprocessSubstRule(SMTLibString("x"), Apply("var", Seq(SMTLibString("y"))), Apply("var", Seq(SMTLibString("x")))),
        Seq(KeyValueAttribute(Keyword("named"), "C-Subst_x_y_x"))
      )),
      Assert(Annotate(
        Axioms.preprocessSubstRule(SMTLibString("x"), Apply("var", Seq(SMTLibString("y"))), Apply("var", Seq(SMTLibString("y")))),
        Seq(KeyValueAttribute(Keyword("named"), "C-Subst_x_y_y"))
      )),
      Assert(Annotate(
        Axioms.preprocessSubstRule(SMTLibString("y"), Apply("var", Seq(SMTLibString("x"))), Apply("var", Seq(SMTLibString("x")))),
        Seq(KeyValueAttribute(Keyword("named"), "C-Subst_y_x_x"))
      )),
      Assert(Annotate(
        Axioms.preprocessSubstRule(SMTLibString("y"), Apply("var", Seq(SMTLibString("x"))), Apply("var", Seq(SMTLibString("y")))),
        Seq(KeyValueAttribute(Keyword("named"), "C-Subst_y_x_y"))
      )),
      Assert(Annotate(
        Axioms.preprocessSubstRule(SMTLibString("y"), Apply("var", Seq(SMTLibString("y"))), Apply("var", Seq(SMTLibString("x")))),
        Seq(KeyValueAttribute(Keyword("named"), "C-Subst_y_y_x"))
      )),
      Assert(Annotate(
        Axioms.preprocessSubstRule(SMTLibString("y"), Apply("var", Seq(SMTLibString("y"))), Apply("var", Seq(SMTLibString("y")))),
        Seq(KeyValueAttribute(Keyword("named"), "C-Subst_y_y_y"))
      ))
    )

    val substRules = Axioms.preprocessSubstRules(vars, paths)

    // assert(substRules == expectedRules) // not interested in ordering
    assert(substRules.size == expectedRules.size)
    substRules.foreach(rule => assert(expectedRules.contains(rule)))

    z3.addCommands(expectedRules)
    z3.addCommand(CheckSat)
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 1)
    assert(out.head == "sat")
  }

  test("C-Ident") {
    // !(x = y |- x = y)
    val c = Axioms.pathEq(Axioms.path("x"), Axioms.path("y"))
    val assertion = Assert(Not(Axioms.entails(Seq(c), c)))

    z3.addCommands(Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    assert(out(1) == "(C-Ident)")
  }

  test("C-Refl 1") {
    // !(nil |- x.f = x.f)
    val assertion = Assert(Not(Axioms.entails(Seq(), Axioms.pathEq(Axioms.path("x.f"), Axioms.path("x.f")))))

    z3.addCommands(Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    assert(out(1) == "(C-Refl)")
  }

  test("C-Refl 2") {
    // !(x.f :: Cls |- x.f = x.f)
    val x = Axioms.path("x.f")
    val xx = Axioms.pathEq(x, x)
    val of = Axioms.instanceOf(x, "Cls")
    val cls = Assert(Axioms.cls("Cls"))

    val assertion = Assert(Not(Axioms.entails(Seq(of), xx)))

    z3.addCommands(Seq(cls, assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    assert(out(1).contains("C-Refl"))
  }

  test("C-Class 1") {
    val x = Axioms.path("x")

    val of = Axioms.instanceOf(x, "Cls")
    val by = Axioms.instantiatedBy(x, "Cls")

    val knowledge = Seq(
      Axioms.assertClass("Cls"),
      Axioms.assertPath(x),
      Assert(Axioms.entails(Seq(), by))
    )
    val assertion = Assert(Not(Axioms.entails(Seq(), of)))

    z3.addCommands(knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    assert(out(1) == "(C-Class)")
  }

  test("C-Class 2") {
    val x = Axioms.path("x")

    val of = Axioms.instanceOf(x, "Cls")
    val by = Axioms.instantiatedBy(x, "Cls")

    val knowledge = Seq(
      Axioms.assertClass("Cls"),
      Axioms.assertPath(x)
    )
    val assertion = Assert(Not(Axioms.entails(Seq(by), of)))

    z3.addCommands(knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    assert(out(1) == "(C-Ident C-Class)")
  }

  test("C-Weak (x = y, y = z |- y = z)") {
    val x = Axioms.path("x")
    val y = Axioms.path("y")
    val z = Axioms.path("z")

    val xy = Axioms.pathEq(x, y)
    val yz = Axioms.pathEq(y, z)

    val knowledge = Seq(
      Axioms.assertPath(x),
      Axioms.assertPath(y),
      Axioms.assertPath(z),
      Axioms.assertVariable("x"),
      Axioms.assertVariable("y"),
      Axioms.assertVariable("z")
    )
    val assertion = Assert(Not(Axioms.entails(Seq(xy, yz), yz)))

    z3.addCommands(knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    assert(out(1).contains("C-Weak"))
    assert(out(1).contains("C-Ident"))
  }

  // TODO: reverse as algorithmic case of permutation?
//  test("C-Reverse (y = z, x = y |- y = z)") {
//    val x = Axioms.path("x")
//    val y = Axioms.path("y")
//    val z = Axioms.path("z")
//
//    val xy = Axioms.pathEq(x, y)
//    val yz = Axioms.pathEq(y, z)
//
//    val knowledge = Seq(
//      Axioms.assertPath(x),
//      Axioms.assertPath(y),
//      Axioms.assertPath(z),
//      Axioms.assertVariable("x"),
//      Axioms.assertVariable("y"),
//      Axioms.assertVariable("z")
//    )
//    val assertion = Assert(Not(Axioms.entails(Seq(yz, xy), yz)))
//
//    val reverse = DefineFunRec(
//      FunctionDef(
//        "reverse",
//        Seq(
//          SortedVar("l", Sorts("List", Seq("Constraint"))),
//          SortedVar("acc", Sorts("List", Seq("Constraint")))
//        ),
//        Sorts("List", Seq("Constraint")),
//        Match("l", Seq(
//          MatchCase(Pattern("nil"),
//            "acc"),
//          MatchCase(Pattern("insert", Seq("hd", "tl")),
//            Apply("reverse", Seq(
//              "tl",
//              Apply("insert", Seq("hd", "acc"))
//            ))
//          )
//        ))
//      ))
//
//    val reverseTerm = Forall(Seq(SortedVar("c", "Constraint"), SortedVar("cs", Sorts("List", Seq("Constraint")))),
//      Implies(
//        Apply("entails", Seq(
//          Apply("reverse", Seq("cs", "nil")),
//          "c")),
//        Apply("entails", Seq("cs", "c"))))
//    val cReverse = Assert(Annotate(reverseTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Reverse"))))
//
//    z3.addCommands(knowledge ++ Seq(reverse, cReverse, assertion, CheckSat, GetUnsatCore))
//    val (exit, out) = z3.execute()
//    z3.flush()
//
//    assert(exit == 0)
//    assert(out.size == 2)
//    assert(out.head == Unsat.format())
//    assert(out(1).contains("C-Reverse"))
//    assert(out(1).contains("C-Weak"))
//    assert(out(1).contains("C-Ident"))
//  }

  // TODO: add rule DirectIdent to Axioms (rücksprache halten, combination aus perm, weak, ident)
//  test("C-DirectIdent (y = z, x = y |- y = z)") {
//
//    val x = Axioms.path("x")
//    val y = Axioms.path("y")
//    val z = Axioms.path("z")
//
//    val xy = Axioms.pathEq(x, y)
//    val yz = Axioms.pathEq(y, z)
//
//    val knowledge = Seq(
//      Axioms.assertPath(x),
//      Axioms.assertPath(y),
//      Axioms.assertPath(z),
//      Axioms.assertVariable("x"),
//      Axioms.assertVariable("y"),
//      Axioms.assertVariable("z")
//    )
//    val assertion = Assert(Not(Axioms.entails(Seq(yz, xy), yz)))
//
//    val identTerm = Forall(Seq(SortedVar("c", "Constraint"), SortedVar("cs", Sorts("List", Seq("Constraint")))),
//      Implies(
//        Apply("elem", Seq("c", "cs")),
//        Apply("entails", Seq("cs", "c"))))
//    val cIdent = Assert(Annotate(identTerm, Seq(KeyValueAttribute(Keyword("named"), "C-DirectIdent"))))
//
//    z3.addCommands(knowledge ++ Seq(cIdent, assertion, CheckSat, GetUnsatCore))
//    val (exit, out) = z3.execute(5000)
//    z3.flush()
//
//    assert(exit == 0)
//    assert(out.size == 2)
//    assert(out.head == Unsat.format())
//    assert(out(1).contains("C-DirectIdent"))
//  }

  test("Contraction") {
    // !(x = y, x = y |- x = y)
    val c = Axioms.pathEq(Axioms.path("x"), Axioms.path("y"))
    val assertion = Assert(Not(Axioms.entails(Seq(c, c), c)))

    z3.addCommands(Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    assert(out(1).contains("C-Ident"))
  }

  test("C-Perm") {
    val x = Axioms.path("x")
    val y = Axioms.path("y")
    val z = Axioms.path("z")

    val xy = Axioms.pathEq(x, y)
    val yz = Axioms.pathEq(y, z)
    val xz = Axioms.pathEq(x, z)

    val knowledge = Seq(
      Axioms.assertPath(x),
      Axioms.assertPath(y),
      Axioms.assertPath(z),
      Axioms.assertVariable("x"),
      Axioms.assertVariable("y"),
      Axioms.assertVariable("z"),
      Assert(Axioms.entails(Seq(yz, xz, xy), yz))
    )
    val assertion = Assert(Not(Axioms.entails(Seq(xy, yz, xz), yz)))

    z3.addCommands(knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    assert(
      out(1) == "(C-Perm)" ||
      out(1).contains("C-Ident")
    )
  }

  test("PathEq is symmetric 1") {
    val x = Axioms.path("x")
    val y = Axioms.path("y")
    val xy = Axioms.pathEq(x, y)
    val yx = Axioms.pathEq(y, x)

    val vars = Seq(Axioms.string("x"), Axioms.string("y"))
    val paths = Seq(("x", x), ("y", y))

    val preprocessed = Axioms.preprocessSubstRules(vars, paths)

    val knowledge = Seq(
      Assert(Axioms.variable("x")),
      Assert(Axioms.variable("y")),
      Assert(Axioms.pathExists(x)),
      Assert(Axioms.pathExists(y)),
      Assert(Axioms.entails(Seq(), xy))
    )
    val assertion = Assert(Not(Axioms.entails(Seq(), yx)))

    z3.addCommands(preprocessed ++ knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    assert(out(1).contains("C-Refl"))
    assert(out(1).contains("C-Subst"))
  }

  test("PathEq is symmetric 2") {
    val x = Axioms.path("x")
    val y = Axioms.path("y")
    val xy = Axioms.pathEq(x, y)
    val yx = Axioms.pathEq(y, x)

    val vars = Seq(Axioms.string("x"), Axioms.string("y"))
    val paths = Seq(("x", x), ("y", y))

    val preprocessed = Axioms.preprocessSubstRules(vars, paths)

    val knowledge = Seq(
      Axioms.assertVariable("x"),
      Axioms.assertVariable("y"),
      Axioms.assertPath(x),
      Axioms.assertPath(y)
    )
    val assertion = Assert(Not(Axioms.entails(Seq(xy), yx)))

    z3.addCommands(preprocessed ++ knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    assert(out(1).contains("C-Refl"))
    assert(out(1).contains("C-Weak"))
    assert(out(1).contains("C-Subst"))
  }

  test("PathEq is transitive 1") {
    val x = Axioms.path("x")
    val y = Axioms.path("y")
    val z = Axioms.path("z")

    val xy = Axioms.pathEq(x, y)
    val yz = Axioms.pathEq(y, z)
    val xz = Axioms.pathEq(x, z)

    val vars = Seq(Axioms.string("x"), Axioms.string("y"), Axioms.string("z"))
    val paths = Seq(("x", x), ("y", y), ("z", z))
    val preprocessed = Axioms.preprocessSubstRules(vars, paths)

    val knowledge = Seq(
      Axioms.assertVariable("x"),
      Axioms.assertVariable("y"),
      Axioms.assertVariable("z"),
      Axioms.assertPath(x),
      Axioms.assertPath(y),
      Axioms.assertPath(z),
      Assert(Axioms.entails(Seq(), xy)),
      Assert(Axioms.entails(Seq(), yz)),
    )
    val assertion = Assert(Not(Axioms.entails(Seq(), xz)))

    z3.addCommands(preprocessed ++ knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    assert(out(1).contains("C-Subst"))
  }

  test("PathEq is transitive 2") {
    val x = Axioms.path("x")
    val y = Axioms.path("y")
    val z = Axioms.path("z")

    val xy = Axioms.pathEq(x, y)
    val yz = Axioms.pathEq(y, z)
    val xz = Axioms.pathEq(x, z)

    val vars = Seq(Axioms.string("x"), Axioms.string("y"), Axioms.string("z"))
    val paths = Seq(("x", x), ("y", y), ("z", z))
    val preprocessed = Axioms.preprocessSubstRules(vars, paths)

    val knowledge = Seq(
      Axioms.assertVariable("x"),
      Axioms.assertVariable("y"),
      Axioms.assertVariable("z"),
      Axioms.assertPath(x),
      Axioms.assertPath(y),
      Axioms.assertPath(z),
      Assert(Axioms.entails(Seq(), xy)),
      Assert(Axioms.entails(Seq(), yz)),
    )
    val assertion = Assert(Not(Axioms.entails(Seq(xy, yz), xz)))

    z3.addCommands(preprocessed ++ knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    assert(out(1).contains("C-Weak"))
    assert(out(1).contains("C-Refl"))
    assert(out(1).contains("C-Subst"))
  }

  // TODO: solver does timeout producing unknown
  // TODO: how to help the solver instantiate properly
  test("PathEq is transitive 3") {
    val x = Axioms.path("x")
    val y = Axioms.path("y")
    val z = Axioms.path("z")

    val xy = Axioms.pathEq(x, y)
    val yz = Axioms.pathEq(y, z)
    val xz = Axioms.pathEq(x, z)

    val vars = Seq(Axioms.string("x"), Axioms.string("y"), Axioms.string("z"))
    val paths = Seq(("x", x), ("y", y), ("z", z))
    val preprocessed = Axioms.preprocessSubstRules(vars, paths)

    val knowledge = Seq(
      Axioms.assertVariable("x"),
      Axioms.assertVariable("y"),
      Axioms.assertVariable("z"),
      Axioms.assertPath(x),
      Axioms.assertPath(y),
      Axioms.assertPath(z)
    )
    val assertion = Assert(Not(Axioms.entails(Seq(xy, yz), xz)))

    z3.addCommands(preprocessed ++ knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute(3*1000)
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    assert(out(1).contains("C-Subst"))
    assert(out(1).contains("C-Refl"))
    assert(out(1).contains("C-Weak"))
  }

  // integrated in above
//  test("PathEq is transitive with preprocess") {
//    val x = Axioms.path("x")
//    val y = Axioms.path("y")
//    val z = Axioms.path("z")
//
//    val xy = Axioms.pathEq(x, y)
//    val yz = Axioms.pathEq(y, z)
//    val xz = Axioms.pathEq(x, z)
//
//    val vars = Seq(Axioms.string("x"), Axioms.string("y"), Axioms.string("z"))
//    val paths = Seq(("x", x), ("y", y), ("z", z))
//
//    val knowledge = Seq(
//      Axioms.assertVariable("x"),
//      Axioms.assertVariable("y"),
//      Axioms.assertVariable("z"),
//      Axioms.assertPath(x),
//      Axioms.assertPath(y),
//      Axioms.assertPath(z)
//    )
//    val preprocessed = Axioms.preprocessSubstRules(vars, paths)
//    val assertion = Assert(Not(Axioms.entails(Seq(xy, yz), xz)))
//
//    z3.addCommands(preprocessed ++ knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
//    val (exit, out) = z3.execute()
//    z3.flush()
//
//    assert(exit == 0)
//    assert(out.size == 2)
//    assert(out.head == Unsat.format())
//    assert(out(1).contains("C-Subst"))
//    assert(out(1).contains("C-Refl"))
//    assert(out(1).contains("C-Weak"))
//  }

  test("lookup-program-entailment") {
    val x = Axioms.path("x")
    val y = Axioms.path("y")
    val xf = Axioms.path("x.f")
    val yf = Axioms.path("y.f")

    val p: Program = List(
      ConstraintEntailment(Id('x), List(InstantiatedBy(Id('x), Id('Zero))), InstanceOf(Id('x), Id('Nat))),
      ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Zero)), PathEquivalence(Id('x), FieldPath(Id('x), Id('f)))), InstanceOf(Id('x), Id('Nat)))
    )
    val vars: List[Id] = List(Id('x), Id('y))
    val lookup = SMTLibConverter.makeProgramEntailmentLookupFunction(p, vars)

    val expectedSublist1x =
      Apply("insert", Seq(
        Axioms.instanceOf(x, "Zero"),
        Apply("insert", Seq(
          Axioms.pathEq(x, xf),
          "nil"
        ))))

    val expectedSublist2x =
      Apply("insert", Seq(
        Axioms.instantiatedBy(x, "Zero"),
        "nil"
      ))

    val expectedx =
      Apply("cons", Seq(
        expectedSublist1x,
        Apply("cons", Seq(
          expectedSublist2x,
          "nan"
        ))
      ))

    val expectedSublist1y =
      Apply("insert", Seq(
        Axioms.instanceOf(y, "Zero"),
        Apply("insert", Seq(
          Axioms.pathEq(y, yf),
          "nil"
        ))))

    val expectedSublist2y =
      Apply("insert", Seq(
        Axioms.instantiatedBy(y, "Zero"),
        "nil"
      ))

    val expectedy =
      Apply("cons", Seq(
        expectedSublist1y,
        Apply("cons", Seq(
          expectedSublist2y,
          "nan"
        ))
      ))

    // Defined lookup x
    val assertion1 = Eq(
      Apply("lookup-program-entailment", Seq(Axioms.instanceOf(x, "Nat"))),
      expectedx
      )

    // Defined lookup y
    val assertion2 = Eq(
      Apply("lookup-program-entailment", Seq(Axioms.instanceOf(y, "Nat"))),
      expectedy
    )

    // Undefined lookup
    val assertion3 = Eq(
      Apply("lookup-program-entailment", Seq(Axioms.instanceOf(xf, "Foo"))),
      "nan"
    )

    val assertion = Assert(Not(And(assertion1, And(assertion2, assertion3))))

    z3.addCommands(Seq(lookup, assertion, CheckSat))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 1)
    assert(out.head == "unsat")
  }

  test("C-Prog (x :: Zero |- x :: Nat)") {
    val x = Axioms.path("x")

    val xZero = Axioms.instanceOf(x, "Zero")
    val xNat = Axioms.instanceOf(x, "Nat")

    val p: Program = List(
      ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Zero))), InstanceOf(Id('x), Id('Nat))),
      ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Succ)), InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat))), InstanceOf(Id('x), Id('Nat)))
    )
    val paths: List[Path] = List(Id('x))
    val lookup = SMTLibConverter.makeProgramEntailmentLookupFunction(p, paths)

    val preprocess = Seq(
      lookup,
      Assert(Annotate(Axioms.preprocessProgRule(), Seq(KeyValueAttribute(Keyword("named"), "C-Prog"))))
    )

    val knowledge = Seq(
      Axioms.assertVariable("x"),
      Axioms.assertPath(x),
      Axioms.assertClass("Zero"),
      Axioms.assertClass("Nat")
    )

    val assertion = Assert(Not(Axioms.entails(Apply("insert", Seq(xZero, IdentifierAs("nil", Sorts("List", Seq("Constraint"))))), xNat)))

    z3.addCommands(preprocess ++ knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == "unsat")
    assert(out(1).contains("C-Prog"))
    assert(out(1).contains("C-Ident"))
  }

  test("C-Subst (y.p :: Zero, y.p = x |- x :: Zero)") {
    val x = Axioms.path("x")
    val yp = Axioms.path("y.p")

    val xZero = Axioms.instanceOf(x, "Zero")
    val ypZero = Axioms.instanceOf(yp, "Zero")
    val ypx = Axioms.pathEq(x, yp)

    val p: Program = List(ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Zero))), InstanceOf(Id('x), Id('Nat))))

    val vars = List(Id('x))
    val paths = List(Id('x), FieldPath(Id('y), Id('p)))
    val varsTerms = vars.map(SMTLibConverter.convertId)
    val pathTerms = Seq(("x", x), ("y.p", yp))

    val lookup = SMTLibConverter.makeProgramEntailmentLookupFunction(p, paths)

    val preprocess = Axioms.preprocessSubstRules(varsTerms, pathTerms)
//    ++
//      Seq(
//        lookup,
//        Assert(Annotate(Axioms.preprocessProgRule(), Seq(KeyValueAttribute(Keyword("named"), "C-Prog"))))
//      )

    val knowledge = Seq(
      Axioms.assertVariable("x"),
      Axioms.assertPath(x),
      Axioms.assertPath(yp),
      Axioms.assertClass("Zero")
    )

    val assertion = Assert(Not(Axioms.entails(Seq(ypZero, ypx), xZero)))

    z3.addCommands(preprocess ++ knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()
  }

  test("selfmade C-Subst (x :: Zero, y.p = x |- y.p :: Zero)") {
    val x = Axioms.path("x")
    val yp = Axioms.path("y.p")
    val xZero = Axioms.instanceOf(x, "Zero")
    val ypZero = Axioms.instanceOf(yp, "Zero")
    val ypx = Axioms.pathEq(yp, x)

    val vars = Seq(Axioms.string("x"))
    val paths = Seq(("x", x), ("y.p", yp))

    val preprocessed = Seq(
      Assert(Annotate(
        Forall(Seq(
          SortedVar("a2", "Constraint"),
          SortedVar("cs", Sorts("List", Seq("Constraint")))
        ),
          Implies(
            Let(
              Seq(VarBinding("a", Apply("generalize-constraint", Seq("a2", yp, SMTLibString("x"))))),
              And(
                Apply("entails", Seq("cs", Apply("path-eq", Seq(yp, x)))),
                Apply("entails", Seq("cs", "a")) // Apply("subst-constraint", Seq("a", SMTLibString("x"), x)) // TODO: timeout with subst
              )
            ),
            Apply("entails", Seq("cs", "a2"))
          )),
        Seq(KeyValueAttribute(Keyword("named"), "foo.bar"))))
    )

    val knowledge = Seq(
      Axioms.assertVariable("x"),
      Axioms.assertPath(x),
      Axioms.assertPath(yp),
      Axioms.assertClass("Zero")
    )

    val assertion = Assert(Not(Axioms.entails(Seq(xZero, ypx), ypZero)))

    z3.addCommands(preprocessed ++ knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()
  }

  test("C-Subst (x :: Zero, y.p = x |- y.p :: Zero)") {
    val x = Axioms.path("x")
    val yp = Axioms.path("y.p")
    val xZero = Axioms.instanceOf(x, "Zero")
    val ypZero = Axioms.instanceOf(yp, "Zero")
    val ypx = Axioms.pathEq(yp, x)

    val vars = Seq(Axioms.string("x"))
    val paths = Seq(("x", x), ("y.p", yp))

    val preprocessed = Axioms.preprocessSubstRules(vars, paths)

    val knowledge = Seq(
      Axioms.assertVariable("x"),
      Axioms.assertPath(x),
      Axioms.assertPath(yp),
      Axioms.assertClass("Zero")
    )

    val assertion = Assert(Not(Axioms.entails(Seq(xZero, ypx), ypZero)))

    z3.addCommands(preprocessed ++ knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()
  }

  test("C-Prog (x :: Zero, y.p = x |- y.p :: Nat)") {
    val x = Axioms.path("x")
    val y = Axioms.path("y")
    val yp = Axioms.path("y.p")

    val xZero = Axioms.instanceOf(x, "Zero")
//    val ySucc = Axioms.instanceOf(y, "Succ")
    val ypNat = Axioms.instanceOf(yp, "Nat")
    val ypx = Axioms.pathEq(yp, x)

    val p: Program = List(ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Zero))), InstanceOf(Id('x), Id('Nat))))

    val vars = List(Id('x), Id('y))
    val paths = List(Id('x), Id('y), FieldPath(Id('y), Id('p)))
    val varsTerms = vars.map(SMTLibConverter.convertId)
    val pathTerms = Seq(("x", x), ("y", y), ("y.p", yp))

    val lookup = SMTLibConverter.makeProgramEntailmentLookupFunction(p, paths)

    val preprocess = Seq(
      lookup,
      Assert(Annotate(Axioms.preprocessProgRule(), Seq(KeyValueAttribute(Keyword("named"), "C-Prog"))))
    ) ++ Axioms.preprocessSubstRules(varsTerms, pathTerms)

    val knowledge = Seq(
      Axioms.assertVariable("x"),
      Axioms.assertVariable("y"),
      Axioms.assertPath(x),
      Axioms.assertPath(y),
      Axioms.assertPath(yp),
      Axioms.assertClass("Zero"),
//      Axioms.assertClass("Succ"),
      Axioms.assertClass("Nat"),
      Assert(Axioms.entails(Seq(), Axioms.instanceOf(yp, "Zero")))
    )

    val assertion = Assert(Not(Axioms.entails(Seq(xZero, ypx), ypNat)))
//    val assertion = Assert(Not( // unsat
//      Eq(
//        Apply("lookup-program-entailment", Seq(ypNat)),
//        Apply("cons", Seq(
//          Apply("insert", Seq(
//            Axioms.instanceOf(yp, "Zero"),
//            "nil"
//          )),
//          "nan"
//        ))
//      )
//    ))
//    val assertion = Assert(Not(Axioms.entails(Seq(xZero, ypx), Axioms.instanceOf(yp, "Zero")))) // needs subst in context

    z3.addCommands(preprocess ++ knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()
  }

  // TODO: check this test
  // TODO: why y.p :: Nat and not y :: Nat?
  // TODO: add subst rules
  test("C-Prog (x :: Zero, y :: Succ, y.p = x |- y :: Nat)") {
    val p: Program = List(
      ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Zero))), InstanceOf(Id('x), Id('Nat))),
      ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Succ)), InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat))), InstanceOf(Id('x), Id('Nat)))
    )
    val vars: List[Id] = List(Id('x1), Id('x2))
    val paths: List[Path] = List(Id('x1), Id('x2), FieldPath(Id('x2), Id('p)))
    val lookup = SMTLibConverter.makeProgramEntailmentLookupFunction(p, vars)

    val x1 = Axioms.path("x1")
    val x2 = Axioms.path("x2")
    val x2P = Axioms.path("x2.p")

    val x1Zero = Axioms.instanceOf(x1, "Zero")
    val x2Succ = Axioms.instanceOf(x2, "Succ")
    val x2Px1 = Axioms.pathEq(x2P, x1)
    val x2Nat = Axioms.instanceOf(x2, "Nat")
    val x2PNat = Axioms.instanceOf(x2P, "Nat")

    val pathsTerms = Seq(("x1", x1), ("x2", x2), ("x2.p", x2P))

    // Aux
    val x1NatInst = Axioms.instanceOf(x1, "Nat")
    val x1ZeroInst = Axioms.instanceOf(x1, "Zero")
    val x1SuccInst = Axioms.instanceOf(x1, "Succ")
    val x2NatInst = Axioms.instanceOf(x2, "Nat")
    val x2ZeroInst = Axioms.instanceOf(x2, "Zero")
    val x2SuccInst = Axioms.instanceOf(x2, "Succ")
    val x1PNatInst = Axioms.instanceOf(Axioms.path("x1.p"), "Nat")

    val knowledge = Seq(
      Axioms.assertVariable("x1"),
      Axioms.assertVariable("x2"),
      Axioms.assertPath(x1),
      Axioms.assertPath(x2),
      Axioms.assertPath(x2P),
      Axioms.assertClass("Zero"),
      Axioms.assertClass("Succ"),
      Axioms.assertClass("Nat"),
//      Axioms.assertInProg("x1", Seq(x1ZeroInst), x1NatInst),
//      Axioms.assertInProg("x1", Seq(x1SuccInst, x1PNatInst), x1NatInst),
//      Axioms.assertInProg("x2", Seq(x2ZeroInst), x2NatInst),
//      Axioms.assertInProg("x2", Seq(x2SuccInst, x2PNat), x2NatInst)
      Assert(Axioms.entails(Seq(), Axioms.instanceOf(x2P, "Zero")))
    )

    val preprocessed = Axioms.preprocessSubstRules(vars.map(x => SMTLibConverter.convertId(x)), pathsTerms) ++
      Seq(
        lookup,
        Assert(Annotate(Axioms.preprocessProgRule(), Seq(KeyValueAttribute(Keyword("named"), "C-Prog"))))
      )
    val assertion = Assert(Not(Axioms.entails(Seq(x1Zero, x2Succ, x2Px1), x2Nat)))
//    val assertion = Assert(Not(Axioms.entails(Seq(x1Zero, x2Succ, x2Px1), Axioms.pathEq(x2P, x2)))) // -> unsat. WTF?

    z3.addCommands(preprocessed ++ knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()
  }
}
