package smtlib

import org.scalatest.FunSuite
import smtlib.solver.{Axioms, Z3Solver}
import smtlib.syntax._
import smtlib.syntax.Implicit._

class TestAxioms extends FunSuite {
  val options: Seq[SMTLibCommand] = Seq(
    SetOption(KeyValueAttribute(Keyword("smt.mbqi"), SimpleSymbol("true"))),
    SetOption(KeyValueAttribute(Keyword("model.compact"), SimpleSymbol("true"))),
    SetOption(KeyValueAttribute(Keyword("smt.qi.max_multi_patterns"), Numeral(4))),
    SetOption(KeyValueAttribute(Keyword("pi.max_multi_patterns"), Numeral(4))),
//    SetOption(KeyValueAttribute(Keyword("trace"), SimpleSymbol("true"))),
    SetOption(ProduceProofs(true)),
    SetOption(ProduceUnsatCores(true)))
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
    assert(out(1) == "(C-Weak C-Refl)")
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

  test("C-Weak") {
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
    assert(out(1) == "(C-Weak C-Ident)")
  }

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
    assert(out(1) == "(C-Weak C-Ident)")
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
    assert(out(1) == "(C-Perm)")
  }

  test("PathEq is symmetric 1") {
    val x = Axioms.path("x")
    val y = Axioms.path("y")
    val xy = Axioms.pathEq(x, y)
    val yx = Axioms.pathEq(y, x)

    val knowledge = Seq(
      Assert(Axioms.variable("x")),
      Assert(Axioms.variable("y")),
      Assert(Axioms.pathExists(x)),
      Assert(Axioms.pathExists(y)),
      Assert(Axioms.entails(Seq(), xy))
    )
    val assertion = Assert(Not(Axioms.entails(Seq(), yx)))

    z3.addCommands(knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute(2500)
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

    val knowledge = Seq(
      Axioms.assertVariable("x"),
      Axioms.assertVariable("y"),
      Axioms.assertPath(x),
      Axioms.assertPath(y)
    )
    val assertion = Assert(Not(Axioms.entails(Seq(xy), yx)))

    z3.addCommands(knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    assert(out(1).contains("C-Ident"))
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

    z3.addCommands(knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    assert(out(1).contains("C-Refl"))
    assert(out(1).contains("C-Subst"))
  }

  test("PathEq is transitive 2") {
    val x = Axioms.path("x")
    val y = Axioms.path("y")
    val z = Axioms.path("z")

    val xy = Axioms.pathEq(x, y)
    val yz = Axioms.pathEq(y, z)
    val xz = Axioms.pathEq(x, z)

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

    z3.addCommands(knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
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

    val knowledge = Seq(
      Axioms.assertVariable("x"),
      Axioms.assertVariable("y"),
      Axioms.assertVariable("z"),
      Axioms.assertPath(x),
      Axioms.assertPath(y),
      Axioms.assertPath(z)
    )
    val assertion = Assert(Not(Axioms.entails(Seq(xy, yz), xz)))

    z3.addCommands(knowledge ++ Seq(assertion, CheckSat/*, GetUnsatCore*/))
    val (exit, out) = z3.execute(10*1000)
    z3.flush()

//    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    assert(out(1).contains("C-Subst"))
  }
}
