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

  test("C-Refl 2") { // TODO: should this hold?
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
    assert(out(1) == "(C-Refl C-Weak)")
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
    assert(out(1) == "(C-Ident C-Weak)")
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
    assert(out(1) == "(C-Ident C-Weak)")
  }

  test("PathEq is symmetric 1") { // TODO: same as transitive
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

    z3.addCommands(knowledge ++ Seq(assertion, CheckSat))
    val (exit, out) = z3.execute()
    z3.flush()
  }

  test("PathEq is symmetric 2") { // TODO: same as transitive
    val x = Axioms.path("x")
    val y = Axioms.path("y")
    val xy = Axioms.pathEq(x, y)
    val yx = Axioms.pathEq(y, x)

    val knowledge = Seq(
      Assert(Axioms.variable("x")),
      Assert(Axioms.variable("y")),
      Assert(Axioms.pathExists(x)),
      Assert(Axioms.pathExists(y))
    )
    val assertion = Assert(Not(Axioms.entails(Seq(xy), yx)))

    z3.addCommands(knowledge ++ Seq(assertion, CheckSat))
    val (exit, out) = z3.execute(5000)
    z3.flush()
  }

  test("PathEq is transitive") { // TODO: solver does timeout producing unknown entailment false or solver unable to instantiate "clever"?
    val x = Axioms.path("x")
    val y = Axioms.path("y")
    val z = Axioms.path("z")

    val xy = Axioms.pathEq(x, y)
    val yz = Axioms.pathEq(y, z)
    val xz = Axioms.pathEq(x, z)

    val assertion = Assert(Not(Axioms.entails(Seq(xy, yz), xz)))

    z3.addCommands(Seq(assertion, CheckSat, GetUnsatCore))
    val (exit, out) = z3.execute()
    z3.flush()

    assert(exit == 0)
    assert(out.size == 2)
    assert(out.head == Unsat.format())
    //assert(out(1) == "(C-Refl)")
  }
}
