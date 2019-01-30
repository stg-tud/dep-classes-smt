package dcc.syntax

import dcc.syntax.Program.Program
import org.scalatest.FunSuite
import smtlib.syntax.{Apply, SMTLibString, SimpleSymbol}

class TestSMTLibConversion extends FunSuite {
  val p: Path = Id('x)
  val q: Path = FieldPath(Id('x), Id('f))
  val r: Path = FieldPath(FieldPath(Id('x), Id('f)), Id('f1))

  val pSMTLib = Apply(SimpleSymbol("var"), Seq(SMTLibString("x")))
  val qSMTLib = Apply(
                  SimpleSymbol("pth"),
                  Seq(
                    Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))),
                    SMTLibString("f")
                  ))
  val rSMTLib = Apply(
                  SimpleSymbol("pth"),
                  Seq(
                    Apply(SimpleSymbol("pth"), Seq(
                      Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))),
                      SMTLibString("f")
                    )),
                    SMTLibString("f1")
                  ))

  test("Convert Path") {
    assert(SMTLibConverter.convertPath(p) == pSMTLib)
    assert(SMTLibConverter.convertPath(q) == qSMTLib )
    assert(SMTLibConverter.convertPath(r) == rSMTLib)
  }

  val eq = PathEquivalence(p, q)
  val of = InstanceOf(p, Id('Cls))
  val by = InstantiatedBy(q, Id('Cls))

  val eqSMTLib = Apply(SimpleSymbol("path-eq"), Seq(pSMTLib, qSMTLib))
  val ofSMTLib = Apply(SimpleSymbol("instance-of"), Seq(pSMTLib, SMTLibString("Cls")))
  val bySMTLib = Apply(SimpleSymbol("instantiated-by"), Seq(qSMTLib, SMTLibString("Cls")))

  test("Convert Constraints") {
    assert(SMTLibConverter.convertConstraint(eq) == eqSMTLib)
    assert(SMTLibConverter.convertConstraint(of) == ofSMTLib)
    assert(SMTLibConverter.convertConstraint(by) == bySMTLib)
  }

  val naturalNumbers: Program = List(
    ConstructorDeclaration(Id('Zero), Id('x), Nil),
    ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Zero))), InstanceOf(Id('x), Id('Nat))),
    ConstructorDeclaration(Id('Succ), Id('x), List(InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat)))),
    ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Succ)), InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat))), InstanceOf(Id('x), Id('Nat))),
    AbstractMethodDeclaration(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Nat))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat))))),
    MethodImplementation(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Zero))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat)))),
      ObjectConstruction(Id('Zero), Nil)),
    MethodImplementation(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Succ)), InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat)))),
      FieldAccess(Id('x), Id('p)))
  )

  val zeroEntailment = Apply(
                        SimpleSymbol("in-program"),
                        Seq(
                          SMTLibString("x"),
                          Apply(SimpleSymbol("insert"), Seq(
                            Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))), SMTLibString("Zero"))),
                            SimpleSymbol("nil")
                          )),
                          Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))), SMTLibString("Nat")))
                        ))

  val succEntailment = Apply(
                        SimpleSymbol("in-program"),
                        Seq(
                          SMTLibString("x"),
                          Apply(SimpleSymbol("insert"), Seq(
                            Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))), SMTLibString("Succ"))),
                            Apply(SimpleSymbol("insert"), Seq(
                              Apply(SimpleSymbol("instance-of"), Seq(
                                Apply(SimpleSymbol("pth"), Seq(
                                  Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))),
                                  SMTLibString("p")
                                )),
                                SMTLibString("Nat"))),
                              SimpleSymbol("nil")
                            ))
                          )),
                          Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))), SMTLibString("Nat")))
                        ))

  test("Convert Program Entailments") {
    val entailments = SMTLibConverter.convertProgramEntailments(naturalNumbers)

    assert(entailments.size == 2)
    assert(entailments.head == zeroEntailment)
    assert(entailments.last == succEntailment)
  }

  test("Convert Entailment") {
    val ctx = List(by, eq)
    val entailment = Apply(SimpleSymbol("entails"), Seq(
      Apply(SimpleSymbol("insert"), Seq(
        bySMTLib,
        Apply(SimpleSymbol("insert"), Seq(
          eqSMTLib,
          SimpleSymbol("nil")
        ))
      )),
      ofSMTLib
    ))

    assert(SMTLibConverter.convertEntailment(ctx, of) == entailment)
  }

  test("Convert Variables") {
    val cs = List(of, by, eq, PathEquivalence(Id('y), Id('z)), PathEquivalence(FieldPath(Id('m), Id('h)), Id('y)))
    val vars = List(
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("m"))),
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("y"))),
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("z"))),
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("x")))
    )

    assert(SMTLibConverter.convertVariables(cs) == vars)
  }

  test("Convert Variables Paths Classes") {
    val x: Path = Id('x)
    val y: Path = Id('y)
    val z: Path = Id('z)
    val pth: Path = FieldPath(Id('x), Id('f))

    val xSMTLib = Apply(SimpleSymbol("var"), Seq(SMTLibString("x")))
    val ySMTLib = Apply(SimpleSymbol("var"), Seq(SMTLibString("y")))
    val zSMTLib = Apply(SimpleSymbol("var"), Seq(SMTLibString("z")))
    val pthSMTLib = Apply(SimpleSymbol("pth"), Seq(
      Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))),
      SMTLibString("f")
    ))

    val cs = List(
      PathEquivalence(z, pth),
      PathEquivalence(x, y),
      InstanceOf(x, Id('Cls1)),
      InstantiatedBy(y, Id('Cls1)),
      InstantiatedBy(z, Id('Cls2))
    )

    val (vars, paths, classes) = SMTLibConverter.convertVariablesPathsClasses(cs)

    val expectedVars = List(
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("y"))),
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("x"))),
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("z"))),
    )

    val expectedPaths = List(
      Apply(SimpleSymbol("path-exists"), Seq(ySMTLib)),
      Apply(SimpleSymbol("path-exists"), Seq(xSMTLib)),
      Apply(SimpleSymbol("path-exists"), Seq(pthSMTLib)),
      Apply(SimpleSymbol("path-exists"), Seq(zSMTLib)),
    )

    val expectedClasses = List(
      Apply(SimpleSymbol("class"), Seq(SMTLibString("Cls2"))),
      Apply(SimpleSymbol("class"), Seq(SMTLibString("Cls1"))),
    )

    assert(vars == expectedVars)
    assert(paths == expectedPaths)
    assert(classes == expectedClasses)
  }
}
