package dcc.syntax

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
}
