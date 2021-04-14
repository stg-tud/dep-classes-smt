package dcc.syntax

import org.scalatest.funsuite.AnyFunSuite

class TestUtil extends AnyFunSuite {
  import dcc.Util._

  val x: Id = Id(Symbol("x"))
  val y: Id = Id(Symbol("y"))
  val z: Id = Id(Symbol("z"))

  val xf: Path = FieldPath(x, Id(Symbol("f")))
  val yf: Path = FieldPath(y, Id(Symbol("f")))
  val zf: Path = FieldPath(z, Id(Symbol("f")))
  val yff: Path = FieldPath(yf, Id(Symbol("f")))

  test("renameIdInPath") {
    assert(x == renameIdInPath(x, x, x))
    assert(y == renameIdInPath(x, y, x))
    assert(z == renameIdInPath(x, y, z))
    assert(yf == renameIdInPath(x, y, xf))
  }

  test("renameIdInConstraint") {
    assert(PathEquivalence(yf, y) == renameIdInConstraint(x, y, PathEquivalence(xf, x)))
    assert(PathEquivalence(yff, z) == renameIdInConstraint(x, z, PathEquivalence(yff, x)))
    assert(InstanceOf(yf, Id(Symbol("cls"))) == renameIdInConstraint(x, y, InstanceOf(xf, Id( Symbol("cls")))))
    assert(InstantiatedBy(z, Id(Symbol("cls"))) == renameIdInConstraint(y, z, InstantiatedBy(y, Id(Symbol("cls")))))
  }

  test("alphaConversion") {
    val cs = alphaConversion(x, z, List(
      PathEquivalence(xf, x),
      PathEquivalence(yff, x),
      InstanceOf(xf, Id(Symbol("cls"))),
      InstantiatedBy(y, Id(Symbol("cls")))
    ))

    assert(cs.size == 4)
    assert(cs.contains(PathEquivalence(zf, z)))
    assert(cs.contains(PathEquivalence(yff, z)))
    assert(cs.contains(InstanceOf(zf, Id(Symbol("cls")))))
    assert(cs.contains(InstantiatedBy(y, Id(Symbol("cls")))))
  }

  test("substitute path") {
    assert(x == substitute(x, x, x))
    assert(x == substitute(y, x, y))
    assert(z == substitute(x, y, z))
    assert(yf == substitute(x, y, xf))
    assert(yff == substitute(x, yf, xf))
  }

  test("substitute constraint") {
    assert(PathEquivalence(yf, y) == substitute(x, y, PathEquivalence(xf, x)))
    assert(PathEquivalence(yff, yf) == substitute(x, yf, PathEquivalence(xf, x)))
    assert(InstanceOf(zf, Id(Symbol("cls"))) == substitute(y, zf, InstanceOf(y, Id(Symbol("cls")))))
    assert(InstantiatedBy(z, Id(Symbol("cls"))) == substitute(y, z, InstantiatedBy(y, Id(Symbol("cls")))))
  }

  test("substitute constraints") {
    val cs = substitute(x, yf, List(
      PathEquivalence(xf, x),
      PathEquivalence(yff, x),
      InstanceOf(xf, Id(Symbol("cls"))),
      InstantiatedBy(y, Id(Symbol("cls")))
    ))

    assert(cs.size == 4)
    assert(cs.contains(PathEquivalence(yff, yf)))
    assert(cs.contains(PathEquivalence(yff, yf)))
    assert(cs.contains(InstanceOf(yff, Id(Symbol("cls")))))
    assert(cs.contains(InstantiatedBy(y, Id(Symbol("cls")))))
  }
}