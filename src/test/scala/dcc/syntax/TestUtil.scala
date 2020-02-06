package dcc.syntax

import org.scalatest.FunSuite

class TestUtil extends FunSuite {
  import dcc.Util._

  val x: Id = Id('x)
  val y: Id = Id('y)
  val z: Id = Id('z)

  val xf: Path = FieldPath(x, Id('f))
  val yf: Path = FieldPath(y, Id('f))
  val zf: Path = FieldPath(z, Id('f))
  val yff: Path = FieldPath(yf, Id('f))

  test("renameIdInPath") {
    assert(x == renameIdInPath(x, x, x))
    assert(y == renameIdInPath(x, y, x))
    assert(z == renameIdInPath(x, y, z))
    assert(yf == renameIdInPath(x, y, xf))
  }

  test("renameIdInConstraint") {
    assert(PathEquivalence(yf, y) == renameIdInConstraint(x, y, PathEquivalence(xf, x)))
    assert(PathEquivalence(yff, z) == renameIdInConstraint(x, z, PathEquivalence(yff, x)))
    assert(InstanceOf(yf, Id('cls)) == renameIdInConstraint(x, y, InstanceOf(xf, Id('cls))))
    assert(InstantiatedBy(z, Id('cls)) == renameIdInConstraint(y, z, InstantiatedBy(y, Id('cls))))
  }

  test("alphaConversion") {
    val cs = alphaConversion(x, z, List(
      PathEquivalence(xf, x),
      PathEquivalence(yff, x),
      InstanceOf(xf, Id('cls)),
      InstantiatedBy(y, Id('cls))
    ))

    assert(cs.size == 4)
    assert(cs.contains(PathEquivalence(zf, z)))
    assert(cs.contains(PathEquivalence(yff, z)))
    assert(cs.contains(InstanceOf(zf, Id('cls))))
    assert(cs.contains(InstantiatedBy(y, Id('cls))))
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
    assert(InstanceOf(zf, Id('cls)) == substitute(y, zf, InstanceOf(y, Id('cls))))
    assert(InstantiatedBy(z, Id('cls)) == substitute(y, z, InstantiatedBy(y, Id('cls))))
  }

  test("substitute constraints") {
    val cs = substitute(x, yf, List(
      PathEquivalence(xf, x),
      PathEquivalence(yff, x),
      InstanceOf(xf, Id('cls)),
      InstantiatedBy(y, Id('cls))
    ))

    assert(cs.size == 4)
    assert(cs.contains(PathEquivalence(yff, yf)))
    assert(cs.contains(PathEquivalence(yff, yf)))
    assert(cs.contains(InstanceOf(yff, Id('cls))))
    assert(cs.contains(InstantiatedBy(y, Id('cls))))
  }
}
