package dcc.syntax

import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.util.Random

class TestTestutils extends FunSuite {
  def pathDepth(p: Path): Int = p match {
    case Id(_) => 1
    case FieldPath(q, _) => 1 + pathDepth(q)
  }

  def pathVar(p: Path): Id = p match {
    case x@Id(_) => x
    case FieldPath(q, _) => pathVar(q)
  }

  private def assertConstraintExact(c: Constraint, metadata: (List[Id], List[Path], List[Id])): org.scalatest.Assertion = {
    val (vars, paths, classes) = metadata

    c match {
      case PathEquivalence(p, q) if p == q =>
        assert(vars == List(pathVar(p)))
        assert(paths == List(p))
        assert(classes == Nil)
      case PathEquivalence(p, q) =>
        assert(vars.size == 2)
        assert(vars.contains(pathVar(p)))
        assert(vars.contains(pathVar(q)))

        assert(paths.size == 2)
        assert(paths.contains(p))
        assert(paths.contains(q))

        assert(classes == Nil)
      case InstanceOf(p, cls) =>
        assert(vars == List(pathVar(p)))
        assert(paths == List(p))
        assert(classes == List(cls))
      case InstantiatedBy(p, cls) =>
        assert(vars == List(pathVar(p)))
        assert(paths == List(p))
        assert(classes == List(cls))
      case _ => fail("unsupported constraint")
    }
  }

  private def assertConstraintLowerBound(c: Constraint, metadata: (List[Id], List[Path], List[Id])): org.scalatest.Assertion = {
    val (vars, paths, classes) = metadata

    c match {
      case PathEquivalence(p, q) =>
        assert(vars.contains(pathVar(p)))
        assert(vars.contains(pathVar(q)))

        assert(paths.contains(p))
        assert(paths.contains(q))
      case InstanceOf(p, cls) =>
        assert(vars.contains(pathVar(p)))
        assert(paths.contains(p))
        assert(classes.contains(cls))
      case InstantiatedBy(p, cls) =>
        assert(vars.contains(pathVar(p)))
        assert(paths.contains(p))
        assert(classes.contains(cls))
      case _ => fail("unsupported constraint")
    }
  }

  test("generateRandomId: static prefix") {
    val prefix = "foobar"
    val id = Testutils.generateRandomId(prefix)

    assert(id.toString.startsWith(prefix))
  }

  test("generateRandomId: random prefix") {
    for (_ <- 0 to 1000) {
      val prefix = Random.nextString(100)
      val id = Testutils.generateRandomId(prefix)

      assert(id.toString.startsWith(prefix))
    }
  }

  test("generateRandomId: default prefix") {
    for (_ <- 0 to 10) {
      val id = Testutils.generateRandomId()

      assert(id.toString.startsWith("var_"))
    }
  }

  test("generateRandomId: ends with 3 digits") {
    for (_ <- 0 to 1000) {
      val id = Testutils.generateRandomId().toString

      noException should be thrownBy id.splitAt(id.length-3)._2.toInt
    }
  }

  test("generateRandomPath: default depth") {
    for (p <- (0 to 1000).toList.map(_ => Testutils.generateRandomPath())) {
      assert(pathDepth(p) <= 2)
    }
  }

  test("generateRandomPath: depth = 0") {
    for (p <- (0 to 1000).toList.map(_ => Testutils.generateRandomPath(0))) {
      assert(pathDepth(p) == 1)
    }
  }

  test("generateRandomPath: negative depth") {
    for (i <- -100 to 0) {
      for (p <- (0 to 100).toList.map(_ => Testutils.generateRandomPath(i))) {
        assert(pathDepth(p) == 1)
      }
    }
  }

  test("generateRandomPath: dynamic depth") {
    for (i <- 1 to 100) {
      for (p <- (0 to 100).toList.map(_ => Testutils.generateRandomPath(i))) {
        assert(pathDepth(p) <= i)
      }
    }
  }

  test("generateRandomPath: known var") {
    for (_ <- 0 to 100) {
      val x: Id = Testutils.generateRandomId()
      for (p <- (0 to 100).toList.map(_ => Testutils.generateRandomPath(x = Option(x)))) {
        assert(pathVar(p) == x)
      }
    }
  }

  test("generateRandomConstraints: default amount") {
    assert(Testutils.generateRandomConstraints().size == 10)
  }

  test("generateRandomConstraints: negative amount") {
    for (i <- -100 to 0) {
      assert(Testutils.generateRandomConstraints(i) == Nil)
    }
  }

  test("generateRandomConstraints: specific amount") {
    for(i <- 0 to 100) {
      assert(Testutils.generateRandomConstraints(i).size == i)
    }
  }

  test("generateRandomConstraintWithMetadata") {
    for (_ <- 1 to 100) {
      val (c, metadata) = Testutils.generateRandomConstraintWithMetadata()
      assertConstraintExact(c, metadata)
    }
  }

  test("generateRandomConstraintsWithMetadata: default amount") {
    val (cs, metadata) = Testutils.generateRandomConstraintsWithMetadata()
    assert(cs.size == 10)
    for (c <- cs) {
      assertConstraintLowerBound(c, metadata)
    }
  }

  test("generateRandomConstraintsWithMetadata: specific amount") {
    for(i <- 0 to 100) {
      val (cs, metadata) = Testutils.generateRandomConstraintsWithMetadata(i)
      assert(cs.size == i)
      for (c <- cs) {
        assertConstraintLowerBound(c, metadata)
      }
    }
  }
}
