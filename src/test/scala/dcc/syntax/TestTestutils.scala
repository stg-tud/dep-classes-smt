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
    for (i <- 1 until 100) {
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
}
