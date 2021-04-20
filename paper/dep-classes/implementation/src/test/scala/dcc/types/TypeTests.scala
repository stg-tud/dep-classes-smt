package dcc.types

import dcc.Util.substitute
import org.scalatest.Assertions.fail

object TypeTests {
  def testTypeOk(actual: Either[Type, String], expected: Type): Unit = actual match {
    case Left(Type(x, a)) =>
      val Type(y, expectedConstraints) = expected
      val b = substitute(y, x, expectedConstraints)

      b.find(!a.contains(_)) match {
        case None => // succeed
        case Some(constraint) => fail(s"Expected constraint '$constraint' does not exist in ${Type(x, a)}")
      }

      a.find(!b.contains(_)) match {
        case None => // succeed
        case Some(constraint) => fail(s"Actual result type contains unexpected constraint '$constraint'")
      }
    case Right(error) => fail(s"Expected Type, but got error '$error'")
  }

  def testTypeError(actual: Either[Type, String]): Unit = actual match {
    case Left(t) => fail(s"Expected error response, but got type $t")
    case Right(_) => // succeed
  }

  def testTypeError(actual: Either[Type, String], expected: String): Unit = actual match {
    case Left(t) => fail(s"Expected error response, but got type $t")
    case Right(error) => if (!error.contains(expected)) fail(s"Actual error\n\t$error\ndid not match expected error\n\t$expected")
  }
}
