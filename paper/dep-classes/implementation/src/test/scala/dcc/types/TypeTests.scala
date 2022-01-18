package dcc.types

import dcc.Util.substitute
import org.scalatest.Assertion
import org.scalatest.Assertions.{fail, succeed}

object TypeTests {
  def testTypeOk(actual: Either[Type, List[TError]], expected: Type): Assertion = actual match {
    case Left(Type(x, a)) =>
      val Type(y, expectedConstraints) = expected
      val b = substitute(y, x, expectedConstraints)

      val didExpectThoseToExists = b.diff(a)

      if (didExpectThoseToExists.nonEmpty) {
        fail(s"Expected constraints $didExpectThoseToExists do not exists in type ${Type(x, a)}")
      }
      else {
        // nobody expects the spanish inquisition
        val didNotExpectThose = a.diff(b)

        if (didNotExpectThose.nonEmpty)
          fail(s"Type ${Type(x, a)} contains unexpected constraints $didNotExpectThose")
        else
          succeed
      }

//      b.find(!a.contains(_)) match {
//        case None => // succeed
//        case Some(constraint) => fail(s"Expected constraint '$constraint' does not exist in type ${Type(x, a)}")
//      }
//
//      a.find(!b.contains(_)) match {
//        case None => // succeed
//        case Some(constraint) => fail(s"Type ${Type(x, a)} contains unexpected constraint '$constraint'")
//      }
    case Right(error) => fail(s"Expected type $expected, but got error '$error'")
  }

  def testTypeError(actual: Either[Type, List[TError]]): Assertion = actual match {
    case Left(t) => fail(s"Expected error response, but got type $t")
    case Right(_) => succeed
  }

  def testTypeError(actual: Either[Type, List[TError]], expected: List[TError]): Assertion = actual match {
    case Left(t) => fail(s"Expected error '$expected', but got type $t")
    case Right(error) =>
      if (error.size == expected.size && error.forall(expected.contains(_)))
        fail(s"Actual error\n\t$error\ndid not match expected error\n\t$expected")
      else
        succeed
  }
}
