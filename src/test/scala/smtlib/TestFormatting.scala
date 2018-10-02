package smtlib

import org.scalatest.FunSuite
import syntax._
import syntax.Implicit._

class TestFormatting extends FunSuite{
  test("Sort") {
    assert(Sort("Bool").format() == "Bool")
    assert(Sort("Int").format() == "Int")
    assert(Sort("String").format() == "String")

    val array = Sort("Array", Seq(Sort("Int"), Sort("Real")))
    assert(array.format() == "(Array Int Real)")

    val listArray = Sort("List", Seq(array))
    assert(listArray.format() == "(List (Array Int Real))")

    val bitVec = Sort("_", Seq(Sort("BitVec"), Sort("3")))
    assert(bitVec.format() == "(_ BitVec 3)")
  }

  test("Term.QualifiedIdentifier") {
    assert(QualifiedIdentifier("x").format() == "x")
    assert(QualifiedIdentifier("x", Sort("Bool")).format() == "(as x Bool)")
  }

  test("Term.Application") {
    assert(Apply(QualifiedIdentifier("append"), Seq(QualifiedIdentifier("x"), QualifiedIdentifier("y"))).format() == "(append x y)")
  }

  val binding1 = VarBinding("h", Apply(QualifiedIdentifier("head"), Seq(QualifiedIdentifier("x"))))
  val binding2 = VarBinding("t", Apply(QualifiedIdentifier("tail"), Seq(QualifiedIdentifier("x"))))
  val let = Let(Seq(binding1, binding2),
    Apply(QualifiedIdentifier("insert"),
      Seq(QualifiedIdentifier("h"),
        Apply(QualifiedIdentifier("append"),
          Seq(QualifiedIdentifier("t"), QualifiedIdentifier("y"))))))

  test("Term.Let") {
    assert(binding1.format() == "(h (head x))")
    assert(binding2.format() == "(t (tail x))")
    assert(let.format() == "(let ((h (head x)) (t (tail x))) (insert h (append t y)))")
  }

  val var1 = SortedVar("x", Sort("List", Seq(Sort("Int"))))
  val var2 = SortedVar("y", Sort("List", Seq(Sort("Int"))))

  test("Term.Forall") {
    assert(var1.format() == "(x (List Int))")
    assert(var2.format() == "(y (List Int))")

    val forall = Forall(Seq(var1, var2), let)
    assert(forall.format() == "(forall ((x (List Int)) (y (List Int))) (let ((h (head x)) (t (tail x))) (insert h (append t y))))")
  }

  test("Term.Exists") {
    val exists = Exists(Seq(var1, var2), let)
    assert(exists.format() == "(exists ((x (List Int)) (y (List Int))) (let ((h (head x)) (t (tail x))) (insert h (append t y))))")
  }
}
