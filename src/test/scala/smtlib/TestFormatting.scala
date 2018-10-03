package smtlib

import org.scalatest.FunSuite
import syntax._
import syntax.Implicit._

class TestFormatting extends FunSuite{
  test("Lexicon") {
    val num1 = Numeral(0)
    val num2 = Numeral(1256432)
    val dec1 = Decimal(123.456)
    val dec2 = Decimal(123.0123)
    val hex1 = Hexadecimal(0x0a1b54ff)
    val hex2 = Hexadecimal("0a1b54ff")
    val hex3 = Hexadecimal(0x1a2b)
    val hex4 = Hexadecimal("1a2b")
    val bin1 = Binary(Integer.parseInt("00111100", 2))
    val bin2 = Binary("00111100")
    val bin3 = Binary(0xff)
    val bin4 = Binary("11111111")
    val str1 = StringLiteral("foo")
    val str2 = StringLiteral("bar")

    assert(num1.format() == "0")
    assert(num2.format() == "1256432")
    assert(dec1.format() == "123.456")
    assert(dec2.format() == "123.0123")
    assert(hex1.format() == "#xA1B54FF")
    assert(hex2.format() == "#x0A1B54FF")
    assert(hex3.format() == "#x1A2B")
    assert(hex4.format() == "#x1A2B")
    assert(bin1.format() == "#b111100")
    assert(bin2.format() == "#b00111100")
    assert(bin3.format() == "#b11111111")
    assert(bin4.format() == "#b11111111")
    assert(str1.format() == "foo")
    assert(str2.format() == "bar")
  }

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
