package dcc.types

import dcc.entailment.EntailmentSort
import dcc.program.NaturalNumbers
import dcc.syntax.{FieldAccess, InstanceOf, MethodCall, ObjectConstruction, Path, PathEquivalence}
import dcc.syntax.Implicit.StringToId
import org.scalatest.funsuite.AnyFunSuite

class TestTypeCheckExpressions extends AnyFunSuite {
  private def compareStringWithHole(compare: String, pattern: String, hole: Char = '_'): Boolean = {
    val toMatch = pattern.split(hole).toList

    val holeWidth = ((compare.length - toMatch.map(_.length).sum) / toMatch.size) + 1

    for (i <- toMatch.indices) {
      val sliceStart: Int = (0 until i).map(toMatch(_).length).sum
      val slice = compare.slice(sliceStart+i*holeWidth, sliceStart+i*holeWidth+toMatch(i).length)

      if(slice != toMatch(i))
        return false
    }

    true
  }

  test ("test string comparison helper") {
    val s1 = "this is a $ string with $ char $ holes in it"
    val s2 = "this is a $x string with $x char $x holes in it"
    val s3 = "this is a $x1 string with $x1 char $x1 holes in it"

    val pattern1 = "this is a _ string with _ char _ holes in it"
    val pattern2 = "this is a ? string with ? char ? holes in it"
    val pattern3 = "this is a $ string with $ char $ holes in it"

    compareStringWithHole(s1, pattern1)
    compareStringWithHole(s2, pattern1)
    compareStringWithHole(s3, pattern1)

    compareStringWithHole(s1, pattern2, '?')
    compareStringWithHole(s2, pattern2, '?')
    compareStringWithHole(s3, pattern2, '?')

    compareStringWithHole(s1, pattern3, '$')
    compareStringWithHole(s2, pattern3, '$')
    compareStringWithHole(s3, pattern3, '$')
  }

  private val checker = InferenceChecker(NaturalNumbers.program, EntailmentSort.GroundPathDepthLimit)

  private val xZeroContext = List(InstanceOf("x", "Zero"))

  private val yOneContext = List(InstanceOf("x", "Zero")
                                ,InstanceOf("y", "Succ"), InstanceOf(Path.fromString("y.p"), "Zero")
                                ,PathEquivalence("x", Path.fromString("y.p")))

  private val zTwoContext = List(InstanceOf("x", "Zero")
                                ,InstanceOf("y", "Succ"), InstanceOf(Path.fromString("y.p"), "Zero")
                                ,PathEquivalence("x", Path.fromString("y.p"))
                                ,InstanceOf("z", "Succ"), InstanceOf(Path.fromString("z.p"), "Succ")
                                ,PathEquivalence("y", Path.fromString("z.p")))

  test ("check bound variable 1") {
    assert(checker.typeCheck(xZeroContext, "x", Type("n", Set(PathEquivalence("x", "n")))))
  }

  test ("check bound variable 2") {
    assert(checker.typeCheck(yOneContext, "x", Type("n", Set(PathEquivalence("n", "x")))))
  }

  test ("check bound variable 3") {
    assert(checker.typeCheck(yOneContext, "y", Type("n", Set(PathEquivalence("n", "y")))))
  }

  test ("check bound variable 4") {
    assert(checker.typeCheck(zTwoContext, "x", Type("n", Set(PathEquivalence("n", "x")))))
  }

  test ("check bound variable 5") {
    assert(checker.typeCheck(zTwoContext, "y", Type("n", Set(PathEquivalence("n", "y")))))
  }

  test ("check bound variable 6") {
    assert(checker.typeCheck(zTwoContext, "z", Type("n", Set(PathEquivalence("n", "z")))))
  }

  test ("check unbound variable 1") {
    val result = checker.typeOf(Nil, "x")

    assert(result.isRight)
    val Right(errors) = result

    assert(errors.size == 1)
    assert(errors.contains("variable 'x' is not available in context ·"))
  }

  test ("check unbound variable 2") {
    val result = checker.typeOf(xZeroContext, "y")

    assert(result.isRight)
    val Right(errors) = result

    assert(errors.size == 1)
    assert(errors.contains("variable 'y' is not available in context x :: Zero"))
  }

  test ("check field access 1") {
    assert(checker.typeCheck(yOneContext, FieldAccess("y", "p"), Type("n", Set(InstanceOf("n", "Zero")))))
  }

  test ("check field access 2") {
    assert(checker.typeCheck(yOneContext, FieldAccess("y", "p"), Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check field access 3") {
    assert(checker.typeCheck(zTwoContext, FieldAccess("y", "p"), Type("n", Set(InstanceOf("n", "Zero")))))
  }

  test ("check field access 4") {
    assert(checker.typeCheck(zTwoContext, FieldAccess("z", "p"), Type("n", Set(InstanceOf("n", "Succ")))))
  }

  test ("check field access 5") {
    assert(checker.typeCheck(zTwoContext, FieldAccess("z", "p"), Type("n", Set(InstanceOf("n", "Nat")))))
  }

//  test ("check field access 6") {
//    println(checker.typeOf(zTwoContext, FieldAccess(FieldAccess("z", "p"), "p")))
//    // -> 'x2.p' is not available in context x :: Zero, y :: Succ, y.p :: Zero, x ≡ y.p, z :: Succ, z.p :: Succ, y ≡ z.p, x2 :: Nat, x2 :: Succ
//    // TODO: why is the connection missing?
//    //         presumably since the inner field access is no variable access and doesn't get reduced during compile time
//    //         and the type of the field access only contains instance information and no path equivalence information
//    assert(checker.typeCheck(zTwoContext, FieldAccess(FieldAccess("z", "p"), "p"), Type("n", Set(InstanceOf("n", "Zero")))))
//  }

  test ("check field access failure 1") {
    val result = checker.typeOf(xZeroContext, FieldAccess("x", "p"))

    assert(result.isRight)
    val Right(errors) = result

    assert(errors.size == 1)
    assert(compareStringWithHole(errors.head, "'_.p' is not available in context x :: Zero, _ ≡ x"))
  }

  test ("check field access failure 2") {
    val result = checker.typeOf(yOneContext, FieldAccess("x", "p"))

    assert(result.isRight)
    val Right(errors) = result

    assert(errors.size == 1)
    assert(compareStringWithHole(errors.head, "'_.p' is not available in context x :: Zero, y :: Succ, y.p :: Zero, x ≡ y.p, _ ≡ x"))
  }

  test ("check method call 01") {
    assert(checker.typeCheck(xZeroContext, MethodCall("prev", "x"), Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check method call 02") {
    // We cannot assert that x::Zero |- prev(x) : Zero, since the signature of 'prev' only provides the more general Nat information
    // If we would want to arrive on that conclusion, we would have to dispatch the method during compile time, which is not advisable
    //   also it wouldn't necessarily work on any occasion. (e.g. if a method call depends on dynamic runtime information)
    assert(!checker.typeCheck(xZeroContext, MethodCall("prev", "x"), Type("n", Set(InstanceOf("n", "Zero")))))
  }

  test ("check method call 03") {
    assert(checker.typeCheck(yOneContext, MethodCall("prev", "y"), Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check method call 04") {
    // Same as 2
    assert(!checker.typeCheck(yOneContext, MethodCall("prev", "x"), Type("n", Set(InstanceOf("n", "Zero")))))
  }

  test ("check method call 05") {
    assert(!checker.typeCheck(yOneContext, MethodCall("prev", "y"), Type("n", Set(InstanceOf("n", "Succ")))))
  }

  test ("check method call 06") {
    assert(!checker.typeCheck(yOneContext, MethodCall("prev", "y"), Type("n", Set(InstanceOf("n", "Zero")))))
  }

  test ("check method call 07") {
   assert(checker.typeCheck(yOneContext, MethodCall("prev", FieldAccess("y", "p")), Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check method call 08") {
    assert(checker.typeCheck(zTwoContext, MethodCall("prev", "z"), Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check method call 09") {
    assert(!checker.typeCheck(zTwoContext, MethodCall("prev", "z"), Type("n", Set(InstanceOf("n", "Succ")))))
  }

  test ("check method call 10") {
    assert(checker.typeCheck(zTwoContext, MethodCall("prev", "x"), Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check method call 11") {
    assert(checker.typeCheck(zTwoContext, MethodCall("prev", "y"), Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check method call 12") {
    assert(checker.typeCheck(zTwoContext, MethodCall("prev", FieldAccess("y", "p")), Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check method call 13") {
    assert(checker.typeCheck(zTwoContext, MethodCall("prev", FieldAccess("z", "p")), Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check method call failure 1") {
    val result = checker.typeOf(yOneContext, MethodCall("prev", FieldAccess("x", "p")))

    assert(result.isRight)
    val Right(errors) = result

    assert(errors.size == 1)
    compareStringWithHole(errors.head, "_.p' is not available in context x :: Zero, y :: Succ, y.p :: Zero, x ≡ y.p, _ ≡ x")
  }

  test ("check method call failure 2") {
    val result = checker.typeOf(xZeroContext, MethodCall("next", "x"))

    assert(result.isRight)
    val Right(errors) = result

    assert(errors.size == 1)
    assert(errors.contains("no method declaration of 'next' applicable to 'x'"))
  }

  test ("check · |- 'new Zero' is Zero") {
    assert(checker.typeCheck(Nil, ObjectConstruction("Zero", Nil), Type("n", Set(InstanceOf("n", "Zero")))))
  }

  test ("check · |- 'new Zero' is Nat") {
    assert(checker.typeCheck(Nil, ObjectConstruction("Zero", Nil), Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check '· |- new Succ(new Zero)' is 1") {
    assert(checker.typeCheck(Nil, ObjectConstruction("Succ", List(("p", ObjectConstruction("Zero", Nil)))),
      Type("n", Set(InstanceOf("n", "Succ"), InstanceOf(Path.fromString("n.p"), "Zero")))))
  }

  test ("check '· |- new Succ(new Zero)' is Nat") {
    assert(checker.typeCheck(Nil, ObjectConstruction("Succ", List(("p", ObjectConstruction("Zero", Nil)))),
      Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check '· |- new Succ(new Succ(new Zero))' is 2") {
    assert(checker.typeCheck(Nil, ObjectConstruction("Succ", List(("p", ObjectConstruction("Succ", List(("p", ObjectConstruction("Zero", Nil))))))),
      Type("n", Set(InstanceOf("n", "Succ"), InstanceOf(Path.fromString("n.p"), "Succ"), InstanceOf(Path.fromString("n.p.p"), "Zero")))))
  }

  test ("check '· |- new Succ(new Succ(new Zero))' is Nat") {
    assert(checker.typeCheck(Nil, ObjectConstruction("Succ", List(("p", ObjectConstruction("Succ", List(("p", ObjectConstruction("Zero", Nil))))))),
      Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check 'x::Zero |- new Zero' is Nat") {
    assert(checker.typeCheck(xZeroContext, ObjectConstruction("Zero", Nil), Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check 'x::Zero |- new Succ(x)' is 1") {
    assert(checker.typeCheck(xZeroContext, ObjectConstruction("Succ", List(("p", "x"))),
      Type("n", Set(InstanceOf("n", "Succ"), InstanceOf(Path.fromString("n.p"), "Zero")))))
  }

  test ("check 'x::Zero |- new Succ(x)' is Nat") {
    assert(checker.typeCheck(xZeroContext, ObjectConstruction("Succ", List(("p", "x"))),
      Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check 'x::Zero, y::Succ, y.p::Zero, x≡y.p |- new Succ(x)' is 1") {
    assert(checker.typeCheck(yOneContext, ObjectConstruction("Succ", List(("p", "x"))),
      Type("n", Set(InstanceOf("n", "Succ"), InstanceOf(Path.fromString("n.p"), "Zero")))))
  }

  test ("check 'x::Zero, y::Succ, y.p::Zero, x≡y.p |- new Succ(x)' is Nat") {
    assert(checker.typeCheck(yOneContext, ObjectConstruction("Succ", List(("p", "x"))),
      Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check 'x::Zero, y::Succ, y.p::Zero, x≡y.p |- new Succ(y)' is 2") {
    assert(checker.typeCheck(yOneContext, ObjectConstruction("Succ", List(("p", "y"))),
      Type("n", Set(InstanceOf("n", "Succ"), InstanceOf(Path.fromString("n.p"), "Succ"), InstanceOf(Path.fromString("n.p.p"), "Zero")))))
  }

  test ("check 'x::Zero, y::Succ, y.p::Zero, x≡y.p |- new Succ(y)' is Nat") {
    assert(checker.typeCheck(yOneContext, ObjectConstruction("Succ", List(("p", "y"))),
      Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check 'x::Zero, y::Succ, y.p::Zero, x≡y.p |- new Succ(y.p)' is 1") {
    assert(checker.typeCheck(yOneContext, ObjectConstruction("Succ", List(("p", FieldAccess("y", "p")))),
      Type("n", Set(InstanceOf("n", "Succ"), InstanceOf(Path.fromString("n.p"), "Zero")))))
  }

  test ("check 'x::Zero, y::Succ, y.p::Zero, x≡y.p |- new Succ(y.p)' is Nat") {
    assert(checker.typeCheck(yOneContext, ObjectConstruction("Succ", List(("p", FieldAccess("y", "p")))),
      Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check 'x::Zero, y::Succ(x), z::Succ(y) |- new Succ(x)' is 1") {
    assert(checker.typeCheck(zTwoContext, ObjectConstruction("Succ", List(("p", "x"))),
      Type("n", Set(InstanceOf("n", "Succ"), InstanceOf(Path.fromString("n.p"), "Zero")))))
  }

  test ("check 'x::Zero, y::Succ(x), z::Succ(y) |- new Succ(x)' is Nat") {
    assert(checker.typeCheck(zTwoContext, ObjectConstruction("Succ", List(("p", "x"))),
      Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check 'x::Zero, y::Succ(x), z::Succ(y) |- new Succ(y)' is 2") {
    assert(checker.typeCheck(zTwoContext, ObjectConstruction("Succ", List(("p", "y"))),
      Type("n", Set(InstanceOf("n", "Succ"), InstanceOf(Path.fromString("n.p"), "Succ"), InstanceOf(Path.fromString("n.p.p"), "Zero")))))
  }

  test ("check 'x::Zero, y::Succ(x), z::Succ(y) |- new Succ(y)' is Nat") {
    assert(checker.typeCheck(zTwoContext, ObjectConstruction("Succ", List(("p", "y"))),
      Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check 'x::Zero, y::Succ(x), z::Succ(y) |- new Succ(z)' is 3") {
    assert(checker.typeCheck(zTwoContext, ObjectConstruction("Succ", List(("p", "z"))),
      Type("n", Set(InstanceOf("n", "Succ"), InstanceOf(Path.fromString("n.p"), "Succ"), InstanceOf(Path.fromString("n.p.p"), "Succ"), InstanceOf(Path.fromString("n.p.p.p"), "Zero")))))
  }

  test ("check 'x::Zero, y::Succ(x), z::Succ(y) |- new Succ(z)' is Nat") {
    assert(checker.typeCheck(zTwoContext, ObjectConstruction("Succ", List(("p", "z"))),
      Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check 'x::Zero, y::Succ(x), z::Succ(y) |- new Succ(y.p)' is 1") {
    assert(checker.typeCheck(zTwoContext, ObjectConstruction("Succ", List(("p", FieldAccess("y", "p")))),
      Type("n", Set(InstanceOf("n", "Succ"), InstanceOf(Path.fromString("n.p"), "Zero")))))
  }

  test ("check 'x::Zero, y::Succ(x), z::Succ(y) |- new Succ(y.p)' is Nat") {
    assert(checker.typeCheck(zTwoContext, ObjectConstruction("Succ", List(("p", FieldAccess("y", "p")))),
      Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check 'x::Zero, y::Succ(x), z::Succ(y) |- new Succ(z.p)' is >=2") {
    //println(checker.typeOf(zTwoContext, ObjectConstruction("Succ", List(("p", FieldAccess("z", "p"))))))
    // --> [x1. x1.cls ≡ Succ, x1.p :: Nat, x1.p :: Succ]

    // TODO: why is the connection to n.p.p not available?
    //         presumably because of the field access again, see field access test 6
    assert(checker.typeCheck(zTwoContext, ObjectConstruction("Succ", List(("p", FieldAccess("z", "p")))),
      //Type("n", Set(InstanceOf("n", "Succ"), InstanceOf(Path.fromString("n.p"), "Succ"), InstanceOf(Path.fromString("n.p.p"), "Zero"))))) // doesnt
      Type("n", Set(InstanceOf("n", "Succ"), InstanceOf(Path.fromString("n.p"), "Succ"))))) // works
      // Type("n", Set(InstanceOf("n", "Succ"), InstanceOf(Path.fromString("n.p"), "Succ"), InstanceOf(Path.fromString("n.p.p"), "Nat"))))) // doesnt
  }

  test ("check 'x::Zero, y::Succ(x), z::Succ(y) |- new Succ(z.p)' is Nat") {
    assert(checker.typeCheck(zTwoContext, ObjectConstruction("Succ", List(("p", FieldAccess("z", "p")))),
      Type("n", Set(InstanceOf("n", "Nat")))))
  }

  test ("check 'new Foo' is no constructor found") {
    val result = checker.typeOf(Nil, ObjectConstruction("Foo", Nil))

    assert(result.isRight)
    val Right(errors) = result

    assert(errors.size == 1)
    assert(errors.contains("no constructor found for class 'Foo'"))
  }

  test ("check 'new Nat' is abstract / no constructor") {
    val result = checker.typeOf(Nil, ObjectConstruction("Nat", Nil))

    assert(result.isRight)
    val Right(errors) = result

    assert(errors.size == 1)
    assert(errors.contains("no constructor found for class 'Nat'"))
  }

  test ("check 'new Succ' is missing argument 'p'") {
    val result = checker.typeOf(Nil, ObjectConstruction("Succ", Nil))

    assert(result.isRight)
    val Right(errors) = result

    assert(errors.size == 1)
    assert(compareStringWithHole(errors.head, "Class Succ: constructor constraint _.p :: Nat could not be fulfilled"))
  }

  test ("check 'x::Zero |- new Zero(p=x)' has unexpected field 'p'") {
    // TODO: undecided if this should be an error or not, see discussion about object creation / refinement types
    val result = checker.typeOf(xZeroContext, ObjectConstruction("Zero", List(("p", "x"))))

    assert(result.isRight)
    val Right(errors) = result

    assert(errors.size == 1)
    assert(errors.contains("Class Zero: unexpected field in constructor call"))
  }

  test ("check 'x::Zero |- new Zero(p=x)' has unexpected field 'q'") {
    // Same as above
    // Bonus: The smt solver would fail if the behaviour would be deemed OK,
    //        as field q is unknown from the program context
    //        leading to the access of undefined variables in the encoding
    val result = checker.typeOf(xZeroContext, ObjectConstruction("Zero", List(("q", "x"))))

    assert(result.isRight)
    val Right(errors) = result

    assert(errors.size == 1)
    assert(errors.contains("Class Zero: unexpected field in constructor call"))
  }

  test ("check 'x::Zero |- new Succ(p=x,q=x)' has unexpected field 'q'") {
    // Same as above
    val result = checker.typeOf(xZeroContext, ObjectConstruction("Succ", List(("p", "x"), ("q", "x"))))

    assert(result.isRight)
    val Right(errors) = result

    assert(errors.size == 1)
    assert(errors.contains("Class Succ: unexpected field in constructor call"))
  }
}
