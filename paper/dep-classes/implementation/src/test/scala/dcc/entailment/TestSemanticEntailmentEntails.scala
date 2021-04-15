package dcc.entailment

import dcc.program.{Empty, NaturalNumbers}
import dcc.syntax.{Constraint, FieldPath, InstanceOf, InstantiatedBy, PathEquivalence}
import dcc.syntax.Implicit.StringToId
import org.scalatest.funsuite.AnyFunSuite

class TestSemanticEntailmentEntails extends AnyFunSuite{
  test("· |- a=a") {
    val entailmentEmpty = new SemanticEntailment(Empty.program)
    val entailmentNaturalNumbers = new SemanticEntailment(NaturalNumbers.program)

    assert(entailmentEmpty.entails(Nil, PathEquivalence("p", "p")))
    assert(entailmentNaturalNumbers.entails(Nil, PathEquivalence("x", "x")))
  }

  test("a=b |- b=a") {
    val entailmentEmpty = new SemanticEntailment(Empty.program)
    val entailmentNaturalNumbers = new SemanticEntailment(NaturalNumbers.program)

    assert(entailmentEmpty.entails(List(PathEquivalence("p", "q")), PathEquivalence("q", "p")))
    assert(entailmentEmpty.entails(List(PathEquivalence("q", "p")), PathEquivalence("p", "q")))

    assert(entailmentNaturalNumbers.entails(List(PathEquivalence("a", "b")), PathEquivalence("b", "a")))
    assert(entailmentNaturalNumbers.entails(List(PathEquivalence("b", "a")), PathEquivalence("a", "b")))
  }

  test("a=b,b=c |- a=c") {
    val entailmentEmpty = new SemanticEntailment(Empty.program)
    val entailmentNaturalNumbers = new SemanticEntailment(NaturalNumbers.program)

    assert(entailmentEmpty.entails(List(PathEquivalence("a", "b"), PathEquivalence("b", "c")), PathEquivalence("a", "c")))
    assert(entailmentEmpty.entails(List(PathEquivalence("a", "b"), PathEquivalence("c", "b")), PathEquivalence("c", "a")))

    assert(entailmentNaturalNumbers.entails(List(PathEquivalence("a", "b"), PathEquivalence("b", "c")), PathEquivalence("a", "c")))
    assert(entailmentNaturalNumbers.entails(List(PathEquivalence("a", "b"), PathEquivalence("c", "b")), PathEquivalence("c", "a")))
  }

  test("a=b,b=c,c=d,d=e,e=f,f=g |- a=g") {
    val entailmentEmpty = new SemanticEntailment(Empty.program)
    val entailmentNaturalNumbers = new SemanticEntailment(NaturalNumbers.program)

    assert(entailmentEmpty.entails(
      List(PathEquivalence("a", "b"), PathEquivalence("b", "c"), PathEquivalence("c", "d"), PathEquivalence("d", "e"), PathEquivalence("e", "f"), PathEquivalence("f", "g")),
      PathEquivalence("a", "g")))

    assert(entailmentNaturalNumbers.entails(
      List(PathEquivalence("a", "b"), PathEquivalence("b", "c"), PathEquivalence("c", "d"), PathEquivalence("d", "e"), PathEquivalence("e", "f"), PathEquivalence("f", "g")),
      PathEquivalence("a", "g")))
  }

  test("a=b,b=c,c=d,d=e,e=f,f=g,g=h |- a=h is unknown for natural numbers program") {
    val entailmentEmpty = new SemanticEntailment(Empty.program)
    val entailmentNaturalNumbers = new SemanticEntailment(NaturalNumbers.program)

    assert(entailmentEmpty.entails(
      List(PathEquivalence("a", "b"), PathEquivalence("b", "c"), PathEquivalence("c", "d"), PathEquivalence("d", "e"), PathEquivalence("e", "f"), PathEquivalence("f", "g"), PathEquivalence("g", "h")),
      PathEquivalence("a", "h")))

    assert(!entailmentNaturalNumbers.entails(  // returns unknown
      List(PathEquivalence("a", "b"), PathEquivalence("b", "c"), PathEquivalence("c", "d"), PathEquivalence("d", "e"), PathEquivalence("e", "f"), PathEquivalence("f", "g"), PathEquivalence("g", "h")),
      PathEquivalence("a", "h")))
  }

  test("a.cls=C |- a::C") {
    val entailment = new SemanticEntailment(NaturalNumbers.program)

    assert(entailment.entails(List(InstantiatedBy("a", "Zero")), InstanceOf("a", "Zero")))
    assert(entailment.entails(List(InstantiatedBy("a", "Succ")), InstanceOf("a", "Succ")))
    assert(entailment.entails(List(InstantiatedBy("a", "Nat")), InstanceOf("a", "Nat")))
  }

  test("a::Zero |- a::Nat") {
    val entailment = new SemanticEntailment(NaturalNumbers.program)

    assert(entailment.entails(List(InstanceOf("a", "Zero")), InstanceOf("a", "Nat")))
  }

  test("a.cls=Zero |- a::Nat") {
    val entailment = new SemanticEntailment(NaturalNumbers.program)

    assert(entailment.entails(List(InstantiatedBy("a", "Zero")), InstanceOf("a", "Nat")))
  }

  test("a::Succ,a.p::Nat |- a::Nat") {
    val entailment = new SemanticEntailment(NaturalNumbers.program)

    assert(entailment.entails(List(InstanceOf("a", "Succ"), InstanceOf(FieldPath("a", "p"), "Nat")), InstanceOf("a", "Nat")))
  }

  test("a::Succ,a.p::Zero |- a::Nat") {
    val entailment = new SemanticEntailment(NaturalNumbers.program)

    assert(entailment.entails(List(InstanceOf("a", "Succ"), InstanceOf(FieldPath("a", "p"), "Zero")), InstanceOf("a", "Nat")))
  }

  test("a.cls=Succ,a.p.cls=Zero |- a::Nat") {
    val entailment = new SemanticEntailment(NaturalNumbers.program)

    assert(entailment.entails(List(InstantiatedBy("a", "Succ"), InstantiatedBy(FieldPath("a", "p"), "Zero")), InstanceOf("a", "Nat")))
  }
}
