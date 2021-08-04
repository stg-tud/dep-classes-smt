package dcc.entailment

import dcc.program.{Empty, NaturalNumbers}
import dcc.syntax.{FieldPath, InstanceOf, InstantiatedBy, PathEquivalence}
import dcc.syntax.Implicit.StringToId
import org.scalatest.funsuite.AnyFunSuite

class TestSimplifiedSemanticEntailmentEntails extends AnyFunSuite {
  test("· |- a=a") {
    val entailmentEmpty = new SimplifiedSemanticEntailment(Empty.program)
    val entailmentNaturalNumbers = new SimplifiedSemanticEntailment(NaturalNumbers.program)

    assert(entailmentEmpty.entails(Nil, PathEquivalence("p", "p")))
    assert(entailmentNaturalNumbers.entails(Nil, PathEquivalence("x", "x")))
  }

  test("a=b |- b=a") {
    val entailmentEmpty = new SimplifiedSemanticEntailment(Empty.program)
    val entailmentNaturalNumbers = new SimplifiedSemanticEntailment(NaturalNumbers.program)

    assert(entailmentEmpty.entails(List(PathEquivalence("p", "q")), PathEquivalence("q", "p")))
    assert(entailmentEmpty.entails(List(PathEquivalence("q", "p")), PathEquivalence("p", "q")))

    assert(entailmentNaturalNumbers.entails(List(PathEquivalence("a", "b")), PathEquivalence("b", "a")))
    assert(entailmentNaturalNumbers.entails(List(PathEquivalence("b", "a")), PathEquivalence("a", "b")))
  }

  test("a=b,b=c |- a=c") {
    val entailmentEmpty = new SimplifiedSemanticEntailment(Empty.program)
    val entailmentNaturalNumbers = new SimplifiedSemanticEntailment(NaturalNumbers.program)

    assert(entailmentEmpty.entails(List(PathEquivalence("a", "b"), PathEquivalence("b", "c")), PathEquivalence("a", "c")))
    assert(entailmentEmpty.entails(List(PathEquivalence("a", "b"), PathEquivalence("c", "b")), PathEquivalence("c", "a")))

    assert(entailmentNaturalNumbers.entails(List(PathEquivalence("a", "b"), PathEquivalence("b", "c")), PathEquivalence("a", "c")))
    assert(entailmentNaturalNumbers.entails(List(PathEquivalence("a", "b"), PathEquivalence("c", "b")), PathEquivalence("c", "a")))
  }

  test("a=b,b=c,c=d,d=e,e=f,f=g |- a=g") {
    val entailmentEmpty = new SimplifiedSemanticEntailment(Empty.program)
    val entailmentNaturalNumbers = new SimplifiedSemanticEntailment(NaturalNumbers.program)

    assert(entailmentEmpty.entails(
      List(PathEquivalence("a", "b"), PathEquivalence("b", "c"), PathEquivalence("c", "d"), PathEquivalence("d", "e"), PathEquivalence("e", "f"), PathEquivalence("f", "g")),
      PathEquivalence("a", "g")))

    assert(entailmentNaturalNumbers.entails(
      List(PathEquivalence("a", "b"), PathEquivalence("b", "c"), PathEquivalence("c", "d"), PathEquivalence("d", "e"), PathEquivalence("e", "f"), PathEquivalence("f", "g")),
      PathEquivalence("a", "g")))
  }

  test("a=b,b=c,c=d,d=e,e=f,f=g,g=h |- a=h") {
    val entailmentEmpty = new SimplifiedSemanticEntailment(Empty.program)
    val entailmentNaturalNumbers = new SimplifiedSemanticEntailment(NaturalNumbers.program)

    assert(entailmentEmpty.entails(
      List(PathEquivalence("a", "b"), PathEquivalence("b", "c"), PathEquivalence("c", "d"), PathEquivalence("d", "e"), PathEquivalence("e", "f"), PathEquivalence("f", "g"), PathEquivalence("g", "h")),
      PathEquivalence("a", "h")))

    assert(entailmentNaturalNumbers.entails(
      List(PathEquivalence("a", "b"), PathEquivalence("b", "c"), PathEquivalence("c", "d"), PathEquivalence("d", "e"), PathEquivalence("e", "f"), PathEquivalence("f", "g"), PathEquivalence("g", "h")),
      PathEquivalence("a", "h")))
  }

  test("a.cls=C |- a::C") {
    val entailment = new SimplifiedSemanticEntailment(NaturalNumbers.program)

    assert(entailment.entails(List(InstantiatedBy("a", "Zero")), InstanceOf("a", "Zero")))
    assert(entailment.entails(List(InstantiatedBy("a", "Succ")), InstanceOf("a", "Succ")))
    assert(entailment.entails(List(InstantiatedBy("a", "Nat")), InstanceOf("a", "Nat")))
  }

  test("a::Zero |- a::Nat") {
    val entailment = new SimplifiedSemanticEntailment(NaturalNumbers.program)

    assert(entailment.entails(List(InstanceOf("a", "Zero")), InstanceOf("a", "Nat")))
  }

  test("a.cls=Zero |- a::Nat") {
    val entailment = new SimplifiedSemanticEntailment(NaturalNumbers.program)

    assert(entailment.entails(List(InstantiatedBy("a", "Zero")), InstanceOf("a", "Nat")))
  }

  test("a::Succ,a.p::Nat |- a::Nat") {
    val entailment = new SimplifiedSemanticEntailment(NaturalNumbers.program)

    assert(entailment.entails(List(InstanceOf("a", "Succ"), InstanceOf(FieldPath("a", "p"), "Nat")), InstanceOf("a", "Nat")))
  }

  test("a::Succ,a.p::Zero |- a::Nat") {
    val entailment = new SimplifiedSemanticEntailment(NaturalNumbers.program)

    assert(entailment.entails(List(InstanceOf("a", "Succ"), InstanceOf(FieldPath("a", "p"), "Zero")), InstanceOf("a", "Nat")))
  }

  test("a.cls=Succ,a.p.cls=Zero |- a::Nat") {
    val entailment = new SimplifiedSemanticEntailment(NaturalNumbers.program)

    assert(entailment.entails(List(InstantiatedBy("a", "Succ"), InstantiatedBy(FieldPath("a", "p"), "Zero")), InstanceOf("a", "Nat")))
  }

  test("· |- x=y") {
    val entailment = new SimplifiedSemanticEntailment(NaturalNumbers.program)

    assert(!entailment.entails(Nil, PathEquivalence("x", "y")))
  }

  test("a=b |- a=c") {
    val entailment = new SimplifiedSemanticEntailment(NaturalNumbers.program)

    assert(!entailment.entails(List(PathEquivalence("a", "b")), PathEquivalence("a", "c")))
  }

  test ("a::Succ, a.p::Zero |- a::Zero") { // TODO: investigate why this (and above) terminates in less than 1s when called from the test, but times out when called directly in the solver or from type checking
    val entailment = new SimplifiedSemanticEntailment(NaturalNumbers.program, debug = 4)

    assert(!entailment.entails(List(InstanceOf("a", "Succ"), InstanceOf(FieldPath("a", "p"), "Zero")), InstanceOf("a", "Zero")))
  }

  test ("x::Succ, x.p::Zero |- x::Zero") { // TODO: does time out. variable naming clash with var x in C-Prog rules?
    val entailment = new SimplifiedSemanticEntailment(NaturalNumbers.program, debug = 4)

    assert(!entailment.entails(List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero")), InstanceOf("x", "Zero")))
  }
}
