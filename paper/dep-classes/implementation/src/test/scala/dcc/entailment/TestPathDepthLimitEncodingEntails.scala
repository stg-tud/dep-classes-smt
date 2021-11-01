package dcc.entailment

import dcc.entailment.EntailmentSort.PathDepthLimit
import dcc.program.NaturalNumbers
import dcc.syntax.{Constraint, FieldPath, InstanceOf, InstantiatedBy, PathEquivalence}
import dcc.syntax.Implicit.StringToId
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

class TestPathDepthLimitEncodingEntails extends AnyFunSuite with BeforeAndAfterEach {
  private var entailment = EntailmentFactory(PathDepthLimit)(NaturalNumbers.program, 0)

  override protected def beforeEach(): Unit = {
    entailment = EntailmentFactory(PathDepthLimit)(NaturalNumbers.program, 0)
  }

  test("· |- a=a") {
    assert(entailment.entails(Nil, PathEquivalence("x", "x")))
  }

  test("a=b |- b=a") {
    assert(entailment.entails(List(PathEquivalence("a", "b")), PathEquivalence("b", "a")))
    assert(entailment.entails(List(PathEquivalence("b", "a")), PathEquivalence("a", "b")))
  }

  test("a=b,b=c |- a=c") {
    assert(entailment.entails(List(PathEquivalence("a", "b"), PathEquivalence("b", "c")), PathEquivalence("a", "c")))
    assert(entailment.entails(List(PathEquivalence("a", "b"), PathEquivalence("c", "b")), PathEquivalence("c", "a")))
  }

  // takes ~20 seconds, uncomment if wanting to show this
  test("a=b,b=c,c=d,d=e,e=f,f=g |- a=g") {
    assert(entailment.entails(
      List(PathEquivalence("a", "b"), PathEquivalence("b", "c"), PathEquivalence("c", "d"), PathEquivalence("d", "e"), PathEquivalence("e", "f"), PathEquivalence("f", "g")),
      PathEquivalence("a", "g")))
  }

  // takes >90 secs
  test("a=b,b=c,c=d,d=e,e=f,f=g,g=h |- a=h") {
    assert(entailment.entails(
      List(PathEquivalence("a", "b"), PathEquivalence("b", "c"), PathEquivalence("c", "d"), PathEquivalence("d", "e"), PathEquivalence("e", "f"), PathEquivalence("f", "g"), PathEquivalence("g", "h")),
      PathEquivalence("a", "h")))
  }

  // does some i/o error in process call (process call timeout?)
//  test("a=b,b=c,c=d,d=e,e=f,f=g,g=h,h=i,i=j,j=k,k=l,l=m,m=n,n=o,o=p,p=q,q=r,r=s,s=t,t=u,u=v,v=w,w=x,x=y,y=z |- a=z") {
//    def constructTransitiveContext(vars: List[String]): List[Constraint] = vars match {
//      case Nil => Nil
//      case _ :: Nil => Nil
//      case l :: r :: tl => PathEquivalence(l,r)::constructTransitiveContext(r::tl)
//    }
//
//    val vars: List[String] = ('a' to 'z').map(_.toString).toList
//    val ctx = constructTransitiveContext(vars)
//
//    assert(entailment.entails(ctx, PathEquivalence("a", "z")))
//  }

  test("a.cls=C |- a::C") {
    assert(entailment.entails(List(InstantiatedBy("a", "Zero")), InstanceOf("a", "Zero")))
    assert(entailment.entails(List(InstantiatedBy("a", "Succ")), InstanceOf("a", "Succ")))
    assert(entailment.entails(List(InstantiatedBy("a", "Nat")), InstanceOf("a", "Nat")))
  }

  test("a::Zero |- a::Zero") {
    assert(entailment.entails(List(InstanceOf("a", "Zero")), InstanceOf("a", "Zero")))
  }

  test("a.cls=Zero |- a::Zero") {
    assert(entailment.entails(List(InstantiatedBy("a", "Zero")), InstanceOf("a", "Zero")))
  }

  test("a::Zero |- a::Nat") {
    assert(entailment.entails(List(InstanceOf("a", "Zero")), InstanceOf("a", "Nat")))
  }

  test("a.cls=Zero |- a::Nat") {
    assert(entailment.entails(List(InstantiatedBy("a", "Zero")), InstanceOf("a", "Nat")))
  }

  test("a::Succ,a.p::Nat |- a::Nat") {
    assert(entailment.entails(List(InstanceOf("a", "Succ"), InstanceOf(FieldPath("a", "p"), "Nat")), InstanceOf("a", "Nat")))
  }

  test("a::Succ,a.p::Zero |- a::Nat") {
    assert(entailment.entails(List(InstanceOf("a", "Succ"), InstanceOf(FieldPath("a", "p"), "Zero")), InstanceOf("a", "Nat")))
  }

  test("a.cls=Succ,a.p.cls=Zero |- a::Nat") {
    assert(entailment.entails(List(InstantiatedBy("a", "Succ"), InstantiatedBy(FieldPath("a", "p"), "Zero")), InstanceOf("a", "Nat")))
  }

  test("a.cls=Succ,a.p.cls=Zero |- a::Succ") {
    assert(entailment.entails(List(InstantiatedBy("a", "Succ"), InstantiatedBy(FieldPath("a", "p"), "Zero")), InstanceOf("a", "Succ")))
  }

  test("· |- x=y") {
    assert(!entailment.entails(Nil, PathEquivalence("x", "y")))
  }

  test("a=b |- a=c") {
    assert(!entailment.entails(List(PathEquivalence("a", "b")), PathEquivalence("a", "c")))
  }

  test ("a :: Zero |- a :: Succ") {
    assert(!entailment.entails(List(InstanceOf("a", "Zero")), InstanceOf("a", "Succ")))
  }

  test ("a::Succ, a.p::Zero |- a::Zero") {
    assert(!entailment.entails(List(InstanceOf("a", "Succ"), InstanceOf(FieldPath("a", "p"), "Zero")), InstanceOf("a", "Zero")))
  }

  test ("x::Succ, x.p::Zero |- x::Zero") {
    assert(!entailment.entails(List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero")), InstanceOf("x", "Zero")))
  }

  test ("a.cls=Zero |- a :: Succ") {
    assert(!entailment.entails(List(InstantiatedBy("a", "Zero")), InstanceOf("a", "Succ")))
  }

  test ("a.cls=Succ, a.p.cls=Zero |- a::Zero") {
    assert(!entailment.entails(List(InstantiatedBy("a", "Succ"), InstantiatedBy(FieldPath("a", "p"), "Zero")), InstanceOf("a", "Zero")))
  }
}
