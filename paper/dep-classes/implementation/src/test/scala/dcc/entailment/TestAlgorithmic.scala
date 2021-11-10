package dcc.entailment

import dcc.program.NaturalNumbers
import dcc.syntax.{Constraint, InstanceOf, InstantiatedBy, PathEquivalence}
import dcc.syntax.Implicit.StringToId
import org.scalatest.funsuite.AnyFunSuite

class TestAlgorithmic extends AnyFunSuite{
  private def measureAvgTime[R](block: => R, repeats: Int): (R, Double) = {
    val result = block
    var total = 0L

    (0 until repeats).foreach { _ =>
      val t0 = System.nanoTime()
      block
      val t1 = System.nanoTime()
      total += t1-t0
    }

    (result, total.toDouble/repeats)
  }

  @inline
  private def NsToMs(l: Double): Double = l/1000000d

  val algo = new Algorithmic(NaturalNumbers.program)

  test ("check p=p,p.cls=cls |- p::cls") {
    val repeats = 10
    println()
    print("check p=p,p.cls=cls |- p::cls: ")
    val (result, total) = measureAvgTime(algo.entails(List(PathEquivalence("p", "p"), InstantiatedBy("p", "cls")), InstanceOf("p", "cls")), repeats)
    println(if (result) "✓" else "×")
    println(s"\ttook ${NsToMs(total)}ms")

    assert(result)
  }

  test("check a=b,b=c,p.cls=cls |- b=c") {
    val repeats = 10
    println()
    print("check a=b,b=c,p.cls=cls |- b=c: ")
    val (result, total) = measureAvgTime(algo.entails(List(PathEquivalence("a", "b"), PathEquivalence("b", "c"), InstantiatedBy("p", "cls")), PathEquivalence("b", "c")), repeats)
    println(if (result) "✓" else "×")
    println(s"\ttook ${NsToMs(total)}ms")

    assert(result)
  }

  test("check a=b |- b=a") {
    val repeats = 10
    println()
    print("check a=b |- b=a: ")
    val (result, total) = measureAvgTime(algo.entails(List(PathEquivalence("a", "b")), PathEquivalence("b", "a")), repeats)
    println(if (result) "✓" else "×")
    println(s"\ttook ${NsToMs(total)}ms")

    assert(result)
  }

  test("check a=b,b=c |- a=c") {
    val repeats = 10
    println()
    print("check a=b,b=c |- a=c: ")
    val (result, total) = measureAvgTime(algo.entails(List(PathEquivalence("a", "b"), PathEquivalence("b", "c")), PathEquivalence("a", "c")), repeats)
    println(if (result) "✓" else "×")
    println(s"\ttook ${NsToMs(total)}ms")

    assert(result)
  }

  test("check a=b,b=c,c=d |- a=d") {
    val repeats = 10
    println()
    print("check a=b,b=c,c=d |- a=d: ")
    val (result, total) = measureAvgTime(algo.entails(
      List(
        PathEquivalence("a", "b"),
        PathEquivalence("b", "c"),
        PathEquivalence("c", "d")
      ), PathEquivalence("a", "d")), repeats)
    println(if (result) "✓" else "×")
    println(s"\ttook ${NsToMs(total)}ms")

    assert(result)
  }

  test("check a=b,b=c,c=d,d=e |- a=e") {
    val repeats = 10
    println()
    print("check a=b,b=c,c=d,d=e |- a=e: ")
    val (result, total) = measureAvgTime(algo.entails(
      List(
        PathEquivalence("a", "b"),
        PathEquivalence("b", "c"),
        PathEquivalence("c", "d"),
        PathEquivalence("d", "e")
      ), PathEquivalence("a", "e")), repeats)
    println(if (result) "✓" else "×")
    println(s"\ttook ${NsToMs(total)}ms")

    assert(result)
  }

//  test("check a=b,b=c,...,q=r |- a=r") {
//    val repeats = 10
//    println()
//    print("check a=b,b=c,...,q=r |- a=r: ")
//    val (result, total) = measureAvgTime(algo.entails(
//      List(
//        PathEquivalence("a", "b"),
//        PathEquivalence("b", "c"),
//        PathEquivalence("c", "d"),
//        PathEquivalence("d", "e"),
//        PathEquivalence("e", "f"),
//        PathEquivalence("f", "g"),
//        PathEquivalence("g", "h"),
//        PathEquivalence("h", "i"),
//        PathEquivalence("i", "j"),
//        PathEquivalence("j", "k"),
//        PathEquivalence("k", "l"),
//        PathEquivalence("l", "m"),
//        PathEquivalence("m", "n"),
//        PathEquivalence("n", "o"),
//        PathEquivalence("o", "p"),
//        PathEquivalence("p", "q"),
//        PathEquivalence("q", "r")
//      ), PathEquivalence("a", "r")), repeats)
//    println(if (result) "✓" else "×")
//    println(s"\ttook ${NsToMs(total)}ms")
//
//    assert(result)
//  }

  private def constructTransitiveContext(vars: List[String]): List[Constraint] = vars match {
    case Nil => Nil
    case _ :: Nil => Nil
    case l :: r :: tl => PathEquivalence(l,r)::constructTransitiveContext(r::tl)
  }

  private def constraintsToString(cs: List[Constraint]): String = cs match {
    case Nil => "·"
    case last::Nil => last.toString
    case x::xs => s"$x, ${constraintsToString(xs)}"
  }

  test ("test transitivity timings") {
    val repeats = 10

    println(s"measure avg runtime of transitivity chain entailments over $repeats repeats")

    // warmup
    algo.entails(Nil, PathEquivalence("a", "a"))

    val start: Char = 'a'
    ('a' to 'f').foreach {
      end: Char =>
        val vars = (start to end).map(_.toString).toList
        val ctx = constructTransitiveContext(vars)
        val conclusion = PathEquivalence(start.toString, end.toString)
        print(s"check ${constraintsToString(ctx)} |- $conclusion: ")
        val (result, time) = measureAvgTime(algo.entails(ctx, conclusion), repeats)
        println(if (result) "✓" else "×")
        println(f"\ttook ${NsToMs(time)}%1.4fms")
    }
  }
}