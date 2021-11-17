package dcc

import dcc.entailment.{AlgorithmicFix1, Entailment, EntailmentFactory, EntailmentSort}
import dcc.program.NaturalNumbers
import dcc.syntax.{Constraint, Id, PathEquivalence}

object MeasureRuntime extends App {
  private def measureAvgTime(block: => Boolean, repeats: Int): (Boolean, Double) = {
    var total = 0L
    var result = false

    (0 until repeats).foreach { _ =>
      val t0 = System.nanoTime()
      result = block
      val t1 = System.nanoTime()
      total += t1-t0
    }

    (result, total.toDouble/repeats)
  }

  private def constructTransitiveContext(vars: List[String]): List[Constraint] = vars match {
    case Nil => Nil
    case _ :: Nil => Nil
    case l :: r :: tl => PathEquivalence(Id(Symbol(l)),Id(Symbol(r)))::constructTransitiveContext(r::tl)
  }

  private def constraintsToString(cs: List[Constraint]): String = cs match {
    case Nil => "·"
    case last::Nil => last.toString
    case x::xs => s"$x, ${constraintsToString(xs)}"
  }

  private def NanoTimeToMilliSeconds(ns: Double): Double = ns/1000000d

  private def NanoTimeToStringRounded(ns: Double): String = {
    val ms = ns/1000000d

    if (ms < 1d) {
      return f"$ns%1.0f ns"
    }

    val s = ms/1000d

    if (s < 1d) {
      return f"$ms%1.3f ms"
    }

    val min = s/60d

    if (min < 2d) {
      return f"$s%1.3f s"
    }

    f"$min%1.2f min"
  }

  /***
    * Measures the avg runtime of transitivity chains starting from 'a' until the endpoint reaches `end` using `entailment` over `iterations` iterations.
    * The number iterations of performed iterations may decrease if `decreaseIterationsWithIncreasingRuntime` is set.
    * @param entailment The entailment to measure the runtime of
    * @param end The maximum element to wich the transitivity chains will be expanded. Should be between 'b' and 'z'.
    * @param iterations The number of iterations to measure
    * @param decreaseIterationsWithIncreasingRuntime If set, decreases the number of iterations to measure the average runtime over
    *                                                if the average runtime exceeds some internal threshold
    */
  private def measureTransitivityChainEntailmentRuntime(entailment: Entailment, end: Char, iterations: Int = 20, decreaseIterationsWithIncreasingRuntime: Boolean = true): Unit = {
    // warmup
    (1 to 10).foreach(_ => entailment.entails(Nil, PathEquivalence(Id(Symbol("a")), Id(Symbol("a")))))

    // number of iterations
    var repeats = iterations

    // start variable of the transitivity chain
    val start: Char = 'a'

    println(s"measure avg runtime of transitivity chain entailments with ${entailment.typ}")
    ('a' to end).foreach {
      // end variable of the transitivity chain
      end: Char =>
        val vars = (start to end).map(_.toString).toList
        val ctx = constructTransitiveContext(vars)
        val conclusion = PathEquivalence(Id(Symbol(start.toString)), Id(Symbol(end.toString)))
        print(s"measure '${constraintsToString(ctx)} |- $conclusion' using $repeats iterations: ")
        val (result, time) = measureAvgTime(entailment.entails(ctx, conclusion), repeats)

        if (time >= 10L*1000L*1000000L) {
          // if more than 10s avg
          // only do 1 iteration going forwards
          repeats = 1
        } else if (time >= 500*1000000) {
          // if more than 500ms avg
          // only do iterations/4 iterations going forwards
          repeats = iterations/4
        } else if (time >= 10*1000000) {
          // if more than 10ms avg
          // only perform iterations/2 iterations going forwards
          repeats = iterations/2
        }
        println(if (result) "✓" else "×")
        println(s"\ttook ${NanoTimeToStringRounded(time)} on avg")
        println(s"\t    (${NanoTimeToMilliSeconds(time)} ms)")

        println(f"${entailment.typ};$conclusion;${NanoTimeToMilliSeconds(time)}")
    }
  }

  measureTransitivityChainEntailmentRuntime(EntailmentFactory(EntailmentSort.Algorithmic)(NaturalNumbers.program, 0), 'f')
  measureTransitivityChainEntailmentRuntime(EntailmentFactory(EntailmentSort.AlgorithmicFix1)(NaturalNumbers.program, 0), 'f')
  measureTransitivityChainEntailmentRuntime(EntailmentFactory(EntailmentSort.AlgorithmicFix2)(NaturalNumbers.program, 0), 'f')
//  measureTransitivityChainEntailmentRuntime(EntailmentFactory(EntailmentSort.PathDepthLimit)(NaturalNumbers.program, 0), 'h')
//  measureTransitivityChainEntailmentRuntime(EntailmentFactory(EntailmentSort.GroundPathDepthLimit)(NaturalNumbers.program, 0), 'h')
}
