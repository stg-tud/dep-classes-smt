package dcc

import dcc.entailment.EntailmentSort._
import dcc.entailment.{Entailment, EntailmentFactory}
import dcc.program.{BooleanExpressions, NaturalNumbers}
import dcc.syntax.Program.Program
import dcc.syntax.{Constraint, Id, PathEquivalence}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object MeasureRuntime extends App {
  private val testParamNames: List[String] = List("test", "testsuite", "test-suite", "run")
  private val entailmentParamNames: List[String] = List("entail", "entails", "entailment", "sort", "algo", "algorithm")
  private val programParamNames: List[String] = List("program", "prog", "p")
  private val endCharParamNames: List[String] = List("end", "endchar", "goal", "char", "until")
  private val iterationsParamNames: List[String] = List("iterations", "iterate", "iter", "repeat", "repeats", "repetitions", "reps")

  private def measureAvgTime(block: => Boolean, repeats: Int): (Boolean, List[Long]) = {
    val timings: ListBuffer[Long] = new ListBuffer[Long]
    var result = false

    (0 until repeats).foreach { _ =>
      val t0 = System.nanoTime()
      result = block
      val t1 = System.nanoTime()
      timings += t1-t0
    }

    (result, timings.toList)
  }

  private val emptyParams = (None, None, None, None, None)
  // TODO: add iterationsDecreaseFlag param?
  // TODO: params only with pairs 'param=value'?
  private def parseParams: (Option[Test.Test], Option[EntailmentSort], Option[Program], Option[Char], Option[Int]) = args.length match {
    case 0 => emptyParams
    case 1 => emptyParams
    case 2 => emptyParams
    // We want to have at least 3 params
    case 3 =>
      val test: Option[Test.Test] =
        if (findParam(args, testParamNames).isDefined)
          parseTest(findParam(args, testParamNames).get)
        else
          parseTest(args.head)

      val entailment: Option[EntailmentSort] =
        if (findParam(args, entailmentParamNames).isDefined)
          parseEntailment(findParam(args, entailmentParamNames).get)
        else
          parseEntailment(args(1))

      val program: Option[Program] =
        if (findParam(args, programParamNames).isDefined)
          parseProgram(findParam(args, programParamNames).get)
        else
          parseProgram(args(2))

      (test, entailment, program, None, None)
    case 4 =>
      val test: Option[Test.Test] =
        if (findParam(args, testParamNames).isDefined)
          parseTest(findParam(args, testParamNames).get)
        else
          parseTest(args.head)

      val entailment: Option[EntailmentSort] =
        if (findParam(args, entailmentParamNames).isDefined)
          parseEntailment(findParam(args, entailmentParamNames).get)
        else
          parseEntailment(args(1))

      val program: Option[Program] =
        if (findParam(args, programParamNames).isDefined)
          parseProgram(findParam(args, programParamNames).get)
        else
          parseProgram(args(2))

      val endChar: Option[Char] =
        if (findParam(args, endCharParamNames).isDefined)
          parseEndChar(findParam(args, endCharParamNames).get)
        else
          parseEndChar(args(3))

      val iterations: Option[Int] =
        if (findParam(args, iterationsParamNames).isDefined)
          parseIterations(findParam(args, iterationsParamNames).get)
        else
          parseIterations(args(3))

      (test, entailment, program, endChar, iterations)
    case _ =>
      val test: Option[Test.Test] =
        if (findParam(args, testParamNames).isDefined)
          parseTest(findParam(args, testParamNames).get)
        else
          parseTest(args.head)

      val entailment: Option[EntailmentSort] =
        if (findParam(args, entailmentParamNames).isDefined)
          parseEntailment(findParam(args, entailmentParamNames).get)
        else
          parseEntailment(args(1))

      val program: Option[Program] =
        if (findParam(args, programParamNames).isDefined)
          parseProgram(findParam(args, programParamNames).get)
        else
          parseProgram(args(2))

      val endChar: Option[Char] =
        if (findParam(args, endCharParamNames).isDefined)
          parseEndChar(findParam(args, endCharParamNames).get)
        // endChar and iterations are optional parameters
        else if (parseEndChar(args(3)).isDefined)
          parseEndChar(args(3))
        else
          parseEndChar(args(4))


      val iterations: Option[Int] =
        if(findParam(args, iterationsParamNames).isDefined)
          parseIterations(findParam(args, iterationsParamNames).get)
        // endChar and iterations are optional parameters
        else if (parseIterations(args(3)).isDefined)
          parseIterations(args(3))
        else
          parseIterations(args(4))

      (test, entailment, program, endChar, iterations)
  }

  private def findParam(params: Array[String], names:List[String]): Option[String] =
    params.find(checkParamNames(_, names))

//  @tailrec
//  private def findParamIndex(params: Array[String], names: List[String], counter: Int = 0): Option[Int] = params match {
//    case Array() => None
//    case Array(param, _*) if checkParamNames(param, names) => Some(counter)
//    case Array(_, rst@_*) => findParamIndex(rst.toArray, names, counter+1)
//  }

  @tailrec
  private def checkParamNames(param: String, names: List[String]): Boolean = names match {
    case Nil => false
    case name :: _ if param.toLowerCase.startsWith(name) => true
    case _ :: rst => checkParamNames(param, rst)
  }

  private def parseTest(s: String): Option[Test.Test] = {
    if (s.contains("=")) {
      val arg = s.split('=')
      if (arg.length == 2)
        _parseTest(arg.last)
      else
        None
    } else {
      _parseTest(s)
    }
  }

  private val transitiveChainTestNames: List[String] = List("transitive-chain", "transitivechain", "transitive")
  private val invalidTransitiveChainTestNames: List[String] = List("invalid-transitive-chain", "invalidtransitivechain", "invalidtransitive", "non-transitive", "nontransitive")
  private val randomizedContextTransitiveChainTestNames: List[String] = List("randomized-transitive-chain", "random-transitive-chain", "randomizedtransitivechain", "randomtransitivechain", "randomized-transitive", "randomizedtransitive", "random-transitive", "randomtransitive")
  private def _parseTest(s: String): Option[Test.Test] = s match {
    case _ if transitiveChainTestNames.contains(s.toLowerCase) => Some(Test.TransitiveChain)
    case _ if invalidTransitiveChainTestNames.contains(s.toLowerCase) => Some(Test.InvalidTransitiveChain)
    case _ if randomizedContextTransitiveChainTestNames.contains(s.toLowerCase) => Some(Test.RandomizedContextTransitiveChain)
    case _ => None
  }

  private def parseEntailment(s: String): Option[EntailmentSort] = {
    if (s.contains("=")) {
      val arg = s.split('=')
      if (arg.length == 2)
        _parseEntailment(arg.last)
      else {
        None
      }
    }
    else
      _parseEntailment(s)
  }

  private def _parseEntailment(s: String): Option[EntailmentSort] = s.toLowerCase match {
    case "semantic" => Some(Semantic)
    case "simplifiedsemantic" => Some(SimplifiedSemantic)
    case x if x=="pathdepthlimit" ||
              x=="quantified-limit" =>
      Some(PathDepthLimit)
    case x if x=="groundpathdepthlimit" ||
              x=="ground-limit" ||
              x=="ground" =>
      Some(GroundPathDepthLimit)
    case x if x=="algorithmic" => Some(Algorithmic)
    case x if x=="algorithmicfix1" ||
              x=="fix1" =>
      Some(AlgorithmicFix1)
    case x if x=="algorithmicfix2" ||
              x=="fix2" =>
      Some(AlgorithmicFix2)
    case x if x=="algorithmicfix1randomizedpick" ||
              x=="fix1-random" ||
              x=="random1" =>
      Some(AlgorithmicFix1RandomizedPick)
    case x if x=="algorithmicfix2randomizedpick" ||
              x=="fix2-random" ||
              x=="random2" =>
      Some(AlgorithmicFix2RandomizedPick)
    case _ => None
  }

  private def parseProgram(s: String): Option[Program] = {
    if (s.contains("=")) {
      val arg = s.split('=')
      if (arg.length == 2)
        _parseProgram(arg.last)
      else
        None
    } else {
      _parseProgram(s)
    }
  }

  private val booleanExpressionNames: List[String] = List("booleanexpreesions", "boolean-expreesions", "boolean-expr", "boolean-exprs", "boolean", "bool", "boolexpr", "bool-expr", "bool-exprs")
  private val naturalNumbersNames: List[String] = List("naturalnumbers", "natural-numbers", "numbers", "nat", "naturals")
  private def _parseProgram(s: String): Option[Program] = s.toLowerCase match {
    case _ if naturalNumbersNames.contains(s.toLowerCase) => Some(NaturalNumbers.program)
    case _ if booleanExpressionNames.contains(s.toLowerCase) => Some(BooleanExpressions.program)
    case _ => None
  }

  private def parseEndChar(s: String): Option[Char] = {
    if (s.contains("=")) {
      val arg = s.split('=')
      if (arg.length == 2)
        _parseEndChar(arg.last)
      else
        None
    } else {
      _parseEndChar(s)
    }
  }

  private def _parseEndChar(s: String): Option[Char] = s.length match {
    case 1 if ('a' to 'z').contains(s.head.toLower) => Some(s.head.toLower)
    case _ => None
  }

  private def parseIterations(s: String): Option[Int] = {
    if (s.contains("=")) {
      val arg = s.split('=')
      if (arg.length == 2)
        _parseIterations(arg.last)
      else
        None
    } else {
      _parseIterations(s)
    }
  }

  private def _parseIterations(s: String): Option[Int] = try {
    Some(s.toInt)
  } catch {
    case _: NumberFormatException => None
  }

  private def constructTransitiveContext(vars: List[String]): List[Constraint] = vars match {
    case Nil => Nil
    case _ :: Nil => Nil
    case l :: r :: tl => PathEquivalence(Id(Symbol(l)),Id(Symbol(r)))::constructTransitiveContext(r::tl)
  }

//  private def constraintsToString(cs: List[Constraint]): String = cs match {
//    case Nil => "·"
//    case last::Nil => last.toString
//    case x::xs => s"$x, ${constraintsToString(xs)}"
//  }

  private def calculateForthcomingRepetitions(iterations: Int, meanTime: Double): Int =
    if (meanTime >= 180L*1000L*1000000L) {
      // if more than 180s avg
      // only do iterations/32 iteration going forwards
      iterations/32
    } else if (meanTime >= 60L*1000L*1000000L) {
      // if more than 60s avg
      // only do iterations/16 iteration going forwards
      iterations/16
    } else if (meanTime >= 10L*1000L*1000000L) {
      // if more than 10s avg
      // only do iterations/4 iterations going forwards
      iterations/4
    } else if (meanTime >= 1000*1000000) {
      // if more than 1000ms avg
      // only perform iterations/2 iterations going forwards
      iterations/2
    } else {
      // otherwise don't decrease
      iterations
    }

  private def NanoTimeToMilliSeconds(ns: Double): Double = ns/1000000d

//  private def NanoTimeToStringRounded(ns: Double): String = {
//    val ms = ns/1000000d
//
//    if (ms < 1d) {
//      return f"$ns%1.0f ns"
//    }
//
//    val s = ms/1000d
//
//    if (s < 1d) {
//      return f"$ms%1.3f ms"
//    }
//
//    val min = s/60d
//
//    if (min < 2d) {
//      return f"$s%1.3f s"
//    }
//
//    f"$min%1.2f min"
//  }

  private def calculateTimings(times: List[Long]): Timings = Timings(
    series = times.sorted
  )

  /***
    * Measures the avg runtime of transitivity chains starting from 'a' until the endpoint reaches `end` using `entailment` over `iterations` iterations.
    * The number iterations of performed iterations may decrease if `decreaseIterationsWithIncreasingRuntime` is set.
    * @param entailment The entailment to measure the runtime of
    * @param end The maximum element to which the transitivity chains will be expanded. Should be between 'a' and 'z'.
    * @param iterations The number of iterations to measure
    * @param decreaseIterationsWithIncreasingRuntime If set, decreases the number of iterations to measure the average runtime over
    *                                                if the average runtime exceeds some internal threshold
    */
  private def measureTransitivityChainEntailmentRuntime(entailment: Entailment, end: Char, iterations: Int, decreaseIterationsWithIncreasingRuntime: Boolean = true): Unit = {
    // warmup
    (1 to 10).foreach(_ => entailment.entails(Nil, PathEquivalence(Id(Symbol("a")), Id(Symbol("a")))))

    // number of iterations
    var repeats = iterations

    // start variable of the transitivity chain
    val start: Char = 'a'

    println(s"measure runtime of transitivity chain entailments with ${entailment.typ}")
    ('a' to end).foreach {
      // end variable of the transitivity chain
      end: Char =>
        val vars = (start to end).map(_.toString).toList
        val ctx = constructTransitiveContext(vars)
        val conclusion = PathEquivalence(Id(Symbol(start.toString)), Id(Symbol(end.toString)))
//        print(s"measure '${constraintsToString(ctx)} |- $conclusion' using $repeats iterations: ")
        val (_, times) = measureAvgTime(entailment.entails(ctx, conclusion), repeats)

        val measures = calculateTimings(times)

        if (decreaseIterationsWithIncreasingRuntime) {
          repeats = calculateForthcomingRepetitions(iterations, measures.mean)
        }
//        println(if (result) "✓" else "×")
//        println(s"\ttook ${NanoTimeToStringRounded(measures.mean)} on average")
//        println(s"\t    (${NanoTimeToMilliSeconds(measures.mean)} ms)")

        println(f"${entailment.typ};$conclusion;${NanoTimeToMilliSeconds(measures.min)};${NanoTimeToMilliSeconds(measures.max)};${NanoTimeToMilliSeconds(measures.mean)};${NanoTimeToMilliSeconds(measures.median)}")
    }
  }

  private def measureInvalidTransitivityChainEntailmentRuntime(entailment: Entailment, finalContextEnd: Char, iterations: Int, decreaseIterationsWithIncreasingRuntime: Boolean = true): Unit = {
    // warmup
    (1 to 10).foreach(_ => entailment.entails(Nil, PathEquivalence(Id(Symbol("a")), Id(Symbol("a")))))

    // number of iterations
    var repeats = iterations

    // start variable of the transitivity chain
    val contextStart: Char = 'a'

    println(s"measure runtime of unreachable transitivity chain entailments with ${entailment.typ}")
    ('a' to finalContextEnd).foreach {
      contextEnd: Char =>
        val vars = (contextStart to contextEnd).map(_.toString).toList
        val context = constructTransitiveContext(vars)
        val conclusion = PathEquivalence(Id(Symbol("a")), Id(Symbol(s"unreachable")))
        //val conclusion = PathEquivalence(Id(Symbol("a")), Id(Symbol(s"${contextEnd}1")))

        val (_, times) = measureAvgTime(entailment.entails(context, conclusion), repeats)

        val measures = calculateTimings(times)

        if (decreaseIterationsWithIncreasingRuntime) {
          repeats = calculateForthcomingRepetitions(iterations, measures.mean)
        }

        // TODO: move this printing to a function?
        println(f"${entailment.typ};$contextEnd;${NanoTimeToMilliSeconds(measures.min)};${NanoTimeToMilliSeconds(measures.max)};${NanoTimeToMilliSeconds(measures.mean)};${NanoTimeToMilliSeconds(measures.median)}")
    }
  }

  private val (testParam, entailmentParam, program, endChar, iterationsParam) = parseParams

  println(testParam)

  if (testParam.isDefined && entailmentParam.isDefined && program.isDefined) {
    // Default values for optional params
    val end = endChar.getOrElse('f')
    val iterations = iterationsParam.getOrElse(32)

    val entailment = EntailmentFactory(entailmentParam.get)(program.get, 0)
    testParam.get match {
      case Test.TransitiveChain => measureTransitivityChainEntailmentRuntime(entailment, end, iterations)
      case Test.InvalidTransitiveChain => measureInvalidTransitivityChainEntailmentRuntime(entailment, end, iterations)
      case Test.RandomizedContextTransitiveChain => () // TODO: add test suite
    }
  } else {
    if (testParam.isEmpty)
      System.err.println(s"test parameter must be set")
    if (entailmentParam.isEmpty)
      System.err.println(s"entailment parameter must be set")
    if (program.isEmpty)
      System.err.println(s"program parameter must be set")

    println("Usage:")
    println(s"\t$this TEST ENTAILMENT PROGRAM [CHAR ITERATIONS]")
    println(s"\t$this entailment=ENTAILMENT program=PROGRAM end=CHAR iterations=NAT")
    println("TEST: transitive, non-transitive, randomized-transitive")
    println("ENTAILMENT: semantic, simplifiedSemantic, pathDepthLimit, groundPathDepthLimit, algorithmic, algorithmicFix1, algorithmicFix2, algorithmicFix1RandomizedPick, algorithmicFix2RandomizedPick")
    println("PROGRAM: boolean-expressions, natural-numbers")
  }


  private case class Timings(series: List[Long]) {
    val min: Long = series.min
    val max: Long = series.max
    val mean: Double = series.sum.toDouble / series.size.toDouble
    val median: Long = series.sorted(Ordering.Long)((series.size-1)/2)
  }

  private object Test extends Enumeration {
    type Test = Value
    val TransitiveChain, InvalidTransitiveChain, RandomizedContextTransitiveChain = Value
  }
}
