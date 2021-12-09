package dcc

import dcc.entailment.EntailmentSort._
import dcc.entailment.{Entailment, EntailmentFactory}
import dcc.program.{BooleanExpressions, NaturalNumbers}
import dcc.syntax.Program.Program
import dcc.syntax.{Constraint, Id, PathEquivalence}

import scala.annotation.tailrec

// TODO: measure runtime with a list to get min/max/avg/mean values
//       maybe use multiple classes/or parameter to allow multiple tests in parallel
// TODO: refactor name to MeasureTransitivityChainRuntime?
object MeasureRuntime extends App {
  private val entailmentParamNames: List[String] = List("entail", "entails", "entailment", "sort", "test")
  private val programParamNames: List[String] = List("program", "prog", "p")
  private val endCharParamNames: List[String] = List("end", "endchar", "goal", "char", "until")
  private val iterationsParamNames: List[String] = List("iterations", "iterate", "iter", "repeat", "repeats", "repetitions", "reps")

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

  // TODO: add iterationsDecreaseFlag param?
  private def parseParams: (Option[EntailmentSort], Option[Program], Option[Char], Option[Int]) = args.length match {
    case 0 => (None, None, None, None)
    case 1 => (parseEntailment(args.head), None, None, None)
    case 2 =>
      val entailment: Option[EntailmentSort] =
        if (findParam(args, entailmentParamNames).isDefined)
          parseEntailment(findParam(args, entailmentParamNames).get)
        else
          parseEntailment(args.head)

      val program: Option[Program] =
        if (findParam(args, programParamNames).isDefined)
          parseProgram(findParam(args, programParamNames).get)
        else
          parseProgram(args.last)

      (entailment, program, None, None)
    case 3 =>
      val entailment: Option[EntailmentSort] =
        if (findParam(args, entailmentParamNames).isDefined)
          parseEntailment(findParam(args, entailmentParamNames).get)
        else
          parseEntailment(args.head)

      val program: Option[Program] =
        if (findParam(args, programParamNames).isDefined)
          parseProgram(findParam(args, programParamNames).get)
        else
          parseProgram(args(1))

      val endChar: Option[Char] =
        if (findParam(args, endCharParamNames).isDefined)
          parseEndChar(findParam(args, endCharParamNames).get)
        else
          parseEndChar(args.last)

      (entailment, program, endChar, None)
    case _ =>
      val entailment: Option[EntailmentSort] =
        if (findParam(args, entailmentParamNames).isDefined)
          parseEntailment(findParam(args, entailmentParamNames).get)
        else
          parseEntailment(args.head)

      val program: Option[Program] =
        if (findParam(args, programParamNames).isDefined)
          parseProgram(findParam(args, programParamNames).get)
        else
          parseProgram(args(1))

      val endChar: Option[Char] =
        if (findParam(args, endCharParamNames).isDefined)
          parseEndChar(findParam(args, endCharParamNames).get)
        else
          parseEndChar(args(2))

      val iterations: Option[Int] =
        if(findParam(args, iterationsParamNames).isDefined)
          parseIterations(findParam(args, iterationsParamNames).get)
        else
          parseIterations(args(3))

      (entailment, program, endChar, iterations)
  }

  private def findParam(params: Array[String], names:List[String]): Option[String] =
    params.find(checkParamNames(_, names))

  @tailrec
  private def findParamIndex(params: Array[String], names: List[String], counter: Int = 0): Option[Int] = params match {
    case Array() => None
    case Array(param, _*) if checkParamNames(param, names) => Some(counter)
    case Array(_, rst@_*) => findParamIndex(rst.toArray, names, counter+1)
  }

  @tailrec
  private def checkParamNames(param: String, names: List[String]): Boolean = names match {
    case Nil => false
    case name :: _ if param.toLowerCase.startsWith(name) => true
    case _ :: rst => checkParamNames(param, rst)
  }

  private def parseEntailment(s: String): Option[EntailmentSort] = {
    if (s.contains("=")) {
      val arg = s.split('=')
      if (arg.length == 2)
        _parseEntailment(arg.last)
      else
        None
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
              x=="ground-limit" =>
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
    } else
      _parseProgram(s)
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
    } else
      _parseEndChar(s)
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
    } else
      _parseIterations(s)
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

  val (entailmentParam, program, endChar, iterationsParam) = parseParams

  if (entailmentParam.isDefined && program.isDefined) {
    val end = endChar.getOrElse('f')
    val iterations = iterationsParam.getOrElse(20)

    val entailment = EntailmentFactory(entailmentParam.get)(program.get, 0)
    measureTransitivityChainEntailmentRuntime(entailment, end, iterations)
  } else {
    println("Entailment sort parameter must be defined.")
    println("Usage:")
    println(s"\t$this ENTAILMENT PROGRAM [CHAR ITERATIONS]")
    println(s"\t$this entailment=ENTAILMENT program=PROGRAM end=CHAR iterations=NAT")
    println(s"ENTAILMENT: semantic, simplifiedSematic, pathDepthLimit, groundPathDepthLimit, algorithmic, algorithmicFix1, algorithmicFix2, algorithmicFix1RandomizedPick, algorithmicFix2RandomizedPick")
    println(s"PROGRAM: boolean-expressions, natural-numbers")
  }
}
