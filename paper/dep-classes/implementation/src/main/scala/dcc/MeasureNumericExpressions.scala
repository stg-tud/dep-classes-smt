package dcc

import dcc.DCC.{Heap, HeapObj, HeapToString, NoArgsObj, ObjToString, UnaryArgObj}
import dcc.entailment.EntailmentSort
import dcc.interpreter.Interpreter
import dcc.program.NumericExpressions
import dcc.syntax.{Expression, Id, MethodCall, ObjectConstruction}
import dcc.types.InferenceChecker
import dcc.syntax.Implicit.StringToId

object MeasureNumericExpressions extends App {
  // TODO: add parameters for program and interp/typecheck?

  private val interp = new Interpreter(NumericExpressions.program, EntailmentSort.GroundPathDepthLimit)
//  private val checker = new InferenceChecker(NumericExpressions.program, EntailmentSort.GroundPathDepthLimit, debug=2)

  private val natHeap: Heap = Map(
    Id(Symbol("n0")) -> (Id(Symbol("Zero")), List()),
    Id(Symbol("n1")) -> (Id(Symbol("Succ")), List((Id(Symbol("p")), Id(Symbol("n0"))))),
    Id(Symbol("n2")) -> (Id(Symbol("Succ")), List((Id(Symbol("p")), Id(Symbol("n1"))))),
    Id(Symbol("n3")) -> (Id(Symbol("Succ")), List((Id(Symbol("p")), Id(Symbol("n2"))))),
    Id(Symbol("n4")) -> (Id(Symbol("Succ")), List((Id(Symbol("p")), Id(Symbol("n3"))))),
    Id(Symbol("n5")) -> (Id(Symbol("Succ")), List((Id(Symbol("p")), Id(Symbol("n4")))))
  )

  private val litHeap: Heap = Map(
    Id(Symbol("n0")) -> NoArgsObj("Zero"),
    Id(Symbol("n1")) -> UnaryArgObj("Succ", "p", "n0"),
//    Id(Symbol("n2")) -> (Id(Symbol("Succ")), List((Id(Symbol("p")), Id(Symbol("n1"))))),
//    Id(Symbol("n3")) -> (Id(Symbol("Succ")), List((Id(Symbol("p")), Id(Symbol("n2"))))),
//    Id(Symbol("n4")) -> (Id(Symbol("Succ")), List((Id(Symbol("p")), Id(Symbol("n3"))))),
//    Id(Symbol("n5")) -> (Id(Symbol("Succ")), List((Id(Symbol("p")), Id(Symbol("n4"))))),
//    Id(Symbol("l0")) -> UnaryArgObj("Lit", "value", "n0"),
    Id(Symbol("l1")) -> UnaryArgObj("Lit", "value", "n1"),
    Id(Symbol("p-l1-l1")) -> HeapObj("Plus")("l", "r")("l1", "l1")
  )

  private val zeroHeap: Heap = Map(
    Id(Symbol("n0")) -> (Id(Symbol("Zero")), List())
  )

  private val minimalLiteralHeap: Heap = Map(
    Id(Symbol("n0")) -> (Id(Symbol("Zero")), List()),
    Id(Symbol("l0")) -> (Id(Symbol("Lit")), List((Id(Symbol("value")), "n0")))
  )

  private def interpTest(h: Heap, e: Expression): Unit = {
    println(s"heap = ${HeapToString(h)}")

    val pre: Long = System.currentTimeMillis()
    val (h1, e1) = interp.execute(h, e)
    val post: Long = System.currentTimeMillis()

    println(s"interp(heap, $e) = $e1")

    if (h!=h1) {
      h1.filter { case (k, _) => !h.contains(k) }.foreach {
        case (x, obj) => s"$x → ${ObjToString(obj)}"
      }
    }

    println(s"elapsed time = ${post-pre} ms\n")
  }

  interpTest(natHeap, "n0")
  interpTest(natHeap, "n5")
  //interpTest(natHeap, ObjectConstruction("Succ", List(("p", "n5"))))

  interpTest(zeroHeap, ObjectConstruction("Lit", List(("value", "n0"))))

  interpTest(minimalLiteralHeap, "l0")
  interpTest(minimalLiteralHeap, MethodCall("eval", "l0"))
  interpTest(minimalLiteralHeap, ObjectConstruction("Plus", List(("l", "l0"), ("r", "l0"))))

  interpTest(litHeap, MethodCall("eval", "p-l1-l1"))
}
