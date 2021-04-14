package dcc.interpreter

import dcc.DCC.{HC, Heap, OC, Obj, classInProgram}
import dcc.Util.{alphaRename, substitute}
import dcc.entailment.Entailment
import dcc.syntax.Program.Program
import dcc.syntax.{Constraint, Expression, FieldAccess, FieldPath, Id, MethodCall, MethodImplementation, ObjectConstruction, PathEquivalence}

class Interpreter(program: Program, entailment: Entailment) {
  // TODO: change return type to Either or Option?
  def execute(heap: Heap, expr: Expression): (Heap, Expression) = expr match {
    case Id(_) => (heap, expr) // variables are values
    // R-Field
    case FieldAccess(x@Id(_), f) =>
      HC(heap).filter{
        case PathEquivalence(FieldPath(`x`, `f`), Id(_)) => true
        case PathEquivalence(Id(_), FieldPath(`x`, `f`)) => true
        case _ => false
      } match {
        case PathEquivalence(FieldPath(`x`, `f`), y@Id(_)) :: _ => (heap, y)
        case PathEquivalence(y@Id(_), FieldPath(`x`, `f`)) :: _ => (heap, y)
        case _ => (heap, expr) // x does not have field f TODO: return type
      }
    // R-Call
    case MethodCall(m, x@Id(_)) =>
      // Applicable methods
      //val S: List[(List[Constraint], Expression)] = mImplSubst(m, x).filter{case (as, _) => entails(HC(heap), as, vars)}
      val methods = mImplSubst(m, x)
      val S = methods.filter{ case (as, _) => entailment.entails(HC(heap), as) }

      if (S.isEmpty) // m not in P
        return (heap, expr)

      println("Applicable:")
      S.foreach(println)

      // Most specific method
      var (a, e) = S.head

      S.foreach{
        case (a1, e1) if e != e1 =>
          if (entailment.entails(a1, a)) {
            println(s"Most specific:$a1, $e1")
            //(a, e) = (a1, e1)
            a = a1
            e = e1
          }
        case _ => /* noop */
      }

      // TODO: execute correct? we don't want intermediate results
      execute(heap, e)
    // R-New
    case ObjectConstruction(cls, args)
      if args.forall{ // if args are values (Id)
        case (_, Id(_)) => true
        case _ => false
      } =>
      val x: Id  = freshVar()
      //val args1: List[(Id, Id)] = args.map{case (f, Id(z)) => (f, Id(z))} // case (f, _) => (f, Id('notReduced)) guard makes sure everything is an Id
      val o: Obj = (cls, args.asInstanceOf[List[(Id, Id)]])
      // cls in Program: alpha renaming of y to x in b and orElse stuck/error
      val (y: Id, b: List[Constraint]) = classInProgram(cls, program).getOrElse(return (heap, expr))
      //val b1 = alphaConversion(y, x, b)
      val b1 = substitute(y, x, b)
      // heap constraints entail cls constraints
      if (entailment.entails(HC(heap) ++ OC(x, o), b1))
        (heap + (x -> o), x)
      else
        (heap, expr) // stuck TODO: return type
    // RC-Field
    case FieldAccess(e, f) =>
      val (h1, e1) = execute(heap, e)

      if(h1 == heap && e1 == e) {
        (heap, expr) // stuck
      } else {
        execute(h1, FieldAccess(e1, f)) // recursive call for big-step
      }
    // RC-Call
    case MethodCall(m, e) =>
      val (h1, e1) = execute(heap, e)

      if(h1 == heap && e1 == e) {
        (heap, expr) // stuck
      } else {
        execute(h1, MethodCall(m, e1)) // recursive call for big-step
      }
    // RC-New
    case ObjectConstruction(cls, args) =>
      val (h1, args1) = objectArgumentsInterpreter(heap, args)

      if(h1 == heap && args1 == args) {
        (heap, expr) // stuck
      } else {
        execute(h1, ObjectConstruction(cls, args1)) // recursive call for big-step
      }
  }

  private def objectArgumentsInterpreter(heap: Heap, args: List[(Id, Expression)]): (Heap, List[(Id, Expression)]) = args match {
    case Nil => (heap, Nil)
    case (f, x@Id(_)) :: rst =>
      val (h1, args1) = objectArgumentsInterpreter(heap, rst)
      (h1, (f, x) :: args1)
    case (f, e) :: rst =>
      val (h1, e1) = execute(heap, e)
      val (h2, args1) = objectArgumentsInterpreter(h1, rst)
      (h2, (f, e1) :: args1)
  }

  //private def boundVars(heap: Heap): List[Id] = heap.map{case (x, _) => x}.toList

  // Method Implementation
  private def mImpl(m: Id, x: Id): List[(List[Constraint], Expression)] =
    program.foldRight(Nil: List[(List[Constraint], Expression)]){
      case (MethodImplementation(`m`, `x`, a, _, e), rst) => (a, e) :: rst
      case (_, rst) => rst
    }

  private def mImplSubst(m: Id, x: Id): List[(List[Constraint], Expression)] =
    program.foldRight(Nil: List[(List[Constraint], Expression)]){
      case (MethodImplementation(`m`, xImpl, a, _, e), rst) =>
        (substitute(xImpl, x, a), alphaRename(xImpl, x, e)) :: rst
      case (_, rst) => rst
    }

  private var nameCounter: Int = 0
  private def freshName(): Symbol = {
    nameCounter += 1
    Symbol("x" + nameCounter.toString)
  }

  private def freshVar(): Id = Id(freshName())
}
