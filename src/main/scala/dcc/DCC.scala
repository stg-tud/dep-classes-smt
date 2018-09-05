package dcc

import dcc.syntax.{Expression, _}

class DCC {
  // Class(field = value, ...)
  type Obj = (Id, List[(Id, Id)])
  type Heap = Map[Id, Obj]

  // constraint entailment
  def entails(ctx: List[Constraint], c: Constraint): Boolean = (ctx, c) match {
    // C-Ident
    case _ if ctx.contains(c) => true
    // C-Refl
    case (_, PathEquivalence(p, q)) if p == q => true
    // C-Class
    case (_, InstanceOf(p, cls)) => entails(ctx, InstantiatedBy(p, cls))
    // TODO: really needed to implement this or go directly to smt?
    case (_, _) => false
  }

  // TODO: implement
  def interp(heap: Heap, expr: Expression): (Heap, Expression) = expr match {
    // R-Field
    case FieldAccess(x@Id(_), f) =>
      HC(heap).filter{case PathEquivalence(FieldPath(x, f), Id(_)) => true case PathEquivalence(Id(_), FieldPath(x, f)) => true} match {
        case PathEquivalence(FieldPath(x, f), y@Id(_)) :: rst => (heap, y)
        case PathEquivalence(y@Id(_), FieldPath(x, f)) :: rst => (heap, y)
        case Nil => (heap, expr) // var not bound to proper object?
      }
    // R-Call
    case MethodCall(m, x@Id(_)) => _
    // R-New
    case ObjectConstruction(cls, args) /* if args are values */ => _
    // RC-Field
    case FieldAccess(e, f) =>
      val (h1, e1) = interp(heap, e)
      (h1, FieldAccess(e1, f))
    // RC-Call
    case MethodCall(m, e) =>
      val (h1, e1) = interp(heap, e)
      (h1, MethodCall(m, e1))
    // RC-New
    case ObjectConstruction(cls, args) =>
//      val args1 = objArgsInterp(heap, args)
//      (heap, ObjectConstruction(cls, args1)) // TODO: return updated heap in objargsinterp
      val (h1, args1) = objArgsInterp2(heap, args)
      (h1, ObjectConstruction(cls, args1))
  }

  private def objArgsInterp(heap: Heap, args: List[(Id, Expression)]): List[(Id, Expression)] = args match {
    case Nil => Nil
    case (f, x@Id(_)) :: rst => (f, x) :: objArgsInterp(heap, rst)
    case (f, e) :: rst =>
      val (h1, e1) = interp(heap, e)
      (f, e1) :: objArgsInterp(h1, rst)
  }

  private def objArgsInterp1(heap: Heap, args: List[(Id, Expression)]): List[(Heap, (Id, Expression))] = args match {
    case Nil => Nil
    case (f, x@Id(_)) :: rst => (heap, (f, x)) :: objArgsInterp1(heap, rst)
    case (f, e) :: rst =>
      val (h1, e1) = interp(heap, e)
      (h1, (f, e1)) :: objArgsInterp1(h1, rst)
  }

  private def objArgsInterp2(heap: Heap, args: List[(Id, Expression)]): (Heap, List[(Id, Expression)]) = args match {
    case Nil => (heap, Nil)
    case (f, x@Id(_)) :: rst =>
      val (h1, args1) = objArgsInterp2(heap, rst)
      (h1, (f, x) :: args1)
    case (f, e) :: rst =>
      val (h1, e1) = interp(heap, e)
      val (h2, args1) = objArgsInterp2(h1, rst)
      (h2, (f, e1) :: args1)
  }

  // Heap Constraints
  def HC(heap: Heap): List[Constraint] = heap.map{case (x, o) => OC(x, o)}.flatten.toList

  // Object Constraints
  def OC(x: Id, o: Obj): List[Constraint] = o match {
    case (cls, fields) =>
      val init = InstantiatedBy(x, cls)
      val fieldCs = fields.map{case (f, v) => PathEquivalence(FieldPath(x, f), v)}

      init :: fieldCs
  }

  // TODO: implement
  def freshname(x: Symbol): Symbol = x
}

//object Main extends App {
//  val o: (Id, List[(Id, Id)]) = ('Cls, List(('f, 'x), ('g, 'y)))
//  val h: Map[Id, (Id, List[(Id, Id)])] = Map(Id('z) -> o, Id('zz)->o)
//  val dcc = new DCC
//  println(dcc.HC(h))
//}