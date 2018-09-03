package dcc

import dcc.syntax._
import Util._

class DCC {
  // Class(field = value, ...)
  type Obj = (Id, List[(Id, Id)])
  type Heap = Map[Id, Obj]

  // constraint entailment
  def entails(ctx: List[Constraint], c: Constraint): Boolean = (ctx, c) match {
    // C-Ident
    case (_, _) if ctx.contains(c) => true
    // C-Refl
    case (_, PathEquivalence(p, q)) if p == q => true
    // C-Class
    case (_, InstanceOf(p, cls)) => entails(ctx, InstantiatedBy(p, cls))
    // TODO: really needed to implement this or go directly to smt?
    case (_, _) => false
  }

  def interp(heap: Heap, expr: Expression): (Heap, Expression) = {
    //
    (Map(), 'x)
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