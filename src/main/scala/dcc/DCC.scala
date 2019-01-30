package dcc

import dcc.syntax.Program.Program
import dcc.syntax._
import smtlib.solver.{Axioms, Z3Solver}
import smtlib.syntax.{Assert, Not, Sat, Term, Unknown, Unsat}

class DCC(P: Program) {
  // Class(field = value, ...)
  type Obj = (Id, List[(Id, Id)])
  type Heap = Map[Id, Obj]

  def entails(ctx: List[Constraint], cs: List[Constraint]): Boolean =
    cs.forall(c => entails(ctx, c))
    //cs.map(c => entails(ctx, c)).fold(true){_ && _}

  // constraint entailment
  def entails(ctx: List[Constraint], c: Constraint): Boolean = {
    // debug output
//    ctx match {
//      case Nil => println(s"ϵ |- $c")
//      case _::Nil => println(s"$ctx |- $c")
//      case _ =>
//        ctx.foreach(println)
//        println(s"|- $c")
//    }

    (ctx, c) match {
      // C-Ident (with weakening)
      case _ if ctx.contains(c) => true
      // C-Refl (with weakening)
      case (_, PathEquivalence(p, q)) if p == q => true
      case (_, _) =>
        val entailment = SMTLibConverter.convertEntailment(ctx, c)
        val programEntailments: List[Term] = SMTLibConverter.convertProgramEntailments(P)
        val (variables, paths, classes) = SMTLibConverter.convertVariablesPathsClasses(c :: ctx)

        // debug output
//        println(entailment.format())
//        programEntailments.foreach(c => println(c.format()))
//        variables.foreach(c => println(c.format()))
//        paths.foreach(c => println(c.format()))
//        classes.foreach(c => println(c.format()))

        val solver = new Z3Solver(Axioms.all, debug=false)

        solver.addCommands(SMTLibConverter.makeAsserts(variables))
        solver.addCommands(SMTLibConverter.makeAsserts(paths))
        solver.addCommands(SMTLibConverter.makeAsserts(classes))
        solver.addCommands(SMTLibConverter.makeAsserts(programEntailments))
        // TODO: check if not entailment is unsat or entailment is sat?
        solver.addCommand(Assert(Not(entailment)))

        val sat = solver.checksat()

        sat match {
          case Sat => false
          case Unsat => true
          case Unknown => false
        }
    }
  }

  // TODO: change return type to Either or Option?
  def interp(heap: Heap, expr: Expression): (Heap, Expression) = expr match {
    // R-Field
    case FieldAccess(X@Id(_), F@Id(_)) =>
      HC(heap).filter{
          case PathEquivalence(FieldPath(X, F), Id(_)) => true
          case PathEquivalence(Id(_), FieldPath(X, F)) => true
          case _ => false
          } match {
        case PathEquivalence(FieldPath(X, F), y@Id(_)) :: _ => (heap, y)
        case PathEquivalence(y@Id(_), FieldPath(X, F)) :: _ => (heap, y)
        case _ => (heap, expr) // x does not have field f TODO: return type
      }
    // R-Call
    case MethodCall(m, x@Id(_)) => (heap, expr)
//      val S: List[Any] = ??? // TODO: list type: m-impl

      // todo
    // R-New
    case ObjectConstruction(cls, args)
      if args.foldRight(true){ // if args are values (Id)
        case ((_, Id(_)), rst) => rst // true && rst
        case _ => false // false && rst
      } => // TODO: extend guard with other non-interp prerequisites like entailment? implement body
      val x: Id  = Id(freshname())
      val args1: List[(Id, Id)] = args.map{case (f, Id(z)) => (f, Id(z))} // case (f, _) => (f, Id('notReduced)) guard makes sure everything is an Id
      val o: Obj = (cls, args1)
      // cls in Program
      val (y: Id, b: List[Constraint]) = classInProgram(cls, P).getOrElse(return (heap, expr)) // TODO: alpha renaming of y to x in b and orElse stuck/error
      // heap constraints entail cls constraints
      if (entails(HC(heap) ++ OC(x, o), b))
        (heap + (x -> o), x)
      else
        (heap, expr) // stuck TODO: return type
    // RC-Field
    case FieldAccess(e, f) =>
      val (h1, e1) = interp(heap, e)
      interp(h1, FieldAccess(e1, f)) // recursive call for big-step
    // RC-Call
    case MethodCall(m, e) =>
      val (h1, e1) = interp(heap, e)
      interp(h1, MethodCall(m, e1)) // recursive call for big-step
    // RC-New
    case ObjectConstruction(cls, args) =>
      val (h1, args1) = objArgsInterp(heap, args)
      interp(h1, ObjectConstruction(cls, args1)) // recursive call for big-step
  }

  private def classInProgram(Cls: Id, p: Program): Option[(Id, List[Constraint])] = p match {
    case Nil => None
    case ConstructorDeclaration(Cls, x, a) :: _ => Some(x, a)
    case _ :: rst => classInProgram(Cls, rst)
  }

  private def objArgsInterp(heap: Heap, args: List[(Id, Expression)]): (Heap, List[(Id, Expression)]) = args match {
    case Nil => (heap, Nil)
    case (f, x@Id(_)) :: rst =>
      val (h1, args1) = objArgsInterp(heap, rst)
      (h1, (f, x) :: args1)
    case (f, e) :: rst =>
      val (h1, e1) = interp(heap, e)
      val (h2, args1) = objArgsInterp(h1, rst)
      (h2, (f, e1) :: args1)
  }

  // Heap Constraints
  def HC(heap: Heap): List[Constraint] = heap.flatMap{case (x, o) => OC(x, o)}.toList
    //heap.map{case (x, o) => OC(x, o)}.flatten.toList

  // Object Constraints
  def OC(x: Id, o: Obj): List[Constraint] = o match {
    case (cls, fields) =>
      val init = InstantiatedBy(x, cls)
      val fieldCs = fields.map{case (f, v) => PathEquivalence(FieldPath(x, f), v)}

      init :: fieldCs
  }

  // Method Type
  def mType(m: Id, x: Id, y: Id): List[(List[Constraint], List[Constraint])] =
    P.foldRight(Nil: List[(List[Constraint], List[Constraint])]){
      case (AbstractMethodDeclaration(`m`, `x`, a, Type(`y`, b)), rst) => (a, b) :: rst
      case (_, rst) => rst}

  // Method Implementation
  def mImpl(m: Id, x: Id): List[(List[Constraint], Expression)] =
    P.foldRight(Nil: List[(List[Constraint], Expression)]){
      case (MethodImplementation(`m`, `x`, a, _, e), rst) => (a, e) :: rst
      case (_, rst) => rst
    }

  private var nameCounter: Int = 0
  def freshname(): Symbol = {
    nameCounter += 1
    Symbol("x" + nameCounter.toString)
  }
}

import Util._
object Main extends App {
  val naturalNumbers: Program = List(
    ConstructorDeclaration(Id('Zero), Id('x), Nil),
    ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Zero))), InstanceOf(Id('x), Id('Nat))),
    ConstructorDeclaration(Id('Succ), Id('x), List(InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat)))),
    ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Succ)), InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat))), InstanceOf(Id('x), Id('Nat))),
    AbstractMethodDeclaration(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Nat))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat))))),
    MethodImplementation(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Zero))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat)))),
      ObjectConstruction(Id('Zero), Nil)),
    MethodImplementation(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Succ)), InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat)))),
      FieldAccess(Id('x), Id('p)))
  )

  naturalNumbers.foreach(println)

  val dcc = new DCC(naturalNumbers)

  //val (h, e) = dcc.interp(Map.empty, ObjectConstruction(Id('Zero), Nil))
  val (h, e) = dcc.interp(Map.empty, ObjectConstruction(Id('Succ), List((Id('p), ObjectConstruction(Id('Zero), Nil)))))

  println("Heap:")
  h.foreach(println)
  println("Expr:" + e)
}