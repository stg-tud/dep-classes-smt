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
  // TODO: add reduction check in RC rules to avoid endless recursion
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
    case MethodCall(m, x@Id(_)) =>
      // Applicable methods
      val S: List[(List[Constraint], Expression)] = mImpl(m, x).filter{case (as, _) => entails(HC(heap), as)}

      // Most specific method
      var (a, e) = S.head

      S.foreach{
        case (a1, e1) if e != e1 =>
          if (entails(a1, a) && !entails(a, a1)) { // TODO: alpha renaming (footnote page 208)
            //(a, e) = (a1, e1)
            a = a1
            e = e1
          }
        case _ => /* noop */
      }

      (heap, e)
    // R-New
    case ObjectConstruction(cls, args)
      if args.foldRight(true){ // if args are values (Id)
        case ((_, Id(_)), rst) => rst // true && rst
        case _ => false // false && rst
      } => // TODO: extend guard with other non-interp prerequisites like entailment? implement body
      val x: Id  = freshvar()
      val args1: List[(Id, Id)] = args.map{case (f, Id(z)) => (f, Id(z))} // case (f, _) => (f, Id('notReduced)) guard makes sure everything is an Id
      val o: Obj = (cls, args1)
      // cls in Program: alpha renaming of y to x in b and orElse stuck/error
      val (y: Id, b: List[Constraint]) = classInProgram(cls, P).getOrElse(return (heap, expr))
      val b1 = alphaConversion(y, x, b)
      // heap constraints entail cls constraints
      if (entails(HC(heap) ++ OC(x, o), b1))
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

  // TODO: change return type to boolean and move Type to the arguments? (to check if expr has type holds and not doing type inference)
  def typeassignment(context: List[Constraint], expr: Expression): Type = expr match {
    // T-Field
    case FieldAccess(e, f) =>
      val Type(x, a) = typeassignment(context, e)

//      val entails1 = entails(context ++ a, InstanceOf(FieldPath(x, f), ???)) // TODO: for all classes (like T-Var) to find suitable class
//      val entails2 = entails(context ++ a :+ PathEquivalence(FieldPath(x, f), y), b) // TODO: how to find y, b -> use Type as argument and return boolean
//      val xFreeInB = !FV(b).contains(x)

      Type(Id('notyetimplemented), List())
    // T-Var
    case x@Id(_) =>
      classes(P).foldRight(Type(Id('tError), List())){ // first class to match wins
        case (cls, _) if entails(context, InstanceOf(x, cls)) =>
          val y = freshvar()
          Type(y, List(PathEquivalence(y, x)))
        case (_, clss) => clss
      }
    // T-Call
    case MethodCall(m, e) => Type(Id('notyetimplemented), List())
    // T-New
    case ObjectConstruction(cls, args) => Type(Id('notyetimplemented), List())
    // T-Sub // TODO: endless recursion?
    case e => Type(Id('notyetimplemented), List())
  }

  // FV: free variables
  // wf P: well formed program
  def typecheck(P: Program): Boolean = false

  private def classInProgram(Cls: Id, p: Program): Option[(Id, List[Constraint])] = p match {
    case Nil => None
    case ConstructorDeclaration(Cls, x, a) :: _ => Some(x, a)
    case _ :: rst => classInProgram(Cls, rst)
  }

  private def classes(p: Program): List[Id] = p match {
    case Nil => Nil
    case ConstructorDeclaration(cls, _, _) :: rst => cls :: classes(rst)
    case _ :: rst => classes(rst)
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
  private def freshname(): Symbol = {
    nameCounter += 1
    Symbol("x" + nameCounter.toString)
  }

  private def freshvar(): Id = Id(freshname())

  private def renameIdInPath(x: Id, y: Id, p: Path): Path = p match {
    case `x` => y
    case z@Id(_) => z
    case FieldPath(q, f) => FieldPath(renameIdInPath(x, y, q), f)
  }

  private def renameIdInConstraint(x: Id, y: Id, c: Constraint): Constraint = c match {
    case PathEquivalence(p, q) => PathEquivalence(renameIdInPath(x, y, p), renameIdInPath(x, y, q))
    case InstanceOf(p, cls) => InstanceOf(renameIdInPath(x, y, p), cls)
    case InstantiatedBy(p, cls) => InstantiatedBy(renameIdInPath(x, y, p), cls)
  }

  private def alphaConversion(x: Id, y: Id, cs: List[Constraint]): List[Constraint] =
    cs.map(renameIdInConstraint(x, y, _))

  // add .distinct to remove duplicates
  private def FV(constraints: List[Constraint]): List[Id] = constraints.flatMap{
    case PathEquivalence(p, q) => varname(p) :: varname(q) :: Nil
    case InstanceOf(p, _) => varname(p) :: Nil
    case InstantiatedBy(p, _) => varname(p) :: Nil
  }

  private def varname(p: Path): Id = p match {
    case x@Id(_) => x
    case FieldPath(q, _) => varname(q)
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
//  val (h1, e1) = dcc.interp(h, FieldAccess(e, Id('p))) // TODO: does not return if e doesnt reduce to Id

  println("Heap:")
  h.foreach(println)
  println("Expr:" + e)
}