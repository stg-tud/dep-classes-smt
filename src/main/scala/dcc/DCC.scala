package dcc

import dcc.syntax.Program.Program
import dcc.syntax._
import dcc.Util._
import smtlib.solver.{Axioms, Z3Solver}
import smtlib.syntax.{Assert, Not, Sat, Term, Unknown, Unsat}

class DCC(P: Program) {
  // Class(field = value, ...)
  type Obj = (Id, List[(Id, Id)])
  type Heap = Map[Id, Obj]

  def entails(ctx: List[Constraint], cs: List[Constraint], vars: List[Id]): Boolean =
    cs.forall(c => entails(ctx, c, vars))

  // constraint entailment
  def entails(context: List[Constraint], c: Constraint, vars: List[Id]): Boolean = {
    // debug output
//    ctx match {
//      case Nil => println(s"ϵ |- $c")
//      case _::Nil => println(s"$ctx |- $c")
//      case _ =>
//        ctx.foreach(println)
//        println(s"|- $c")
//    }

    (context.distinct, c) match {
      // C-Ident (with weakening)
      case _ if context.contains(c) => true
      // C-Refl (with weakening)
      case (_, PathEquivalence(p, q)) if p == q => true
      case (ctx, _) =>
        val entailment = SMTLibConverter.convertEntailment(ctx, c)
        // TODO: remove variables from this monstrous beast, convert them from added argument vars
        val (variables, paths, classes) = SMTLibConverter.convertVariablesPathsClasses(c :: ctx)

        val programEntailments: List[Term] = SMTLibConverter.instantiateProgramEntailments(P, vars)

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
    case MethodCall(m, x@Id(_)) =>
      val vars = boundVars(heap)

      // Applicable methods
      val S: List[(List[Constraint], Expression)] = mImpl(m, x).filter{case (as, _) => entails(HC(heap), as, vars)}

      // Most specific method
      var (a, e) = S.head

      S.foreach{
        case (a1, e1) if e != e1 =>
          if (entails(a1, a, vars) && !entails(a, a1, vars)) { // TODO: alpha renaming (footnote page 208)
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
      } =>
      val vars = boundVars(heap)
      val x: Id  = freshvar()
      val args1: List[(Id, Id)] = args.map{case (f, Id(z)) => (f, Id(z))} // case (f, _) => (f, Id('notReduced)) guard makes sure everything is an Id
      val o: Obj = (cls, args1)
      // cls in Program: alpha renaming of y to x in b and orElse stuck/error
      val (y: Id, b: List[Constraint]) = classInProgram(cls, P).getOrElse(return (heap, expr))
      val b1 = alphaConversion(y, x, b)
      // heap constraints entail cls constraints
      if (entails(HC(heap) ++ OC(x, o), b1, x :: vars))
        (heap + (x -> o), x)
      else
        (heap, expr) // stuck TODO: return type
    // RC-Field
    case FieldAccess(e, f) =>
      val (h1, e1) = interp(heap, e)

      if(h1 == heap && e1 == e) {
        (heap, expr) // stuck
      } else {
        interp(h1, FieldAccess(e1, f)) // recursive call for big-step
      }
    // RC-Call
    case MethodCall(m, e) =>
      val (h1, e1) = interp(heap, e)

      if(h1 == heap && e1 == e) {
        (heap, expr) // stuck
      } else {
        interp(h1, MethodCall(m, e1)) // recursive call for big-step
      }
    // RC-New
    case ObjectConstruction(cls, args) =>
      val (h1, args1) = objArgsInterp(heap, args)

      if(h1 == heap && args1 == args) {
        (heap, expr) // stuck
      } else {
        interp(h1, ObjectConstruction(cls, args1)) // recursive call for big-step
      }
  }

  // TODO: change return type to boolean and move Type to the arguments? (to check if expr has type holds and not doing type inference)
  def typeassignment(context: List[Constraint], expr: Expression): Type = expr match {
    // T-Field
    case FieldAccess(e, f) =>
      val Type(x, a) = typeassignment(context, e)

//      val entails1 = entails(context ++ a, InstanceOf(FieldPath(x, f), ???)) // TODO: for all classes (like T-Var) to find suitable class
//      val entails2 = entails(context ++ a :+ PathEquivalence(FieldPath(x, f), y), b) // TODO: how to find y, b -> use Type as argument and return boolean (see typeassignment1)
//      val xFreeInB = !FV(b).contains(x)

      Type(Id('notyetimplemented), List())
    // T-Var
    case x@Id(_) =>
      classes(P).foldRight(Type(Id('tError), List())){ // first class to match wins
        case (cls, _) if entails(context, InstanceOf(x, cls), Nil) => // TODO: replace Nil with vars
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

  // TODO: replace Nil with vars in entails
  def typeassignment1(context: List[Constraint], expr: Expression, t: Type): Boolean = expr match {
    // T-Field
    case FieldAccess(e, f) =>
      val Type(x, a) = typeassignment(context, e)
      // TODO: typeassignment1(context, e, Type(???, ???)) how to find x, y -> use type as return value (see typeassignment)
      val y = t.x
      val b = t.constraints

      !FV(b).contains(x) &&
      entails(PathEquivalence(FieldPath(x, f), y) :: context ++ a, b, Nil) &&
      classes(P).foldRight(false){
        case (cls, _) if entails(context ++ a, InstanceOf(FieldPath(x, f), cls), Nil) => true
        case (_, clss) => clss
      }
    // T-Var
    case x@Id(_) =>
      t.constraints.size == 1 &&
        (t.constraints.head == PathEquivalence(t.x, x) ||
         t.constraints.head == PathEquivalence(x, t.x)) &&
      classes(P).foldRight(false){ // first class to match wins
        case (cls, _) if entails(context, InstanceOf(x, cls), Nil) => true
        case (_, clss) => clss
      }
    // T-Call TODO
    case MethodCall(m, e) => false
    // T-New TODO
    case ObjectConstruction(cls, args) => false
    // T-Sub // TODO: endless recursion?
    case e => false
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
  private def HC(heap: Heap): List[Constraint] = heap.flatMap{case (x, o) => OC(x, o)}.toList
    //heap.map{case (x, o) => OC(x, o)}.flatten.toList

  // Object Constraints
  private def OC(x: Id, o: Obj): List[Constraint] = o match {
    case (cls, fields) =>
      val init = InstantiatedBy(x, cls)
      val fieldCs = fields.map{case (f, v) => PathEquivalence(FieldPath(x, f), v)}

      init :: fieldCs
  }

  // Method Type
  private def mType(m: Id, x: Id, y: Id): List[(List[Constraint], List[Constraint])] =
    P.foldRight(Nil: List[(List[Constraint], List[Constraint])]){
      case (AbstractMethodDeclaration(`m`, `x`, a, Type(`y`, b)), rst) => (a, b) :: rst
      case (_, rst) => rst}

  // Method Implementation
  private def mImpl(m: Id, x: Id): List[(List[Constraint], Expression)] =
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

  private def boundVars(heap: Heap): List[Id] = heap.map{case (x, _) => x}.toList
  // heap.foldRight(Nil: List[Id]){case (elem, rst) => elem._1 :: rst}
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
  val (h1, e1) = dcc.interp(h, FieldAccess(e, Id('p)))

  println("Heap:")
  h1.foreach(println)
  println("Expr:" + e1)
}