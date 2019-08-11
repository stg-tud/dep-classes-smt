package dcc

import dcc.syntax.Program.Program
import dcc.syntax._
import dcc.Util._
import smtlib.solver.{Axioms, Z3Solver}
import smtlib.syntax.{Assert, Not, Sat, Unknown, Unsat}

class DCC(P: Program) {
  // Class(field = value, ...)
  type Obj = (Id, List[(Id, Id)])
  type Heap = Map[Id, Obj]

  def Entails(ctx: List[Constraint], cs: List[Constraint], vars: List[Id], skipNoSubst: Boolean = true): Boolean =
    cs.forall(c => entails(ctx, c, vars, skipNoSubst = skipNoSubst))

  // constraint entailment
  def entails(context: List[Constraint], c: Constraint, vars: List[Id], skipNoSubst: Boolean = true): Boolean = {
    // pre optimization TODO: kinda dirty here: move to somewhere else?
    // TODO: add further optimizations for path eqs?
//    val context1 = context
    var context1 = context.map{
      case InstantiatedBy(p, cls) => InstanceOf(p, cls)
      case d => d
    }
    var c1 = c
    c match {
      case InstanceOf(p, cls) => context1.foreach{
        case eq@PathEquivalence(`p`, q) =>
          c1 = InstanceOf(q, cls)
          context1 = context1.filter{ // TODO: first apply the equiv over the context before removing?
            case `eq` => false
            case _ => true
          }
        case eq@PathEquivalence(q, `p`) =>
          c1 = InstanceOf(q, cls)
          context1 = context1.filter{
            case `eq` => false
            case _ => true
          }
        case _ =>
      }
      case _ =>
    }

    // debug output
    context1 match {
      case Nil => println(s"ϵ |- $c1")
      case ctx  => println(s"${syntax.Util.commaSeparate(ctx.distinct)} |- $c1")
    }

    (context1.distinct, c) match {
      // C-Ident (with weakening)
      case _ if context.contains(c) => true
      // C-Refl (with weakening)
      case (_, PathEquivalence(p, q)) if p == q => true
      case (ctx, _) =>

        val entailment = SMTLibConverter.convertEntailment(ctx, c1)

        // TODO: remove variables from this monstrous beast, convert them from added argument vars
        // TODO: alternatively
        // TODO: maybe change List[String] for vars to List[Id], such that it is usable by generateSubstRules
        // TODO:   then remove argument vars from entails function again
        val (strs, pths, clss) = SMTLibConverter.extractVariablesPathsClasses(c1 :: ctx)
        val (variables, paths, classes) = SMTLibConverter.convertVariablesPathsClasses(strs, pths, clss)

        val lookup = SMTLibConverter.makeProgramEntailmentLookupFunction(P, pths)
        val substRulesPruned = SMTLibConverter.generateSubstRules(vars, pths, pruning = true, skipNoSubst = skipNoSubst)

        // debug output
//        substRulesPruned.foreach(c => println(c.format()))
//        println(lookup.format())
//        variables.foreach(c => println(c.format()))
//        paths.foreach(c => println(c.format()))
//        classes.foreach(c => println(c.format()))
//        println(entailment.format())

        val solver = new Z3Solver(Axioms.allDirectClosure, debug=false)

        solver.addCommands(substRulesPruned)
        solver.addCommand(lookup)
        solver.addCommand(Axioms.cProg)
        solver.addCommands(SMTLibConverter.makeAsserts(classes))
        solver.addCommands(SMTLibConverter.makeAsserts(paths))
        solver.addCommands(SMTLibConverter.makeAsserts(variables))
        // TODO: check if not entailment is unsat or entailment is sat?
        solver.addCommand(Assert(Not(entailment)))
//        solver.addCommand(Assert(entailment))

        val sat = solver.checksat(3000)

        println(s"\t${sat.format()}")

        sat match {
          case Sat => false
          case Unsat => true
          case Unknown =>
            solver.flush() // Second round without pruning
            solver.addCommands(SMTLibConverter.generateSubstRules(vars, pths, skipNoSubst = skipNoSubst))
            solver.addCommand(lookup)
            solver.addCommand(Axioms.cProg)
            solver.addCommands(SMTLibConverter.makeAsserts(classes))
            solver.addCommands(SMTLibConverter.makeAsserts(paths))
            solver.addCommands(SMTLibConverter.makeAsserts(variables))
            solver.addCommand(Assert(Not(entailment)))

            val sat2 = solver.checksat()
            println(s"\t${sat2.format()}")

            sat2 match {
              case Unsat => true
              case _ => false
            }
        }
    }
  }

  // TODO: change return type to Either or Option?
  def interp(heap: Heap, expr: Expression): (Heap, Expression) = expr match {
    case x@Id(_) => (heap, expr) // variables are values
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
      val vars = boundVars(heap)

      // Applicable methods
      //val S: List[(List[Constraint], Expression)] = mImplSubst(m, x).filter{case (as, _) => entails(HC(heap), as, vars)}
      val methods = mImplSubst(m, x)
      val S = methods.filter{case (as, _) => Entails(HC(heap), as, vars)}

      if (S.isEmpty) // m not in P
        return (heap, expr)

      println("Applicable:")
      S.foreach(println)

      // Most specific method
      var (a, e) = S.head

      S.foreach{
        case (a1, e1) if e != e1 =>
          if (Entails(a1, a, vars) && !Entails(a, a1, vars)) {
            println(s"Most specific:$a1, $e1")
            //(a, e) = (a1, e1)
            a = a1
            e = e1
          }
        case _ => /* noop */
      }

      // TODO: interp correct? we dont want intermediate results
      interp(heap, e)
    // R-New
    case ObjectConstruction(cls, args)
      if args.forall{ // if args are values (Id)
        case (_, Id(_)) => true
        case _ => false
      } =>
      val vars = boundVars(heap)
      val x: Id  = freshvar()
      //val args1: List[(Id, Id)] = args.map{case (f, Id(z)) => (f, Id(z))} // case (f, _) => (f, Id('notReduced)) guard makes sure everything is an Id
      val o: Obj = (cls, args.asInstanceOf[List[(Id, Id)]])
      // cls in Program: alpha renaming of y to x in b and orElse stuck/error
      val (y: Id, b: List[Constraint]) = classInProgram(cls, P).getOrElse(return (heap, expr))
      //val b1 = alphaConversion(y, x, b)
      val b1 = substitute(y, x, b)
      // heap constraints entail cls constraints
      if (Entails(HC(heap) ++ OC(x, o), b1, x :: vars))
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
  def _typeassignment(context: List[Constraint], expr: Expression): Type = expr match {
    // T-Field
    case FieldAccess(e, f) =>
      val Type(x, a) = _typeassignment(context, e)

//      val entails1 = entails(context ++ a, InstanceOf(FieldPath(x, f), ???)) // TODO: for all classes (like T-Var) to find suitable class
//      val entails2 = entails(context ++ a :+ PathEquivalence(FieldPath(x, f), y), b) // TODO: how to find y, b -> use Type as argument and return boolean (see typeassignment1)
//      val xFreeInB = !FV(b).contains(x)

      Type(Id('notyetimplemented), List())
    // T-Var
    case x@Id(_) =>
      classes.foldRight(Type(Id('tError), List())){ // first class to match wins
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
  def _typeassignment1(context: List[Constraint], expr: Expression, t: Type): Boolean = expr match {
    // T-Field
    case FieldAccess(e, f) =>
      val Type(x, a) = _typeassignment(context, e)
      // TODO: typeassignment1(context, e, Type(???, ???)) how to find x, y -> use type as return value (see typeassignment)
      val y = t.x
      val b = t.constraints

      !FV(b).contains(x) &&
      Entails(PathEquivalence(FieldPath(x, f), y) :: context ++ a, b, Nil) &&
      classes.foldRight(false){
        case (cls, _) if entails(context ++ a, InstanceOf(FieldPath(x, f), cls), Nil) => true
        case (_, clss) => clss
      }
    // T-Var
    case x@Id(_) =>
      t.constraints.size == 1 &&
        (t.constraints.head == PathEquivalence(t.x, x) ||
         t.constraints.head == PathEquivalence(x, t.x)) &&
      classes.foldRight(false){ // first class to match wins
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

  def typeassignment(context: List[Constraint], expr: Expression, skipGen: Boolean = true): List[Type] = expr match {
    // T-Var
    case x@Id(_) =>
      //classes(P).foldRight(List(Type(Id('tError), List(PathEquivalence(x, Id('noValidClass)))))){
      classes.foldRight(Nil: List[Type]) {
        case (cls, _) if entails(context, InstanceOf(x, cls), List(x), skipNoSubst = skipGen) =>
          val y = freshvar()
//          Type(y, List(PathEquivalence(y, x))) :: Nil// :: clss TODO: no need to find another one, as the type would be the same (after renaming)
          List(
            Type(y, List(PathEquivalence(y, x)))
            ,Type(y, List(PathEquivalence(x, y)))
          )
        case (_, clss) => clss
      } match {
        case Nil => List(Type(Id('tError), List(PathEquivalence(x, Id('noValidClass)))))
        case l => l
      }
    // T-Field
    case FieldAccess(e, f) =>
      val types = typeassignment(context, e)
      val y = freshvar()

      var ts: List[Type] = Nil
      types.foreach {
        case Type(x, a) =>
          // instance of relations for type constraints
          val instOfs = classes.foldRight(Nil: List[Constraint]) { // TODO: list of vars in entails, like T-Var case
            case (cls, clss) if entails(context ++ a, InstanceOf(FieldPath(x, f), cls), List(x), skipNoSubst = skipGen) =>
              val c = InstanceOf(y, cls)
              ts = Type(y, List(c)) :: ts
              c :: clss
            case (_, clss) => clss
          }

        // other possible constraints for typing (excluding already used instance of relations)
        //          val b: List[Constraint] = ??? // TODO: generate possible constraints
        //
        //          if (entails(PathEquivalence(FieldPath(x, f), y) :: context ++ a, b, Nil))
        //            ts = Type(y, b) :: ts
      }
      ts.distinct
    // T-Call
    case MethodCall(m, e) =>
      val eTypes = typeassignment(context, e)
      val y = freshvar()

      var types: List[Type] = Nil

      // for all possible argument types
      for (Type(x, a) <- eTypes) {
        // for all method declarations
        for ((a1, b) <- mTypeSubst(m, x, y)) {
          val entailsArgs = Entails(context ++ a, a1, List(x), skipNoSubst = skipGen)

          val b1 = (a1 ++ b).foldRight(Nil: List[Constraint]) {
            case (c, cs) if !FV(c).contains(x) => c :: cs
            case (_, cs) => cs
          }

          if (entailsArgs && Entails(context ++ a ++ b, b1, List(y), skipNoSubst = skipGen))
            types = Type(y, b1) :: types
        }
      }

      types
    // T-New
    case ObjectConstruction(cls, args) =>
      /* TODO: the rule could use an overhaul:
      *   - move classInProgram(cls, P) out of the match, no need to look it up multiple times
      *   - subtyping check with classes.foreach could also use a rearrangement
      *   - all in all: make it less self-repeating between both cases */
      val fields: List[Id] = args.map(_._1)
      val argsTypes: List[List[Type]] = args.map(arg => typeassignment(context, arg._2))
      //val argsTypes: List[(Id, List[Type])] = args.map{case (f, e) => (f, typeassignment(context, e))}

      val x = freshvar()
      var types: List[Type] = Nil

      argsTypes match {
        case Nil =>
          val b = List(InstantiatedBy(x, cls))
          val (x1, b1) = classInProgram(cls, P).getOrElse(return List(Type(Id('tError), List(PathEquivalence(cls, Id('classNotFound))))))

          if (Entails(context ++ b, b1, List(x, x1), skipNoSubst = skipGen))
            types = Type(x, b) :: types

          classes.foreach{
            c =>
              if (entails(context ++ b, InstanceOf(x, c), List(x), skipNoSubst = skipGen))
                types = Type(x, List(InstanceOf(x, c))) :: types
          }
        case _ => combinations(argsTypes).foreach {
          argsType =>
            val argsPairs: List[(Id, Type)] = fields.zip(argsType)
            val argsConstraints: List[Constraint] = argsPairs.flatMap{
              case (fi, Type(xi, ai)) => substitute(xi, FieldPath(x, fi), ai)
            }

            val b: List[Constraint] = InstantiatedBy(x, cls) :: argsConstraints

            val (x1, b1) = classInProgram(cls, P).getOrElse(return List(Type(Id('tError), List(PathEquivalence(cls, Id('classNotFound))))))

            if (Entails(context ++ b, substitute(x1, x, b1), List(x), skipNoSubst = skipGen))
              types = Type(x, b) :: types

            classes.foreach{
              c =>
                if (entails(context ++ b, InstanceOf(x, c), List(x), skipNoSubst = skipGen))
                  types = Type(x, List(InstanceOf(x, c))) :: types
            }
        }
      }
      types.distinct
  }

  // FV: free variables
  // wf P: well formed program
  def typecheck(skipGen: Boolean = true): Boolean = {
    val x = freshvar()
    val y = freshvar()

    methods.forall { m =>
      val mTypes = mTypeSubst(m, x, y)

      // TODO: should be sufficient to check for mTypes.head if all mTypes are equal,
      //  since == is transitive?
      mTypes.forall {
        case (a, b) =>
          mTypes.forall {
            case (a1, b1) =>
              b.size == b1.size &&
              b.forall(c => b1.contains(c))
          }
      }
    } && P.forall(typecheckDecl(_, skipGen))
  }

  def typecheckDecl(D: Declaration, skipGen: Boolean = true): Boolean = D match {
    // WF-CD
    case ConstructorDeclaration(cls, x, a) => FV(a) == List(x) || FV(a).isEmpty
    // WF-RD
    case ConstraintEntailment(x, a, InstanceOf(y, _)) if x == y =>
      FV(a) == List(x) && a.exists {
        case InstanceOf(`x`, _) => true
        case _ => false
      }
    // WF-MS
    case AbstractMethodDeclaration(_, x, a, Type(y, b)) =>
      val vars = FV(b) // TODO: check if x != y for size check?
      FV(a) == List(x) && vars.nonEmpty && vars.forall(v => v == x || v == y)
    // WF-MI
    case MethodImplementation(_, x, a, Type(y, b), e) =>
      val vars = FV(b) // TODO: check if x != y for size check?
      FV(a) == List(x) && vars.nonEmpty && vars.forall(v => v == x || v == y) &&
      typeassignment(a, e, skipGen).exists {
        case Type(z, c) =>
          c.size == b.size &&
          substitute(z, y, c).forall(b.contains(_))
      }
  }

  private def classInProgram(Cls: Id, p: Program): Option[(Id, List[Constraint])] = p match {
    case Nil => None
    case ConstructorDeclaration(Cls, x, a) :: _ => Some(x, a)
    case _ :: rst => classInProgram(Cls, rst)
  }

  private def classes: List[Id] = P.flatMap(className).distinct

  // TODO: add search for instance of for constraint entailment second param?
  // TODO: search for class names in all constraints?
  private def className(d: Declaration): Option[Id] = d match {
    case ConstructorDeclaration(cls, _, _) => Some(cls)
    case ConstraintEntailment(_, _, InstanceOf(_, cls)) => Some(cls)
    case _ => None
  }

  private def methods: List[Id] = P.flatMap(methodName).distinct

  private def methodName(d: Declaration): Option[Id] = d match {
    case AbstractMethodDeclaration(m, _, _, _) => Some(m)
    case MethodImplementation(m, _, _, _, _) => Some(m)
    case _ => None
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
      case (MethodImplementation(`m`, `x`, a, Type(`y`, b), _), rst) => (a, b) :: rst
      case (_, rst) => rst}

  // MType where the bound variables of declared argument and return type constraints are
  // substituted with given variables
  private def mTypeSubst(m: Id, x: Id, y: Id): List[(List[Constraint], List[Constraint])] =
    P.foldRight(Nil: List[(List[Constraint], List[Constraint])]){
      case (AbstractMethodDeclaration(`m`, xDecl, a, Type(yDecl, b)), rst) =>
        (substitute(xDecl, x, a), substitute(yDecl, y, b)) :: rst
      case (MethodImplementation(`m`, xImpl, a, Type(yImpl, b), _), rst) =>
        (substitute(xImpl, x, a), substitute(yImpl, y, b)) :: rst
      case (_, rst) => rst}

  // Method Implementation
  private def mImpl(m: Id, x: Id): List[(List[Constraint], Expression)] =
    P.foldRight(Nil: List[(List[Constraint], Expression)]){
      case (MethodImplementation(`m`, `x`, a, _, e), rst) => (a, e) :: rst
      case (_, rst) => rst
    }

  private def mImplSubst(m: Id, x: Id): List[(List[Constraint], Expression)] =
    P.foldRight(Nil: List[(List[Constraint], Expression)]){
      case (MethodImplementation(`m`, xImpl, a, _, e), rst) =>
        (substitute(xImpl, x, a), alphaRename(xImpl, x, e)) :: rst
      case (_, rst) => rst
    }

  private var nameCounter: Int = 0
  private def freshname(): Symbol = {
    nameCounter += 1
    Symbol("x" + nameCounter.toString)
  }

  private def freshvar(): Id = Id(freshname())

  // add .distinct to remove duplicates
  private def FV(constraints: List[Constraint]): List[Id] = constraints.flatMap(FV).distinct

  private def FV(constraint: Constraint): List[Id] = constraint match {
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

  /**
    * Generates all possible combinations of a given list of lists
    * @param list The list of lists for which to generate combinations
    * @return All possible combinations for the given input
    */
  private def combinations(list: List[List[Type]]): List[List[Type]] = {
    _combs = List()
    combine(list, List())
    _combs
  }

  private var _combs: List[List[Type]] = List()
  private def combine(list: List[List[Type]], accum: List[Type]): Unit = list match {
    case Nil =>
    case last :: Nil => last.foreach(elem => _combs = (accum ++ List(elem)) :: _combs)
    case hd :: tl => hd.foreach(elem => combine(tl, accum ++ List(elem)))
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

//  naturalNumbers.foreach(println)

  val dcc = new DCC(naturalNumbers)

//  val (h, e) = dcc.interp(Map.empty, ObjectConstruction(Id('Zero), Nil))
//  val (h1, e1) = dcc.interp(h, ObjectConstruction(Id('Succ), List((Id('p), e))))
//  val e1 = Id('x2)
//  val h1 = h + (e1 -> (Id('Succ), List((Id('p), e.asInstanceOf[Id]))))
//  val (h2, e2) = dcc.interp(h, MethodCall(Id('prev), e))

//  println("Heap:")
//  h.foreach(println)
//  println("Expr:" + e)
//  println("Heap1:")
//  h1.foreach(println)
//  println("Expr1:" + e1)
//  println("Heap2:")
//  h2.foreach(println)
//  println("Expr2:" + e2)

  val xp = dcc.typeassignment(
    List(
      InstanceOf(Id('x), Id('Succ))
    , InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat))
//    , InstanceOf(Id('y), Id('Zero))
//    , PathEquivalence(FieldPath(Id('x), Id('p)), Id('y))
    ),
    FieldAccess(Id('x), Id('p)))
//    Id('x))

  println(xp.size)
  xp.foreach(println)


//  val types = dcc.typeassignment(
//    List(
//      InstanceOf(Id('x), Id('Zero))
//      //    , InstanceOf(Id('y), Id('Zero))
//      //    , PathEquivalence(FieldPath(Id('x), Id('p)), Id('y))
//    ),
//    ObjectConstruction(Id('Zero), List()))
//
//  println(types.size)
//  types.foreach(println)

//  println(dcc.typecheck(naturalNumbers))

  // subst with prog does time out for some reason
//  dcc.entails(
//    List(
//      InstanceOf(Id('x), Id('Succ)),
//      InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat)),
//      PathEquivalence(Id('y), Id('x))
//    ),
//    InstanceOf(Id('y), Id('Nat)), List())
}

//combinations = []
//
//def combine(terms, accum):
//    last = (len(terms) == 1)
//    n = len(terms[0])
//    for i in range(n):
//        item = accum + terms[0][i]
//        if last:
//            combinations.append(item)
//        else:
//            combine(terms[1:], item)

object Foo extends App {
  val l: List[List[Int]] = List(List(1, 2), List(3, 4), List(5))
  //    for (i <- l.head.indices) {
  //      for (j <- l(1).indices) {
  //        for (m <- l(2).indices) {
  //          k = List(l.head(i), l(1)(j), l(2)(m)) :: k
  //        }
  //      }
  //    }

  var k: List[List[Int]] = List()
  def gen(terms: List[List[Int]], accum: List[Int]): Unit = {
    val last = terms.size == 1
    val n = terms.head
    for (i <- n.indices) {
      val item = accum ++ List(terms.head(i))
      if (last) {
        k = item :: k
      } else {
        gen(terms.tail, item)
      }
    }
  }

  var p: List[List[Int]] = List()
  def genS(terms: List[List[Int]], accum: List[Int]): Unit = terms match {
    case Nil =>
    case last :: Nil =>
      last.foreach(
        elem => p = (accum ++ List(elem)) :: p
      )
    case hd :: tl =>
      hd.foreach(
        elem => genS(tl, accum ++ List(elem))
      )
  }

  private def combinations(list: List[List[Int]]): List[List[Int]] = {
    _combs = List()
    combine(list, List())
    _combs
  }

  private var _combs: List[List[Int]] = List()
  private def combine(list: List[List[Int]], accum: List[Int]): Unit = list match {
    case Nil =>
    case last :: Nil => last.foreach(elem => _combs = (accum ++ List(elem)) :: _combs)
    case hd :: tl => hd.foreach(elem => combine(tl, accum ++ List(elem)))
  }

//  gen(l)

  gen(l, List())
  genS(l, List())
  combinations(l)

  k.reverse.foreach(println)

  println(k == p && p == _combs)
}