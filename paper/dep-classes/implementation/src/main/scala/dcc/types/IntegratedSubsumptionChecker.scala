package dcc.types
import dcc.DCC.{FV, classInProgram}
import dcc.Util.substitute
import dcc.entailment.Entailment
import dcc.syntax.Program.Program
import dcc.syntax.{AbstractMethodDeclaration, Constraint, ConstraintEntailment, ConstructorDeclaration, Declaration, Expression, FieldAccess, FieldPath, Id, InstanceOf, InstantiatedBy, MethodCall, MethodImplementation, ObjectConstruction, PathEquivalence}

class IntegratedSubsumptionChecker(override val program: Program, entailment: Entailment) extends Checker {
  override def typeOf(context: List[Constraint], expression: Expression): Either[Type, String] = typeAssignment(context, expression) match {
    case t :: _ => Left(t)
    case Nil => Right(s"could not assign a type for $expression")
  }

  override def typecheck(context: List[Constraint], expression: Expression, typ: Type): Boolean = typeAssignment(context, expression) exists {
    case Type(z, c) =>
      c.size == typ.constraints.size &&
        (substitute(z, typ.x, c) forall typ.constraints.contains)
  }

  override def typecheck: Boolean = {
    val x = freshVariable()
    val y = freshVariable()

    methods.forall { m =>
      val mTypes = mTypeSubst(m, x, y)

      // TODO: should be sufficient to check for mTypes.head if all mTypes are equal,
      //  since == is transitive?
      mTypes.forall {
        case (_, b) =>
          mTypes.forall {
            case (_, b1) =>
              b.size == b1.size &&
                b.forall(c => b1 contains c)
          }
      }
    } && (program forall typecheck)
  }

  override def typecheck(declaration: Declaration): Boolean = declaration match {
    // WF-CD
    case ConstructorDeclaration(_, x, a) => FV(a) == List(x) || FV(a).isEmpty
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
//        typecheck(a, e, Type(y, b)) // TODO: check if this is what we want
        typeAssignment(a, e).exists {
          case Type(z, c) =>
            c.size == b.size &&
              substitute(z, y, c).forall(b.contains(_))
        }
  }

  private def typeAssignment(context: List[Constraint], expr: Expression): List[Type] = expr match {
    // T-Var
    case x@Id(_) =>
      //classes(P).foldRight(List(Type(Id('tError), List(PathEquivalence(x, Id('noValidClass)))))){
      classes.foldRight(Nil: List[Type]) {
        case (cls, _) if entailment.entails(context, InstanceOf(x, cls)) =>
          val y = freshVariable()
          //          Type(y, List(PathEquivalence(y, x))) :: Nil// :: classes TODO: no need to find another one, as the type would be the same (after renaming)
          List(
            Type(y, List(PathEquivalence(y, x)))
            ,Type(y, List(PathEquivalence(x, y)))
          )
        case (_, classes) => classes
      } match {
        case Nil => List(Type(Id(Symbol("tError")), List(PathEquivalence(x, Id(Symbol("noValidClass"))))))
        case l => l
      }
    // T-Field
    case FieldAccess(e, f) =>
      val types = typeAssignment(context, e)
      val y = freshVariable()

      var ts: List[Type] = Nil
      types.foreach {
        case Type(x, a) =>
          // instance of relations for type constraints
          val instOfs = classes.foldRight(Nil: List[Constraint]) { // TODO: list of vars in entails, like T-Var case
            case (cls, classes) if entailment.entails(context ++ a, InstanceOf(FieldPath(x, f), cls)) =>
              val c = InstanceOf(y, cls)
              ts = Type(y, List(c)) :: ts
              c :: classes
            case (_, classes) => classes
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
      val eTypes = typeAssignment(context, e)
      val y = freshVariable()

      var types: List[Type] = Nil

      // for all possible argument types
      for (Type(x, a) <- eTypes) {
        // for all method declarations
        for ((a1, b) <- mTypeSubst(m, x, y)) {
          val entailsArgs = entailment.entails(context ++ a, a1)

          val b1 = (a1 ++ b).foldRight(Nil: List[Constraint]) {
            case (c, cs) if !FV(c).contains(x) => c :: cs
            case (_, cs) => cs
          }

          if (entailsArgs && entailment.entails(context ++ a ++ b, b1))
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
      val argsTypes: List[List[Type]] = args.map(arg => typeAssignment(context, arg._2))
      //val argsTypes: List[(Id, List[Type])] = args.map{case (f, e) => (f, typeAssignment(context, e))}

      val x = freshVariable()
      var types: List[Type] = Nil

      argsTypes match {
        case Nil =>
          val b = List(InstantiatedBy(x, cls))
          val (x1, b1) = classInProgram(cls, program).getOrElse(return List(Type(Id(Symbol("tError")), List(PathEquivalence(cls, Id(Symbol("classNotFound")))))))

          if (entailment.entails(context ++ b, b1))
            types = Type(x, b) :: types

          classes.foreach{
            c =>
              if (entailment.entails(context ++ b, InstanceOf(x, c)))
                types = Type(x, List(InstanceOf(x, c))) :: types
          }
        case _ => combinations(argsTypes).foreach {
          argsType =>
            val argsPairs: List[(Id, Type)] = fields.zip(argsType)
            val argsConstraints: List[Constraint] = argsPairs.flatMap{
              case (fi, Type(xi, ai)) => substitute(xi, FieldPath(x, fi), ai)
            }

            val b: List[Constraint] = InstantiatedBy(x, cls) :: argsConstraints

            val (x1, b1) = classInProgram(cls, program).getOrElse(return List(Type(Id(Symbol("tError")), List(PathEquivalence(cls, Id(Symbol("classNotFound")))))))

            if (entailment.entails(context ++ b, substitute(x1, x, b1)))
              types = Type(x, b) :: types

            classes.foreach{
              c =>
                if (entailment.entails(context ++ b, InstanceOf(x, c)))
                  types = Type(x, List(InstanceOf(x, c))) :: types
            }
        }
      }
      types.distinct
  }

  private var nameCounter: Int = 0
  private def freshName(): Symbol = {
    nameCounter += 1
    Symbol("x" + nameCounter.toString)
  }

  private def freshVariable(): Id = Id(freshName())

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
  private def combine(list: List[List[Type]], accumulator: List[Type]): Unit = list match {
    case Nil =>
    case last :: Nil => last.foreach(elem => _combs = (accumulator ++ List(elem)) :: _combs)
    case hd :: tl => hd.foreach(elem => combine(tl, accumulator ++ List(elem)))
  }
}
