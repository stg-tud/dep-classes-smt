package dcc.types
import dcc.DCC.{FV, constructorConstraintsSubst}
import dcc.Util
import dcc.entailment.EntailmentSort.EntailmentSort
import dcc.entailment.{Entailment, EntailmentFactory}
import dcc.syntax.Program.Program
import dcc.syntax.{AbstractMethodDeclaration, Constraint, ConstraintEntailment, ConstructorDeclaration, Declaration, Expression, FieldAccess, FieldPath, Id, InstanceOf, InstantiatedBy, MethodCall, MethodImplementation, ObjectConstruction, PathEquivalence}
import dcc.syntax.Util.commaSeparate

// Only infers some type for an expression
class SomeInferenceChecker(override val program: Program, override val ENTAILMENT: EntailmentSort, debug: Int = 0) extends Checker {
  val entailment: Entailment = EntailmentFactory(ENTAILMENT)(program, debug)

  override def typeOf(context: List[Constraint], expression: Expression): Either[Type, List[TError]] = expression match {
    case x@Id(_) =>
      classes.find(cls => entailment.entails(context, InstanceOf(x, cls))) match {
        case Some(_) =>
          val y = freshVariable()
          Left(Type(y, List(PathEquivalence(y, x))))
        case None => Right(List(s"variable '$x' is not available in context ${commaSeparate(context)}"))
      }
    case FieldAccess(e, f) =>
      typeOf(context, e) match {
        case Left(Type(x, a)) =>
          val y = freshVariable()
          val b = classes.filter(cls => entailment.entails(context ++ a, InstanceOf(FieldPath(x, f), cls))) map (cls => InstanceOf(y, cls))

          // !FV(b).contains(x) and entailment.entails(context++a:+PathEquivalence(FieldPath(x, f), y), b) by construction of b

          if (b.nonEmpty)
            Left(Type(y, b))
          else
            Right(List(s"'$x.$f' is not available in context ${commaSeparate(context++a)}"))
        case error@Right(_) => error
      }
    case MethodCall(m, e) =>
      typeOf(context, e) match {
        case Left(Type(x, a)) =>
          val y = freshVariable()

          mTypeSubst(m, x, y) find { case (a1, _) => entailment.entails(context++a, a1) } match {
            case Some((_, b)) =>
              // b should be free of x by construction (mTypeSubst) and b |- b trivially
              Left(Type(y, b))
            case None => Right(List(s"no method declaration of '$m' applicable to '$e'"))
          }
        case error@Right(_) => error
      }
    case ObjectConstruction(cls, args) =>
      val x: Id = freshVariable()
      val classConstraints = constructorConstraintsSubst(cls, program, x)
//      println(s"DEBUG: found ${classConstraints.size} constructors for class $cls")
      
      if (classConstraints.isEmpty)
        Right(List(s"No constructor found for class '$cls'"))
      else {
        val fieldResults: List[(Id, Either[Type, List[TError]])] = args map { case (f, ei) => (f, typeOf(context, ei)) }
//        val fieldErrors: List[(Id, List[TError])] = (fieldResults filter { case (_, t) => t.isRight }).asInstanceOf[List[(Id, List[TError])]]
        val fieldErrors: List[(Id, List[TError])] = fieldResults.foldLeft(Nil: List[(Id, List[TError])]) {
          case (rest, (f, Right(err))) => (f, err) :: rest
          case (rest, _) => rest
        }
        if (fieldErrors.nonEmpty) {
          //Right(List(s"Class '$cls' can not be created, couldn't assign a type to field " + fieldErrors.foldLeft("") { case (rest, (f,Right(err))) => s"\n\t$f: $err$rest" case (rest, _) => rest})) // exhaustive match not necessarily needed, as Left is already filtered out
          Right(
            fieldErrors.map {
              case (f, err) => s"Class $cls: couldn't assign a type to field $f, because ${commaSeparate(err)}"
            }
          )
        } else {
          val fieldTypes: List[(Id, Type)] = fieldResults.foldLeft(Nil: List[(Id, Type)]) {
            case (rest, (f, Left(t))) => (f, t) :: rest
            case (rest, _) => rest
          }
          val b: List[Constraint] = InstantiatedBy(x, cls) :: fieldTypes.flatMap {case (f, Type(xi, ai)) => Util.substitute(xi, FieldPath(x, f), ai)}

          classConstraints.find{ b1 => entailment.entails(context++b, b1) } match {
            case Some(_) => Left(Type(x, b))
            case None => Right(List(s"New object does not fulfill the constraints of class '$cls'"))
          }
        }
      }
  }

  override def typeCheck(context: List[Constraint], expression: Expression, typ: Type): Boolean = typeOf(context, expression) match {
    case Left(Type(y, a1)) =>
      // unify type variables between expected type `typ` and the inferred type
      val inferred = Type(typ.x, Util.substitute(y, typ.x, a1))

      if (entailment.entails(context++inferred.constraints, typ.constraints)) {
        true
      }
      else {
        if (debug > 0)
          println(s"type check for $expression failed, expected '$typ' but got '$inferred'")

        false
      }

    case Right(err) =>
      if (debug > 0)
        println(s"type check for $expression failed with $err")

      false
  }

  override def typeCheck(declaration: Declaration): Boolean = declaration match {
      // Emptiness check added to allow for zero argument constructors
    case ConstructorDeclaration(_, x, a) => a.isEmpty || FV(a) == List(x)
    case MethodImplementation(_, x, a, typ@Type(y, b), e) =>
      freeVariablesContainsMax(FV(a), x) &&
        x != y &&
        freeVariablesContainsMax(FV(b), x, y) &&
        typeCheck(a, e, typ)
    case AbstractMethodDeclaration(_, x, a, Type(y, b)) =>
      freeVariablesContainsMax(FV(a), x) &&
        x != y &&
        freeVariablesContainsMax(FV(b), x, y)
    case ConstraintEntailment(x, a, InstanceOf(y, _)) =>
      x == y &&
        FV(a) == List(x) &&
        (a exists {
          case InstanceOf(`x`, _) => true
          case _ => false })
    case _ => false
  }

  override def typeCheck: Boolean = {
    // TODO: add complete/unique
    //  requires some smt (or other) representation, as we would need to quantify over all heaps (inf)

    methods.forall{ m =>
      val mTypes = mType(m)
      mTypes.forall{
        case (_, Type(_, b)) =>
          mTypes.forall{
            case (_, Type(_, b1)) => b == b1
          }
      }
    } &&
    program.forall(typeCheck)
  }


  private var nameCounter: Int = 0
  private def freshName(): Symbol = {
    nameCounter += 1
    Symbol("x" + nameCounter.toString)
  }

  private def freshVariable(): Id = Id(freshName())

  private def freeVariablesContainsMax(freeVariables: List[Id], x: Id): Boolean =
    freeVariables.isEmpty || freeVariables == List(x)

  private def freeVariablesContainsMax(freeVariables: List[Id], x: Id, y: Id): Boolean =
    freeVariables.forall(v => v==x || v==y)
//    freeVariables.isEmpty ||
//      (freeVariables.size == 1 && (freeVariables == List(x) || freeVariables == List(y))) ||
//      (freeVariables.size == 2 && freeVariables.contains(x) && freeVariables.contains(y))
}

object SomeInferenceChecker {
  def apply(program: Program, entailmentSort: EntailmentSort, debug: Int = 0): SomeInferenceChecker = new SomeInferenceChecker(program, entailmentSort, debug)
}