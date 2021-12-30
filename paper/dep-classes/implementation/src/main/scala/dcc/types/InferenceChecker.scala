package dcc.types
import dcc.DCC.{FV, constructorConstraintsSubst}
import dcc.Util
import dcc.entailment.EntailmentFactory
import dcc.entailment.EntailmentSort.EntailmentSort
import dcc.syntax.{AbstractMethodDeclaration, Constraint, ConstraintEntailment, ConstructorDeclaration, Declaration, Expression, FieldAccess, FieldPath, Id, InstanceOf, InstantiatedBy, MethodCall, MethodImplementation, ObjectConstruction, PathEquivalence}
import dcc.syntax.Program.Program
import dcc.syntax.Util.commaSeparate

import scala.annotation.tailrec

// infers the most specific/precise type of an expression
class InferenceChecker(override val program: Program, override val ENTAILMENT: EntailmentSort, debug: Int = 0) extends Checker {
  private val entailment = EntailmentFactory(ENTAILMENT)(program, debug)

  // TODO: overhaul to return the most specific type
  //  use subtype checks (T-Sub) to do so
  override def typeOf(context: List[Constraint], expression: Expression): Either[Type, TError] = expression match {
    case x@Id(_) =>
      classes.find(cls => entailment.entails(context, InstanceOf(x, cls))) match {
        case Some(_) =>
          val y = freshVariable()
          Left(Type(y, List(PathEquivalence(y, x))))
        case None => Right(s"variable '$x' is not available in context ${commaSeparate(context)}")
      }
    case FieldAccess(e, f) =>
      typeOf(context, e) match {
        case Left(Type(x, a)) =>
          val y = freshVariable()
          // b contains all possible instances of x.f
          // e.g. if x.f is a Zero, it will contain x.f::Zero and x.f::Nat
          val b = classes.filter(cls => entailment.entails(context ++ a, InstanceOf(FieldPath(x, f), cls))) map (cls => InstanceOf(y, cls))

          // !FV(b).contains(x) and entailment.entails(context++a:+PathEquivalence(FieldPath(x, f), y), b) by construction of b

          if (b.nonEmpty)
            Left(Type(y, b))
          else
            Right(s"'$x.$f' is not available in context ${commaSeparate(context++a)}")
        case error@Right(_) => error
      }
    case MethodCall(m, e) => // need to do subtype checks here, find some b' for b.
      typeOf(context, e) match {
        case Left(Type(x, a)) =>
          val y = freshVariable()

//          val applicable = mTypeSubst(m, x, y) filter { case (a1, _) => entailment.entails(context++a, a1)}
//          val specific = searchMostSpecificApplicableMethod(applicable)
//          println("applicable methods:")
//          applicable.foreach(println)
//          println(s"most specific method: $specific")

          // probably should identify the most specific applicable method here, not the abstract definition but the concrete implementation
          //   or not? a1 is never used again in the type rule
          mTypeSubst(m, x, y) find { case (a1, _) => entailment.entails(context++a, a1) } match {
            case Some((_, b)) =>
//              println(s"typeOf $m($e): found applicable method with args ${commaSeparate(a1)} and context ${commaSeparate(context++a)}")
//
//              var b1: List[Constraint] = Nil
//
//              for (cls <- classes) {
//                println(s"test for ${commaSeparate(context++a++b)} |– ${InstanceOf(y, cls)}")
//                if(entailment.entails(context++a++b, InstanceOf(y, cls)))
//                  b1 = InstanceOf(y, cls) :: b1
//              }

              //val b1 = classes filter (cls => entailment.entails(context++a++b, InstanceOf(y, cls))) map (cls => InstanceOf(y, cls))

              // b should be free of x by construction (mTypeSubst) and b |- b trivially
              Left(Type(y, b))
            case None => Right(s"no method declaration of '$m' applicable to '$e'")
          }
        case error@Right(_) => error
      }
    case ObjectConstruction(cls, args) =>
      val x: Id = freshVariable()
      val classConstraints = constructorConstraintsSubst(cls, program, x)
      //      println(s"DEBUG: found ${classConstraints.size} constructors for class $cls")

      if (classConstraints.isEmpty)
        Right(s"No constructor found for class '$cls'")
      else {
        val fieldResults: List[(Id, Either[Type, String])] = args map { case (f, ei) => (f, typeOf(context, ei)) }
        val fieldErrors = fieldResults filter { case (_, t) => t.isRight }
        if (fieldErrors.nonEmpty) {
          Right(s"Class '$cls' can not be created, couldn't assign a type to field " + fieldErrors.foldLeft("") { case (rest, (f,Right(err))) => s"\n\t$f: $err$rest" case (rest, _) => rest}) // exhaustive match not necessarily needed, as Left is already filtered out
        } else {
          val fieldTypes: List[(Id, Type)] = fieldResults.foldLeft(Nil: List[(Id, Type)]) {
            case (rest, (f, Left(t))) => (f, t) :: rest
            case (rest, _) => rest
          }
          val b: List[Constraint] = InstantiatedBy(x, cls) :: fieldTypes.flatMap {case (f, Type(xi, ai)) => Util.substitute(xi, FieldPath(x, f), ai)}

          classConstraints.find{ b1 => entailment.entails(context++b, b1) } match {
            case Some(_) => Left(Type(x, b))
            case None => Right(s"New object does not fulfill the constraints of class '$cls'")
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

  @tailrec
  private def searchMostSpecificApplicableMethod(mTypes: List[(List[Constraint], List[Constraint])]): (List[Constraint], List[Constraint]) = {
    require(mTypes.nonEmpty)

    mTypes match {
      case (a0, b0) :: Nil => (a0, b0)
      case (a0, b0) :: rst =>
//        val candidate: Option[Boolean] = mTypes.map{ case (a1, b1) => b0==b1 && entailment.entails(a1, a0) && !entailment.entails(a0, a1) }.find(x => !x)
        val isMostSpecific: Boolean = mTypes.forall{ case (a1, b1) => b0==b1 && entailment.entails(a1, a0) && !entailment.entails(a0, a1) }

        if (isMostSpecific) {
          (a0, b0)
        } else {
          searchMostSpecificApplicableMethod(rst)
        }
    }
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
}
