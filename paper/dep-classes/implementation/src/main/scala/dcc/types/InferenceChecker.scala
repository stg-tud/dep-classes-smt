package dcc.types
import dcc.DCC.{FV, constructorConstraintsSubst}
import dcc.Util
import dcc.entailment.EntailmentFactory
import dcc.entailment.EntailmentSort.EntailmentSort
import dcc.syntax.{AbstractMethodDeclaration, Constraint, ConstraintEntailment, ConstructorDeclaration, Declaration, Expression, FieldAccess, FieldPath, Id, InstanceOf, InstantiatedBy, MethodCall, MethodImplementation, ObjectConstruction, PathEquivalence}
import dcc.syntax.Program.{Program, extractFieldNames}
import dcc.syntax.Util.commaSeparate

import scala.annotation.tailrec

// infers the most specific/precise type of an expression
// TODO: add debug output
class InferenceChecker(override val program: Program, override val ENTAILMENT: EntailmentSort, override val debug: Int = 0) extends Checker {
  private val entailment = EntailmentFactory(ENTAILMENT)(program, debug)

  override def typeOf(context: List[Constraint], expression: Expression): Either[Type, List[TError]] = expression match {
    case x@Id(_) =>
      classes.find(cls => entailment.entails(context, InstanceOf(x, cls))) match {
        case Some(_) =>
          val y = freshVariable()
          Left(Type(y, Set(PathEquivalence(y, x))))
        case None => Right(List(s"variable '$x' is not available in context ${commaSeparate(context)}"))
      }
    case FieldAccess(e, f) =>
      typeOf(context, e) match {
        case Left(Type(x, a)) =>
          val y = freshVariable()
          // b contains all possible instances of x.f
          // e.g. if x.f is a Zero, it will contain x.f::Zero and x.f::Nat
          // TODO: only take the most specific one? (don't need, as the types can contain more information e.g. all class instances the expression e belongs to)
          val b = classes.filter(cls => entailment.entails(context ++ a, InstanceOf(FieldPath(x, f), cls))) map (cls => InstanceOf(y, cls))
          // TODO: for each instance, if it's a concrete class also add the fields?

          // !FV(b).contains(x) and entailment.entails(context++a:+PathEquivalence(FieldPath(x, f), y), b) by construction of b

          if (b.nonEmpty)
            Left(Type(y, b.toSet)) // TODO: change b s.t. it is a Set?
          else
            Right(List(s"'$x.$f' is not available in context ${commaSeparate(context++a)}"))
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
          //   or not? a1 is never used again in the type rule so it's pretty pointless
          //   also is seems sensible that we use the annotated return type of the method
          //   and since b has to be free of x and the context doesn't really connect that well to the b constraints,
          //   we are not able find a more specific type. (at least in the Nat example)
          //   This can be assumed to be true for other cases (programs) as well,
          //   as a program only successfully type-checks if all declarations/implementations of
          //   a method have the exact same return type annotation.
          //   Thus, not allowing to annotate more specific (sub-) types as the return type for an implementation.

          // Take the return type of the most specific method that applies (allows witness example if we allow method implementations to have a more specific return type)
//          // find all applicable methods (incl. abstract definition)
//          val applicable = mTypeSubst(m, x, y) filter { case (a1, _) => entailment.entails(context++a, a1)}
//          val mostSpecific = searchMostSpecificMethod(applicable)
////          println(s"argument type: $a")
////          println("applicable methods:")
////          applicable.foreach(x => println(s"\t$x"))
////          println(s"most specific method $m applicable: $mostSpecific")
//
//          mostSpecific match {
//            case Some((_, b)) =>
//              val b1 = a.filter(!FV(_).contains(x)) ++ b
//              Left(Type(y, b1))
//            case None => Right(List(s"no method declaration of '$m' applicable to '$e'"))
//          }

          // Take return type of method declaration as type (all implementations of the same method have the exact same return type)
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

              val b1 = a.filter(!FV(_).contains(x)) ++ b

              // b should be free of x by construction (mTypeSubst) and b |- b trivially
              Left(Type(y, b1))
            case None => Right(List(s"no method declaration of '$m' applicable to '$e'"))
          }
        case error@Right(_) => error
      }
    case ObjectConstruction(cls, args) =>
//      println("hello there, T-New application")
      val x: Id = freshVariable()
      val classConstraints = constructorConstraintsSubst(cls, program, x)
//            println(s"DEBUG: found ${classConstraints.size} constructors for class $cls")

      if (classConstraints.isEmpty)
        Right(List(s"No constructor found for class '$cls'"))
      else {
        val fieldResults: List[(Id, Either[Type, List[TError]])] = args map { case (f, e) => (f, typeOf(context, e)) }

        // Debug output
//        fieldResults foreach {
//          case (f, t) => println(s"$f: $t")
//        }

        val (fieldTypes: List[(Id, Type)], fieldErrors: List[(Id, List[TError])]) = fieldResults.foldLeft(Nil: List[(Id, Type)], Nil: List[(Id, List[TError])]) {
//        val (fieldTypes, fieldErrors): (List[(Id, Type)], List[(Id, TError)]) = fieldResults.foldLeft(List.empty[(Id, Type)], List.empty[(Id, TError)]) {
          case ((lhs, rhs), (f, Left(t))) => ( (f, t) :: lhs, rhs )
          case ((lhs, rhs), (f, Right(err))) => (lhs, (f, err) :: rhs)
        }
        if (fieldErrors.nonEmpty) {
//          Right(List(s"Class '$cls' can not be created, couldn't assign a type to field " + fieldErrors.foldLeft("") { case (rest, (f,err)) => s"\n\t$f: $err$rest" }))
          Right(
            fieldErrors.map{ case (f, err) => s"Class $cls: couldn't assign a type to field $f, because ${commaSeparate(err)}"}
          )
        } else {
          val b: List[Constraint] = InstantiatedBy(x, cls) :: fieldTypes.flatMap {case (f, Type(xi, ai)) => Util.substitute(xi, FieldPath(x, f), ai.toList)} // TODO: .toList (Set)

//          println(s"b = $b")
//          println(s"b' = $classConstraints")

//          for (b1 <- classConstraints) {
//            println(s"testing ${commaSeparate(context++b)} |- ${commaSeparate(b1)}")
//            val test = entailment.entails(context++b, b1)
//            println(s"\tyielded $test")
//            if (test)
//              return Left(Type(x, b))
//          }
//          Right("nope")

//          classConstraints.find {
//            b1: List[Constraint] =>
//              val constructorFields = extractFieldNames(b1)
//              args.forall{ case (f,_) => constructorFields.contains(f)} && entailment.entails(context++b, b1)
//          } match {
//            case Some(_) =>
//              Left(Type(x, b))
//            case None =>
//              Right(List(s"New object does not fulfill the constraints of class '$cls'")) // TODO: can we find out which constraint is violated? e.g. which field is missing/wrongly assigned
//          }

          var errors: List[TError] = Nil
          for (b1 <- classConstraints) {
            val constructorFields = extractFieldNames(b1)
            if (args.forall{ case (f, _) => constructorFields.contains(f)}) {
              val constructorErrors: List[TError] = b1.filter(!entailment.entails(context++b, _)).map(c => s"Class $cls: constructor constraint $c could not be fulfilled")

              if (constructorErrors.isEmpty)
                return Left(Type(x, b.toSet))

              errors = errors ++ constructorErrors
            } else {
              errors = s"Class $cls: unexpected field in constructor call" :: errors
            }
          }
          Right(errors)
        }
      }
  }

  override def typeCheck(context: List[Constraint], expression: Expression, typ: Type): Boolean = typeOf(context, expression) match {
    case Left(Type(y, a1)) =>
      // unify type variables between expected type `typ` and the inferred type
      val inferred = Type(typ.x, Util.substituteSet(y, typ.x, a1))

      if (entailment.entails(context++inferred.constraints.toList, typ.constraints.toList)) {
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
        freeVariablesContainsMax(FV(b.toList), x, y) &&
        typeCheck(a, e, typ)
    case AbstractMethodDeclaration(_, x, a, Type(y, b)) =>
      freeVariablesContainsMax(FV(a), x) &&
        x != y &&
        freeVariablesContainsMax(FV(b.toList), x, y)
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

    // TODO: add error/debug output, maybe change result type to Either[True, TError]? or to (Boolean, Option[TError])
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

//    val bool1 = methods.forall{ m =>
//      val mTypes = mType(m)
//      mTypes.forall{
//        case (_, Type(_, b)) =>
//          mTypes.forall{
//            case (_, Type(_, b1)) => b == b1
//          }
//      }
//    }
//
//    var bool2 = true
//    for (decl <- program) {
//      val tmp = typeCheck(decl)
//      if (!tmp)
//        bool2 = false
//    }
//
//    bool1 && bool2
  }

//  @tailrec
//  private def searchMostSpecificApplicableMethod(mTypes: List[(List[Constraint], List[Constraint])]): (List[Constraint], List[Constraint]) = {
//    require(mTypes.nonEmpty)
//
//    mTypes match {
//      case (a0, b0) :: Nil => (a0, b0)
//      case (a0, b0) :: rst =>
////        val candidate: Option[Boolean] = mTypes.map{ case (a1, b1) => b0==b1 && entailment.entails(a1, a0) && !entailment.entails(a0, a1) }.find(x => !x)
//        val isMostSpecific: Boolean = mTypes.forall{ case (a1, b1) => b0==b1 && entailment.entails(a1, a0) && !entailment.entails(a0, a1) }
//
//        if (isMostSpecific) {
//          (a0, b0)
//        } else {
//          searchMostSpecificApplicableMethod(rst)
//        }
//    }
//  }

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

  @tailrec
  private def searchMostSpecificMethod(types: List[(List[Constraint], List[Constraint])]): Option[(List[Constraint], List[Constraint])] = types match {
    case Nil => None
    case (a0, b0) :: rst =>
      val isMostSpecific: Boolean = types.forall { case (a1, _) => a0 == a1 || (entailment.entails(a0, a1) && !entailment.entails(a1, a0)) }

      if (isMostSpecific) {
        Some((a0, b0))
      } else {
        searchMostSpecificMethod(rst)
      }
  }
}

object InferenceChecker {
  def apply(program: Program, ENTAILMENT: EntailmentSort, debug: Int = 0): InferenceChecker = new InferenceChecker(program, ENTAILMENT, debug)
}