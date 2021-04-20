package dcc.types
import dcc.DCC.{FV, classInProgramSubst, constructorConstraintsSubst}
import dcc.Util
import dcc.entailment.Entailment
import dcc.syntax.Program.Program
import dcc.syntax.{AbstractMethodDeclaration, Constraint, ConstraintEntailment, ConstructorDeclaration, Declaration, Expression, FieldAccess, FieldPath, Id, InstanceOf, InstantiatedBy, MethodCall, MethodImplementation, ObjectConstruction, PathEquivalence}
import dcc.syntax.Util.commaSeparate

class FaithfulAdaptionChecker(override val program: Program, entailment: Entailment) extends Checker {
  override def typeOf(context: List[Constraint], expression: Expression): Either[Type, String] = expression match {
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
          val b = classes.filter(cls => entailment.entails(context ++ a, InstanceOf(FieldPath(x, f), cls))) map (cls => InstanceOf(y, cls))

          // !FV(b).contains(x) and entailment.entails(context++a:+PathEquivalence(FieldPath(x, f), y), b) by construction of b

          if (b.nonEmpty)
            Left(Type(y, b))
          else
            Right(s"'$x.$f' is not available in context ${commaSeparate(context++a)}")
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
          Right(s"Class '$cls' can not be created, couldn't assign a type to field " + fieldErrors.foldLeft("") { case (rest, (f,Right(err))) => s"\n\t$f: $err$rest" }) // TODO: add exhaustive check? will never fail, as Left() is already filtered out
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
      val Type(x, a) = typ

      entailment.entails(context++Util.substitute(y, x, a1), a)
    case Right(_) => false // TODO: add debug output
  }

  override def typeCheck(declaration: Declaration): Boolean = declaration match {
    case ConstructorDeclaration(_, x, a) => FV(a) == List(x)
    case MethodImplementation(_, x, a, typ@Type(y, b), e) =>
      val freeVarsB = FV(b)
      FV(a) == List(x) &&
        x != y &&
        freeVarsB.size == 2
        (freeVarsB contains x) &&
        (freeVarsB contains y) &&
          typeCheck(a, e, typ)
    case AbstractMethodDeclaration(_, x, a, Type(y, b)) =>
      val freeVarsB = FV(b)
      FV(a) == List(x) &&
        x != y &&
        freeVarsB.size == 2
        (freeVarsB contains x) &&
        (freeVarsB contains y)
    case ConstraintEntailment(x, a, InstanceOf(y, _)) =>
      x == y &&
        FV(a) == List(x) &&
        (a exists {
          case InstanceOf(`x`, _) => true
          case _ => false })
    case _ => false
  }

  override def typeCheck: Boolean = false

  private var nameCounter: Int = 0
  private def freshName(): Symbol = {
    nameCounter += 1
    Symbol("x" + nameCounter.toString)
  }

  private def freshVariable(): Id = Id(freshName())
}
