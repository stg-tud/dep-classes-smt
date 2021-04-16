package dcc.types
import dcc.DCC.classInProgramSubst
import dcc.Util
import dcc.entailment.Entailment
import dcc.syntax.Program.Program
import dcc.syntax.{Constraint, Declaration, Expression, FieldAccess, FieldPath, Id, InstanceOf, InstantiatedBy, MethodCall, ObjectConstruction, PathEquivalence}
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

          mTypeSubst(m, x, y) find { case (a1, b) => entailment.entails(context++a, a1) } match {
            case Some((_, b)) =>
              // b should be free of x by construction (mTypeSubst) and b |- b trivially
              Left(Type(y, b))
            case None => Right(s"no method declaration of '$m' applicable to '$e'")
          }
        case error@Right(_) => error
      }
    case ObjectConstruction(cls, args) =>
      val x: Id = freshVariable()
      classInProgramSubst(cls, program, x) match { // TODO: what if more than one constructor exists in the program? (generate list and check if ↓ holds for at least one of them)
        case Some(b1) =>
          val fieldResults: List[(Id, Either[Type, String])] = args map { case (f, ei) => (f, typeOf(context, ei)) }
          val fieldErrors = fieldResults filter { case (_, t) => t.isRight }
          if (fieldErrors.nonEmpty) {
            val error: String = s"Class '$cls' can not be created, couldn't assign a type to field " + fieldErrors.foldLeft("") { case (rest, (f,Right(err))) => s"\n\t$f: $err$rest" }
            Right(error)
          } else {
            val fieldTypes: List[(Id, Type)] = fieldResults.foldLeft(Nil: List[(Id, Type)]) {
              case (rest, (f, Left(t))) => (f, t) :: rest
              case (rest, _) => rest
            }
            val b: List[Constraint] = InstantiatedBy(x, cls) :: fieldTypes.flatMap {case (f, Type(xi, ai)) => Util.substitute(xi, FieldPath(x, f), ai)}

            if (entailment.entails(context++b, b1))
              Left(Type(x, b))
            else
              Right(s"New object does not fulfill the constraints of class '$cls'")
          }
        case None => Right(s"Not constructor found for class '$cls'")
      }
  }

  override def typecheck(context: List[Constraint], expression: Expression, typ: Type): Boolean = false

  override def typecheck(declaration: Declaration): Boolean = false

  override def typecheck: Boolean = false

  private var nameCounter: Int = 0
  private def freshName(): Symbol = {
    nameCounter += 1
    Symbol("x" + nameCounter.toString)
  }

  private def freshVariable(): Id = Id(freshName())
}
