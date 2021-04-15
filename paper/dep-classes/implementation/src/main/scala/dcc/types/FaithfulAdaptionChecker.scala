package dcc.types
import dcc.entailment.Entailment
import dcc.syntax.Program.Program
import dcc.syntax.{Constraint, Declaration, Expression, FieldAccess, FieldPath, Id, InstanceOf, MethodCall, ObjectConstruction, PathEquivalence}
import dcc.syntax.Util.commaSeparate

class FaithfulAdaptionChecker(override val program: Program, entailment: Entailment) extends Checker {
  override def typeOf(context: List[Constraint], expression: Expression): Either[Type, String] = expression match {
    case x@Id(_) =>
      classes.find(cls => entailment.entails(context, InstanceOf(x, cls))) match {
        case Some(_) =>
          val y = freshVariable()
          Left(Type(y, List(PathEquivalence(y, x))))
        case None => Right(s"var $x is not available in context ${commaSeparate(context)}")
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
            Right(s"$x.$f is not available in context ${commaSeparate(context++a)}")
        case error@Right(_) => error
      }
    case MethodCall(m, e) => Right("not yet implemented")
    case ObjectConstruction(cls, args) => Right("not yet implemented")
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
