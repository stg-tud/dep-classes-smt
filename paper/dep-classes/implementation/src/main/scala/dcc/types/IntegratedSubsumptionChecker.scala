package dcc.types
import dcc.DCC.FV
import dcc.syntax.Program.Program
import dcc.syntax.{AbstractMethodDeclaration, Constraint, ConstraintEntailment, ConstructorDeclaration, Declaration, Expression, Id, InstanceOf, MethodImplementation}
import dcc.syntax.Util.tupleToType

class IntegratedSubsumptionChecker(override val program: Program) extends Checker {
  override def typeOf(context: List[Constraint], expression: Expression): Type = (Symbol("x"), Nil) // TODO: implement

  override def typecheck(context: List[Constraint], expression: Expression, typ: Type): Boolean = false // TODO: implement

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
        typecheck(a, e, Type(y, b)) // TODO: check if this is what we want
//        typeassignment(a, e, skipGen).exists {
//          case Type(z, c) =>
//            c.size == b.size &&
//              substitute(z, y, c).forall(b.contains(_))
//        }
  }

  private var nameCounter: Int = 0
  private def freshName(): Symbol = {
    nameCounter += 1
    Symbol("x" + nameCounter.toString)
  }

  private def freshVariable(): Id = Id(freshName())
}
