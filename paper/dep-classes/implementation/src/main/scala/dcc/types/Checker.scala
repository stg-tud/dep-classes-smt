package dcc.types

import dcc.Util.substitute
import dcc.entailment.EntailmentSort.EntailmentSort
import dcc.syntax.{AbstractMethodDeclaration, Constraint, ConstraintEntailment, ConstructorDeclaration, Declaration, Expression, FieldPath, Id, InstanceOf, InstantiatedBy, MethodImplementation, Path}
import dcc.syntax.Program.{DefinedFields, Program}

import scala.language.postfixOps

trait Checker {
  val program: Program
  val ENTAILMENT: EntailmentSort

  def typeOf(context: List[Constraint], expression: Expression): Either[Type, List[TError]]
  def typeCheck(context: List[Constraint], expression: Expression, typ: Type): Boolean
  def typeCheck(declaration: Declaration): Boolean
  def typeCheck: Boolean

  def approx(t: Type): Set[Type] = {
    var S: Set[Type] = Set(t)
    var P:Set[Path] = Set(t.x)

    var i: Int = 1 // TODO: dev loop break

    while (P.nonEmpty && i <= 1) {
      i += 1

      for (p <- P) {
        var R: Set[Id] = Set.empty

        for (cls <- classes) {
//          if (!R.contains(cls) && S.exists(_.constraints.exists(_.classAppears(cls)))) {
          if (!R.contains(cls) && S.exists(_.constraints.contains(InstanceOf(p, cls)))) {
            R = R + cls

            // refine Appr-Class
            val apprClass: Set[Type] = S.filter(_.constraints.contains(InstanceOf(p, cls)))
            apprClass.foreach{
              case Type(x, a) =>

                program.foreach {
                  case ConstructorDeclaration(`cls`, y, b) =>
                    S = S + Type(x, InstantiatedBy(p, cls) :: a ++ substitute(y, p, b))
                  case ConstraintEntailment(y, b, InstanceOf(z, `cls`)) if y==z =>
                    S = S + Type(x, a ++ substitute(y, p, b))
                  case _ =>
                }
            }

            // refine Appr-Empty
            // TODO: remove entry or set S to empty?
            //  → should be set S to empty set
            //  → actually not, should be remove type from set if Appr-Empty applies?
            //val apprEmpty: Set[Type] = ???
          }
        }
      }

      var tmp: Set[Path] = Set.empty

      P.foreach{
        p =>
          //val fields: Set[Id] = S.map(_.constraints) // TODO: Constraint.getFieldsWithBase(p)
          val fields = DefinedFields(program).filter(f => S.exists(_.constraints.exists(_.containedPaths.contains(FieldPath(p, f)))))

          fields.foreach { f => tmp += FieldPath(p, f) }

      }
      P = tmp
    }

    S.foreach(println)
    S
  }

  // Method Type
  def mType(m: Id): List[(Type, Type)] =
    program.foldRight(Nil: List[(Type, Type)]){
      case (AbstractMethodDeclaration(`m`, x, a, ret), rst) => (Type(x, a), ret) :: rst
      case (MethodImplementation(`m`, x, a, ret, _), rst) => (Type(x, a), ret) :: rst
      case (_, rst) => rst}

  // MType where the bound variables of declared argument and return type constraints are
  // substituted with given variables
  def mTypeSubst(m: Id, x: Id, y: Id): List[(List[Constraint], List[Constraint])] =
    program.foldRight(Nil: List[(List[Constraint], List[Constraint])]){
      case (AbstractMethodDeclaration(`m`, xDeclaration, a, Type(yDeclaration, b)), rst) =>
        (substitute(xDeclaration, x, a), substitute(yDeclaration, y, b)) :: rst
      case (MethodImplementation(`m`, xImpl, a, Type(yImpl, b), _), rst) =>
        (substitute(xImpl, x, a), substitute(yImpl, y, b)) :: rst
      case (_, rst) => rst}

  def classes: List[Id] = (program flatMap className) distinct

  // search for class names in all constraints? nah, abstract classes must be a superclass. concrete classes must have a constructor
  private def className(d: Declaration): Option[Id] = d match {
    case ConstructorDeclaration(cls, _, _) => Some(cls)
    case ConstraintEntailment(_, _, InstanceOf(_, cls)) => Some(cls)
    case _ => None
  }

  def methods: List[Id] = program flatMap methodName distinct

  private def methodName(d: Declaration): Option[Id] = d match {
    case AbstractMethodDeclaration(m, _, _, _) => Some(m)
    case MethodImplementation(m, _, _, _, _) => Some(m)
    case _ => None
  }
}
