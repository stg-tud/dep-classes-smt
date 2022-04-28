package dcc.types

import collection.InheritanceRelation.{Subtype, Supertype}
import collection.{Edge, InheritanceGraph, Node}
import dcc.Util.substitute
import dcc.entailment.EntailmentFactory
import dcc.entailment.EntailmentSort.EntailmentSort
import dcc.syntax.{AbstractMethodDeclaration, Constraint, ConstraintEntailment, ConstructorDeclaration, Declaration, Expression, FieldPath, Id, InstanceOf, InstantiatedBy, MethodImplementation, Path, PathEquivalence}
import dcc.syntax.Program.{DefinedFields, Program}

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

trait Checker {
  val program: Program
  val ENTAILMENT: EntailmentSort
  val debug: Int

  def typeOf(context: List[Constraint], expression: Expression): Either[Type, List[TError]]
  def typeCheck(context: List[Constraint], expression: Expression, typ: Type): Boolean
  def typeCheck(declaration: Declaration): Boolean
  def typeCheck: Boolean

  /**
    * Completeness approximation.
    * @param t The type for which the approximation set will be generated
    * @return The type approximation set
    */
  def generateTypeApproximationSet(t: Type, maxIterations: Int = 5): Set[Type] = {
    var S: Set[Type] = Set(t)
    var P:Set[Path] = Set(t.x)

    var i: Int = 1

    while (P.nonEmpty && i <= maxIterations) {
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
            S = S.foldRight(Set.empty: Set[Type]) {
              case (t, rest) if approxEmptyApplicable(t) => rest
              case (t, rest) => rest + t
            }
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

  private def approxEmptyApplicable(t: Type): Boolean = t match {
    //case Type(x, a) =>
    case Type(_, a) =>
      val instances: List[InstantiatedBy] = a.filter(_.isInstanceOf[InstantiatedBy]).asInstanceOf[List[InstantiatedBy]]

      instances.find(_approxEmptyApplicable(a, instances, _)) match {
        case None => false
        case Some(_) => true
      }
  }

  private def _approxEmptyApplicable(context: List[Constraint], instances: List[InstantiatedBy], instance: InstantiatedBy): Boolean = instance match {
    case InstantiatedBy(p, c) =>
      //val otherInstances = instances.filter(inst => if (inst.cls!=c) true else false)

      val entailment = EntailmentFactory(ENTAILMENT)(program, debug)

      instances.find {
        case InstantiatedBy(_, `c`) => false
        case InstantiatedBy(p1, _) if entailment.entails(context, PathEquivalence(p, p1)) => true
        case _ => false // match may not be exhaustive. It would fail on the following input: InstantiatedBy(_, Id(_))
      } match {
        case None => false
        case Some(_) => true
      }
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

  def inheritanceGraph: InheritanceGraph = {
    val nodes: List[Node] = classes.map(Node)
    val edges: ListBuffer[Edge] = new ListBuffer[Edge]()

    for (declaration <- program) {
      if(declaration.isInstanceOf[ConstraintEntailment]) {
        val ConstraintEntailment(x: Id, as, InstanceOf(_, supertype)) = declaration

        val subtypeCandidate = as.find {
          case InstanceOf(`x`, _) => true
          case _ => false
        }

        if(subtypeCandidate.isDefined) {
          val InstanceOf(_, subtype) = subtypeCandidate.get

          edges += Edge(Node(subtype), Node(supertype), Subtype)
          edges += Edge(Node(supertype), Node(subtype), Supertype)
        }
      }
    }

    InheritanceGraph(nodes.toSet, edges.toSet)
  }
}
