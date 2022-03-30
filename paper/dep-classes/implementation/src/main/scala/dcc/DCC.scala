package dcc

import dcc.syntax.Program.Program
import dcc.syntax.{Constraint, ConstructorDeclaration, FieldPath, Id, InstanceOf, InstantiatedBy, Path, PathEquivalence}

import scala.annotation.tailrec

object DCC {
  type Obj = (Id, List[(Id, Id)])
  type Heap = Map[Id, Obj]

  def NoArgsObj(cls: Id): Obj = (cls, Nil)

  val EmptyHeap: Heap = Map()

  def ObjToString(o: Obj): String = o match {
    case (cls, Nil) => cls.toString
//    case (cls, fields) => s"$cls(${fields.foldRight(""){ case (f, rest) => s"${f._1}=${f._2}, $rest"}.dropRight(2)})"
//    case (cls, fields) => s"$cls(${fields.foldRight("") {
//      case (f, rest) =>
//        if (rest.isEmpty)
//          s"${f._1}=${f._2}"
//        else
//          s"${f._1}=${f._2}, $rest"
//    }})"
    case (cls, fields) => s"$cls(${fields.foldRight(""){
      case (f, "") => s"${f._1}=${f._2}"
      case (f, rest) => s"${f._1}=${f._2}, $rest"
    }})"
  }

  def HeapToString(heap: Heap): String =
    s"{\n${heap.foldRight(""){ case ((x, obj), rest) => s"\t$x → ${ObjToString(obj)}\n$rest" }}}"

  // Heap Constraints
  def HC(heap: Heap): List[Constraint] = heap.flatMap{case (x, o) => OC(x, o)}.toList
  //heap.map{case (x, o) => OC(x, o)}.flatten.toList

  // Object Constraints
  def OC(x: Id, o: Obj): List[Constraint] = o match {
    case (cls, fields) =>
      val init = InstantiatedBy(x, cls)
      val fieldCs = fields.map{case (f, v) => PathEquivalence(FieldPath(x, f), v)}

      init :: fieldCs
  }

  @tailrec
  def classInProgram(Cls: Id, program: Program): Option[(Id, List[Constraint])] = program match {
    case Nil => None
    case ConstructorDeclaration(Cls, x, a) :: _ => Some(x, a)
    case _ :: rst => classInProgram(Cls, rst)
  }

  @tailrec
  def classInProgramSubst(Cls: Id, program: Program, x: Id): Option[List[Constraint]] = program match {
    case Nil => None
    case ConstructorDeclaration(Cls, y, a) :: _ => Some(Util.substitute(y, x, a))
    case _ :: rst => classInProgramSubst(Cls, rst, x)
  }

  def constructorConstraintsSubst(className: Id, program: Program, x: Id): List[List[Constraint]] = program match {
    case Nil => Nil
    case ConstructorDeclaration(`className`, y, a) :: tail => Util.substitute(y, x, a) :: constructorConstraintsSubst(className, tail, x)
    case _ :: tail => constructorConstraintsSubst(className, tail, x)
  }

  def FV(constraints: List[Constraint]): List[Id] = constraints.flatMap(FV).distinct

  def FV(constraint: Constraint): List[Id] = constraint match {
    case PathEquivalence(p, q) => variableName(p) :: variableName(q) :: Nil
    case InstanceOf(p, _) => variableName(p) :: Nil
    case InstantiatedBy(p, _) => variableName(p) :: Nil
  }

  @tailrec
  private def variableName(p: Path): Id = p match {
    case x@Id(_) => x
    case FieldPath(q, _) => variableName(q)
  }
}
