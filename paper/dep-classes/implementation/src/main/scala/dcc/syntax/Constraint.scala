package dcc.syntax

sealed trait Constraint {
  def maxPathDepth: Int
  def containedPaths: List[Path]
  def classAppears(cls: Id): Boolean
  def hasFieldPathWithBase(base: Path): Boolean
  def getFieldsWithBase(base: Path): Set[Id]
}

case class PathEquivalence(p: Path, q: Path) extends Constraint {
  def isReflexive: Boolean = p == q

  override def maxPathDepth: Int = p.depth max q.depth
  override def containedPaths: List[Path] = List(p, q)
  override def classAppears(cls: Id): Boolean = false

  override def hasFieldPathWithBase(base: Path): Boolean =
    (p.base == base && p.fields.nonEmpty) ||
      (q.base == base && q.fields.nonEmpty)

  override def getFieldsWithBase(base: Path): Set[Id] = {
    var res: Set[Id] = Set.empty

    try {
      val FieldPath(`base`, f) = p
      res = res + f
    } catch {
      case _: Throwable =>
    }

    try {
      val FieldPath(`base`, f) = q
      res = res + f
    } catch {
      case _: Throwable =>
    }

    res
  }

  override def toString: String = s"$p ≡ $q"
}
case class InstanceOf(p: Path, cls: Id) extends Constraint {
  override def maxPathDepth: Int = p.depth
  override def containedPaths: List[Path] = List(p)
  override def classAppears(cls: Id): Boolean = this.cls == cls
  override def hasFieldPathWithBase(base: Path): Boolean = p.base == base && p.fields.nonEmpty

  override def getFieldsWithBase(base: Path): Set[Id] = {
    var res: Set[Id] = Set.empty

    try {
      val FieldPath(`base`, f) = p
      res = res + f
    } catch {
      case _: Throwable =>
    }

    res
  }

  override def toString: String = s"$p :: $cls"
}
case class InstantiatedBy(p: Path, cls: Id) extends Constraint {
  override def maxPathDepth: Int = p.depth
  override def containedPaths: List[Path] = List(p)
  override def classAppears(cls: Id): Boolean = this.cls == cls
  override def hasFieldPathWithBase(base: Path): Boolean = p.base == base && p.fields.nonEmpty

  override def getFieldsWithBase(base: Path): Set[Id] = {
    var res: Set[Id] = Set.empty

    try {
      val FieldPath(`base`, f) = p
      res = res + f
    } catch {
      case _: Throwable =>
    }

    res
  }

  override def toString: String = s"$p.cls ≡ $cls"
}