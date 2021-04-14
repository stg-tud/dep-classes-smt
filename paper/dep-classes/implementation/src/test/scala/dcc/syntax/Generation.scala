package dcc.syntax

import scala.language.postfixOps
import scala.util.Random

object Generation {
  def generateRandomId(prefix: String = "var_"): Id = Id(Symbol(f"$prefix${Random.nextInt(1000)}%03d"))

  def generateRandomPath(maxDepth: Int = 2, x: Option[Id] = None): Path = maxDepth match {
    case n if n <= 0 => x.getOrElse(generateRandomId())
    case n if n > 0 => _generateRandomPath(Random.nextInt(n), x.getOrElse(generateRandomId()))
  }

  def _generateRandomPath(depth: Int, x: Id): Path = depth match {
    case 0 => x
    case n if n < 0 => x
    case n => FieldPath(_generateRandomPath(n-1, x), generateRandomId("fld_"))
  }
  def generateRandomClass(): Id = generateRandomId("Cls_")

  def generateRandomConstraint(maxPathDepth: Int = 2): Constraint = Random.nextInt(3) match {
    case 0 => PathEquivalence(generateRandomPath(maxPathDepth), generateRandomPath(maxPathDepth))
    case 1 => InstanceOf(generateRandomPath(maxPathDepth), generateRandomClass())
    case 2 => InstantiatedBy(generateRandomPath(maxPathDepth), generateRandomClass())
  }

  def generateRandomConstraintWithMetadata(): (Constraint, (List[Id], List[Path], List[Id])) = Random.nextInt(3) match {
    case 0 =>
      val x = generateRandomId()
      val y = generateRandomId()
      val p = generateRandomPath(x = Option(x))
      val q = generateRandomPath(x = Option(y))
      (
        PathEquivalence(p, q),
        (List(x, y), List(p, q), Nil)
      )
    case 1 =>
      val x = generateRandomId()
      val p = generateRandomPath(x = Option(x))
      val cls = generateRandomClass()

      (
        InstanceOf(p, cls),
        (List(x), List(p), List(cls))
      )
    case 2 =>
      val x = generateRandomId()
      val p = generateRandomPath(x = Option(x))
      val cls = generateRandomClass()

      (
        InstantiatedBy(p, cls),
        (List(x), List(p), List(cls))
      )
  }

  def generateRandomConstraints(amount: Int = 10): List[Constraint] = (1 to amount).toList.map(_ => generateRandomConstraint())

  def generateRandomConstraintsWithMetadata(amount: Int = 10): (List[Constraint], (List[Id], List[Path], List[Id])) = amount match {
    case 0 => (Nil, (Nil, Nil, Nil))
    case n if n < 0 => (Nil, (Nil, Nil, Nil))
    case n =>
      val (c, (cVars, cPaths, cls)) = generateRandomConstraintWithMetadata()
      val (cs, (vars, paths, classes)) = generateRandomConstraintsWithMetadata(n-1)
      (
        c :: cs distinct,
        (
          (cVars ++ vars).distinct,
          (cPaths ++ paths).distinct,
          (cls ++ classes).distinct
        )
      )
  }
}