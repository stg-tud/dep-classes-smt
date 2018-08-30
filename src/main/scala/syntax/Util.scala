package syntax

object Util {
  def commaSeparate(l: List[Any]): String = l.foldRight(""){(x, xs) => s"$x, $xs"}.dropRight(2)

  implicit def symbolToId(s: Symbol): Id = Id(s)
  implicit def tupleToType(tuple: (Symbol, List[Constraint])): Type = Type(Id(tuple._1), tuple._2) // Id(x) in Type could be implicitly converted from Symbol
}
