package dcc.syntax

trait Path {
  def base: Id
  def baseName: String
  def prefixBaseName(prefix: String): Path
  def fields: List[Id]
  def depth: Int
}

object Path {
  /***
    * Create a Path object from a list of Strings.
    * E.g. List(x, f, g) -> FieldPath(FieldPath(x, f), g)
    * @param l First element of the List will be the receiving variable of the path
    * @return The Path object corresponding to the input list
    */
  def fromList(l: List[String]): Path = l.foldLeft(Id(Symbol("NIL")): Path) {
    case (Id(Symbol("NIL")), x) => Id(Symbol(x))
    case (xs: Path, x: String) => FieldPath(xs, Id(Symbol(x)))
  }

  /***
    * Create a Path object from a dot separated String.
    * E.g. "x.f.g" -> FieldPath(FieldPath(x, f), g)
    * @param s The string to be parsed
    * @return The parsed Path object
    */
  def fromString(s: String): Path = fromList(s.split('.').toList)
//    if (s.contains('.')) {
//      fromList(s.split('.').toList)
//    } else
//      Id(Symbol(s))
}

case class FieldPath(p: Path, f: Id) extends Path {
  override def toString: String = s"$p.$f"
  override def base: Id = p.base
  override def baseName: String = base.baseName
  override def prefixBaseName(prefix: String): Path = FieldPath(p.prefixBaseName(prefix), f)
  override def fields: List[Id] = f :: p.fields
  override def depth: Int = 1+p.depth
}

// also: VariableName extends Path