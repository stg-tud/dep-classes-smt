package dcc.syntax

object Util {
  def commaSeparate(l: List[Any]): String = l.foldRight(""){(x, xs) => s"$x, $xs"}.dropRight(2)
}