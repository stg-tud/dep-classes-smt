package smtlib.syntax

object Util {

}

object Implicit {
  //implicit def BoxSome(x: Sort): Option[Sort] = Some(x)
  implicit def stringToSimpleSymbol(s: String): SimpleSymbol = SimpleSymbol(s)
}