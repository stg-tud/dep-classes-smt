package syntax

object Util {
  implicit def symbolToId(s: Symbol) = Id(s)
}
