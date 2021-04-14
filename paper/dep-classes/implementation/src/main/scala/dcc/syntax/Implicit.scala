package dcc.syntax

import dcc.types.Type
import scala.language.implicitConversions

object Implicit {
  implicit def SymbolToId(s: Symbol): Id = Id(s)
  implicit def StringToId(s: String): Id = Id(Symbol(s))
  implicit def SymbolTupleToType(tuple: (Symbol, List[Constraint])): Type = Type(Id(tuple._1), tuple._2)
  implicit def StringTupleToType(tuple: (String, List[Constraint])): Type = Type(Id(Symbol(tuple._1)), tuple._2)
}
