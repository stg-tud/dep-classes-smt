package dcc.types

import dcc.syntax.Util.commaSeparate
import dcc.syntax.{Constraint, Id}

case class Type(x: Id, constraints: Set[Constraint]) {
  override def toString: String = s"[$x. ${commaSeparate(constraints.toList)}]"

  //def apply(x: String, as: List[Constraint]): Type = Type(Id(Symbol(x)), as.toSet)
}