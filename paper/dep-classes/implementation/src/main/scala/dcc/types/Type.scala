package dcc.types

import dcc.syntax.Util.commaSeparate
import dcc.syntax.{Constraint, Id}

case class Type(x: Id, constraints: List[Constraint]) {
  override def toString: String = s"[$x. ${commaSeparate(constraints)}]"
}