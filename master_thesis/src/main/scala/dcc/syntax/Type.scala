package dcc.syntax

import Util.commaSeparate

case class Type(x: Id, constraints: List[Constraint]) {
  override def toString: String = s"[$x. ${commaSeparate(constraints)}]"
}