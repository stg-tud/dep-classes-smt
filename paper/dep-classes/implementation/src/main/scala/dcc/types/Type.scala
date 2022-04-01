package dcc.types

import dcc.syntax.Util.commaSeparate
import dcc.syntax.{Constraint, Id}

// TODO: update constraints to be as Set instead of List?
case class Type(x: Id, constraints: List[Constraint]) {
  override def toString: String = s"[$x. ${commaSeparate(constraints)}]"
}