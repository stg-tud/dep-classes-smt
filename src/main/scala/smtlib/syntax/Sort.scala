package smtlib.syntax

import smtlib.SMTLibFormatter

case class Sort(identifier: Identifier, sorts: Seq[Sort] = Nil) extends SMTLibFormatter {
  override def format(): String = sorts match {
    case Nil => identifier.format()
    case _ => s"(${identifier.format()} ${SMTLibFormatter.format(sorts)})"
  }
}

//trait Sort extends SMTLibFormatter
//
//case class SortTerm(identifier: Identifier, sorts: Seq[Sort]) extends Sort {
//  override def format(): String = s"(${identifier.format()} ${SMTLibFormatter.format(sorts)})"
//}