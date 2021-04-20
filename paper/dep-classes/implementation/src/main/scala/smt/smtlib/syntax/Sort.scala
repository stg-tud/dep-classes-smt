package smt.smtlib.syntax

import smt.smtlib.SMTLibFormatter

trait Sort extends SMTLibFormatter

case class Sorts(identifier: Identifier, sorts: Seq[Sort]) extends Sort {
  override def format: String = sorts match {
    case Nil => identifier.format // interpreted as a "unary" sort
    case _ => s"(${identifier.format} ${SMTLibFormatter.format(sorts)})"
  }
}

//trait Sort extends SMTLibFormatter
//
//case class SortTerm(identifier: Identifier, sorts: Seq[Sort]) extends Sort {
//  override def format: String = s"(${identifier.format} ${SMTLibFormatter.format(sorts)})"
//}