package smtlib.syntax

import smtlib.SMTLibFormatter

case class Sort(id: String, sorts: Seq[Sort] = Nil) extends SMTLibFormatter {
  override def format(): String = sorts match {
    case Nil => id
    case _ => s"($id ${SMTLibFormatter.format(sorts)})"
  }
}

//sealed trait Sort extends SMTLibFormatter
//
//case object Bool extends Sort {
//  override def format(): String = "Bool"
//}
//
//case class SId(id: String) extends Sort {
//  override def format(): String = id
//}
//case class SConc(id: String, sorts: Seq[Sort]) extends Sort {
//  override def format(): String = s"($id ${sorts.foldRight(""){(x, xs) => s"${x.format()} $xs"}.dropRight(1)})"
//  //sorts.foldRight(""){(x, xs) => s"${x.format()} $xs"}
//  //sorts.flatMap(_.format())
//}