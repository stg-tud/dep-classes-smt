package smt.smtlib

/**
  * Base class for SMT Lib commands
  */

trait SMTLibFormatter {
  def format(): String
}

object SMTLibFormatter {
  def format(seq: Seq[SMTLibFormatter], separator: String = " "): String =
    seq.foldRight(""){(x, xs) => s"${x.format()}$separator$xs"}.dropRight(1)
}

trait SMTLibCommand extends SMTLibFormatter

case class SMTLibScript(commands: Seq[SMTLibCommand]) extends SMTLibFormatter {
  override def format(): String = SMTLibFormatter.format(commands, "\n")
}

trait SMTLibResponse extends SMTLibFormatter