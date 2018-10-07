package smtlib.syntax

import smtlib.SMTLibFormatter

trait Option extends SMTLibFormatter

case class DiagnosticOutputChannel(string: SMTLibString) extends Option {
  override def format(): String = s":diagnostic-output-channel ${string.format()}"
}

case class GlobalDeclarations(b: BValue) extends Option {
  override def format(): String = s":global-declarations ${b.format()}"
}

case class InteractiveMode(b: BValue) extends Option {
  override def format(): String = s":interactive-mode ${b.format()}"
}

case class PrintSuccess(b: BValue) extends Option {
  override def format(): String = s":print-success ${b.format()}"
}

case class ProduceAssertions(b: BValue) extends Option {
override def format(): String = s":produce-assertions ${b.format()}"
}

case class ProduceAssignments(b: BValue) extends Option {
override def format(): String = s":produce-assignments ${b.format()}"
}

case class ProduceModels(b: BValue) extends Option {
override def format(): String = s":produce-models ${b.format()}"
}

case class ProduceProofs(b: BValue) extends Option {
override def format(): String = s":produce-proofs ${b.format()}"
}

case class ProduceUnsatAssumptions(b: BValue) extends Option {
override def format(): String = s":produce-unsat-assumptions ${b.format()}"
}

case class ProduceUnsatCores(b: BValue) extends Option {
override def format(): String = s":produce-unsat-cores ${b.format()}"
}

case class RandomSeed(numeral: Numeral) extends Option {
override def format(): String = s":random-seed ${numeral.format()}"
}

case class RegularOutputChannel(string: SMTLibString) extends Option {
override def format(): String = s":regular-output-channel ${string.format()}"
}

case class ReproducibleResourceLimit(numeral: Numeral) extends Option {
override def format(): String = s":reproducible-resource-limit ${numeral.format()}"
}

case class Verbosity(numeral: Numeral) extends Option {
  override def format(): String = s":verbosity ${numeral.format()}"
}

trait BValue extends SMTLibFormatter

case object True extends BValue {
  override def format(): String = s"true"
}

case object False extends BValue {
  override def format(): String = s"false"
}