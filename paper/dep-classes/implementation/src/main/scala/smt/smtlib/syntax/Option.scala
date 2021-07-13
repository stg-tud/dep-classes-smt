package smt.smtlib.syntax

import smt.smtlib.SMTLibFormatter

trait Option extends SMTLibFormatter {
  override def pretty: String = ""
}

case class DiagnosticOutputChannel(string: SMTLibString) extends Option {
  override def format: String = s":diagnostic-output-channel ${string.format}"
}

case class GlobalDeclarations(b: Boolean) extends Option {
  override def format: String = s":global-declarations $b"
}

case class InteractiveMode(b: Boolean) extends Option {
  override def format: String = s":interactive-mode $b"
}

case class PrintSuccess(b: Boolean) extends Option {
  override def format: String = s":print-success $b"
}

case class ProduceAssertions(b: Boolean) extends Option {
override def format: String = s":produce-assertions $b"
}

case class ProduceAssignments(b: Boolean) extends Option {
override def format: String = s":produce-assignments $b"
}

case class ProduceModels(b: Boolean) extends Option {
override def format: String = s":produce-models $b"
}

case class ProduceProofs(b: Boolean) extends Option {
override def format: String = s":produce-proofs $b"
}

case class ProduceUnsatAssumptions(b: Boolean) extends Option {
override def format: String = s":produce-unsat-assumptions $b"
}

case class ProduceUnsatCores(b: Boolean) extends Option {
override def format: String = s":produce-unsat-cores $b"
}

case class RandomSeed(numeral: Numeral) extends Option {
override def format: String = s":random-seed ${numeral.format}"
}

case class RegularOutputChannel(string: SMTLibString) extends Option {
override def format: String = s":regular-output-channel ${string.format}"
}

case class ReproducibleResourceLimit(numeral: Numeral) extends Option {
override def format: String = s":reproducible-resource-limit ${numeral.format}"
}

case class Verbosity(numeral: Numeral) extends Option {
  override def format: String = s":verbosity ${numeral.format}"
}

//trait BValue extends SMTLibFormatter
//
//case object True extends BValue {
//  override def format: String = s"true"
//}
//
//case object False extends BValue {
//  override def format: String = s"false"
//}