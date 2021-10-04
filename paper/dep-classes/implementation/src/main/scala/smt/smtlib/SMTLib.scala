package smt.smtlib

import smt.smtlib.syntax.{Apply, ConstructorDatatype, ConstructorDec, DeclareDatatype, QualifiedIdentifier, SMTLibSymbol, SimpleSymbol, Term}

/**
  * Base class for SMT Lib commands
  */

trait SMTLibFormatter {
  def format: String
  def pretty: String
}

object SMTLibFormatter {
  def format(seq: Seq[SMTLibFormatter], separator: String = " "): String =
    seq.foldRight(""){(x, xs) => s"${x.format}$separator$xs"}.dropRight(separator.length)

  def pretty(seq: Seq[SMTLibFormatter], separator: String = " "): String =
    seq.foldRight(""){(x, xs) => s"${x.pretty}$separator$xs"}.dropRight(separator.length)
}

trait SMTLibCommand extends SMTLibFormatter

case class SMTLibScript(commands: Seq[SMTLibCommand]) extends SMTLibFormatter {
  override def format: String = SMTLibFormatter.format(commands, "\n")
  override def pretty: String = SMTLibFormatter.pretty(commands, "\n")
  def ++(right: SMTLibScript): SMTLibScript = SMTLibScript(commands ++ right.commands)
  def :++(right: Seq[SMTLibCommand]): SMTLibScript = SMTLibScript(commands ++ right)
  def ++:(left: Seq[SMTLibCommand]): SMTLibScript = SMTLibScript(left ++ commands)
  def +:(left: SMTLibCommand): SMTLibScript = SMTLibScript(left +: commands)
  def :+(right: SMTLibCommand): SMTLibScript = SMTLibScript(commands :+ right)
}

object SMTLibScript {
  val EMPTY: SMTLibScript = SMTLibScript(Seq())
}

trait SMTLibResponse extends SMTLibFormatter {
  // TODO: currently pretty printing is only supposed to give readable results on the input FO formulae
  override def pretty: String = format
}

object SMTLib {
//  example:
//  def fromConstraint: SMTLibCommand = ???

  def buildEnumerationType(typename: SMTLibSymbol, constructors: Seq[ConstructorDec]): SMTLibCommand =
    DeclareDatatype(typename, ConstructorDatatype(constructors))

  def buildEnumerationType(typename: String, constructors: Seq[Any]): SMTLibCommand =
    DeclareDatatype(SimpleSymbol(typename), ConstructorDatatype(constructors.map(constructor => ConstructorDec(SimpleSymbol(constructor.toString), Seq()))))

  def is(constructor: SimpleSymbol, arg: Term): Term = Apply(SimpleSymbol("is-") + constructor, Seq(arg))
  def selector(selector: QualifiedIdentifier, arg: Term): Term = Apply(selector, Seq(arg))
}