package dcc.entailment

import dcc.syntax.Program.Program
import smt.smtlib.SMTLib.buildEnumerationType
import smt.smtlib.SMTLibScript

class SemanticEntailment(val p: Program) {
  def entails: Boolean = {
    println(generateEnumerationTypes.format())
    true
  }

  private def generateEnumerationTypes: SMTLibScript = {
    // TODO: traverse program for classes, vars and fields
    val classes: List[String] = List("Nat", "Succ", "Zero")
    val variables: List[String] = List("x", "y", "z")
    val fields: List[String] = List("f", "g", "h")

    SMTLibScript(Seq(
      buildEnumerationType("Class", classes),
      buildEnumerationType("Variable", variables),
      buildEnumerationType("Field", fields)
    ))
  }
}
