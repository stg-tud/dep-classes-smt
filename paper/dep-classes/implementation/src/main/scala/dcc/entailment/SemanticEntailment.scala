package dcc.entailment

import dcc.syntax.Constraint
import dcc.syntax.Program.Program
import smt.smtlib.SMTLib.buildEnumerationType
import smt.smtlib.SMTLibScript

class SemanticEntailment(val p: Program) {
  def entails(context: List[Constraint], c: Constraint): Boolean = {

    // TODO: traverse program for classes, vars and fields
    val classes: List[String] = List("Nat", "Succ", "Zero")
    val variables: List[String] = List("x", "y", "z")
    val fields: List[String] = List("f", "g", "h")
    println(generateEnumerationTypes(classes, variables, fields).format())

    true
  }

  private def generateEnumerationTypes(classes: List[String], variables: List[String], fields: List[String]): SMTLibScript =
    SMTLibScript(Seq(
      buildEnumerationType("Class", classes),
      buildEnumerationType("Variable", variables),
      buildEnumerationType("Field", fields)
    ))
}
