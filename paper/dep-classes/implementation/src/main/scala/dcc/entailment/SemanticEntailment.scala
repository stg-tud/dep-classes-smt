package dcc.entailment

import dcc.entailment.SemanticEntailment.{Field, Path, Variable}
import dcc.syntax.Constraint
import dcc.syntax.Program.Program
import smt.smtlib.SMTLib.buildEnumerationType
import smt.smtlib.SMTLibScript
import smt.smtlib.syntax.{ConstructorDatatype, ConstructorDec, DeclareDatatype, SelectorDec, SimpleSymbol, Sort}

class SemanticEntailment(val p: Program) {
  val staticDeclarations:SMTLibScript = SMTLibScript(Seq(
    DeclareDatatype(SimpleSymbol("Path"), ConstructorDatatype(Seq(
      ConstructorDec(SimpleSymbol("var"), Seq(SelectorDec(SimpleSymbol("id"), Variable))),
      ConstructorDec(SimpleSymbol("pth"), Seq(SelectorDec(SimpleSymbol("obj"), Path), SelectorDec(SimpleSymbol("field"), Field)))
    )))
  ))

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

object SemanticEntailment {
  // Sorts explicitly added in this translation.
  object Variable extends Sort {
    override def format(): String = "Variable"
  }

  object Field extends Sort {
    override def format(): String = "Field"
  }

  object Class extends Sort {
    override def format(): String = "Class"
  }

  object Path extends Sort {
    override def format(): String = "Path"
  }
}