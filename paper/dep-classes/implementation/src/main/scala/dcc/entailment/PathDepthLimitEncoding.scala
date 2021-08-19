package dcc.entailment
import dcc.syntax.{Constraint, FieldPath, Id, InstanceOf, InstantiatedBy, Path, PathEquivalence, Util}
import dcc.syntax.Program.{DefinedClassNames, DefinedFieldNames, Program}
import smt.smtlib.SMTLibScript
import smt.smtlib.syntax.{SMTLibSymbol, SimpleSymbol, Unsat}
import smt.solver.Z3Solver

import scala.language.postfixOps

class PathDepthLimitEncoding(program: Program, debug: Int = 0) extends Entailment {
  override def entails(context: List[Constraint], constraint: Constraint): Boolean = {
    if (debug > 0)
      println(s"entailment: ${if (context.isEmpty) "·" else Util.commaSeparate(context)} |- $constraint")

    val smt = encode(context, constraint)
    val solver = new Z3Solver(smt, debug=if (debug>2) true else false)

    solver.checkSat match {
      case Left(Unsat) => true
      case Left(_) => false
      case Right(errors) =>
        System.err.println("SMT error:")
        errors foreach {error => System.err.println(error.format)}
        false
    }
  }

  override def entails(context: List[Constraint], constraints: List[Constraint]): Boolean = constraints.forall(entails(context, _))

  def encode(context: List[Constraint], conclusion: Constraint): SMTLibScript = {
    // Reset counters for fresh name generation
    resetFreshNameCounter()

    val variableNames = extractVariableNames(conclusion :: context)
    val fieldNames = DefinedFieldNames(program)
    val classNames = DefinedClassNames(program)

    // TODO: determine depth limit based on program/other means
    val depthLimit = 1
    val paths = enumeratePaths(variableNames, fieldNames, depthLimit)

    //TODO
    SMTLibScript(Seq())
  }

  def enumeratePaths(vars: List[String], fields: List[String], depthLimit: Int): List[Path] = {
    // Initialize paths with variables
    var paths: List[Path] = vars.map(s => Id(Symbol(s)))

    (1 to depthLimit) foreach { _ =>
      paths = paths ++ addFieldsToPaths(paths, fields)
    }

    paths.distinct
  }

  def addFieldsToPaths(paths: List[Path], fields: List[String]): List[Path] = paths.flatMap{p => addFieldsToPath(p, fields)}
//  paths match {
//    case Nil => Nil
//    case p :: rest => addFieldsToPath(p, fields) ++ addFieldsToPaths(rest, fields)
//  }

  def addFieldsToPath(path: Path, fields: List[String]): List[Path] = fields.map(f => FieldPath(path, Id(Symbol(f))))

  private def extractVariableNames(constraints: List[Constraint]): List[String] = constraints flatMap {
    case PathEquivalence(p, q) => List(p.baseName, q.baseName)
    case InstanceOf(p, _) => List(p.baseName)
    case InstantiatedBy(p, _) => List(p.baseName)
  } distinct

  private var varCounter = 0
  private def freshVariable(): SMTLibSymbol = {
    varCounter = varCounter + 1
    SimpleSymbol(s"x$varCounter")
  }
  private var pathCounter = 0
  private def freshPath(): SMTLibSymbol = {
    pathCounter = pathCounter + 1
    SimpleSymbol(s"p$pathCounter")
  }
  private var classCounter = 0
  private def freshClassVar(): SMTLibSymbol = {
    classCounter = classCounter + 1
    SimpleSymbol(s"cls$classCounter")
  }

  private def resetFreshNameCounter(): Unit = {
    varCounter = 0
    pathCounter = 0
    classCounter = 0
  }
}
