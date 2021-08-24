package dcc.entailment
import dcc.Util.substitute
import dcc.syntax.{Constraint, FieldPath, Id, InstanceOf, InstantiatedBy, Path, PathEquivalence, Util}
import dcc.syntax.Program.{DefinedClassNames, DefinedFieldNames, Program}
import smt.smtlib.{SMTLib, SMTLibCommand, SMTLibScript}
import smt.smtlib.syntax.{Apply, DefineFun, FunctionDef, SMTLibSymbol, SimpleSymbol, Sort, SortedVar, Term, Unsat}
import smt.smtlib.theory.BoolPredefined.{And, Bool, Eq, Or}
import smt.solver.Z3Solver

import scala.language.postfixOps

class PathDepthLimitEncoding(program: Program, debug: Int = 0) extends Entailment {
  // Sort names
  private val SortNameClass: String = "Class"
  private val SortNameVariable: String = "Variable"
  private val SortNameField: String = "Field"
  private val SortNamePath: String = "Path"

  // Sorts
  private val SortClass: Sort = SimpleSymbol(SortNameClass)
  private val SortVariable: Sort = SimpleSymbol(SortNameVariable)
  private val SortField: Sort = SimpleSymbol(SortNameField)
  private val SortPath: Sort = SimpleSymbol(SortNamePath)

  // Function names
  private val FunctionPathEquivalence: SMTLibSymbol = SimpleSymbol("path-equivalence")
  private val FunctionInstanceOf: SMTLibSymbol = SimpleSymbol("instance-of")
  private val FunctionInstantiatedBy: SMTLibSymbol = SimpleSymbol("instantiated-by")
  private val FunctionSubstitution: SMTLibSymbol = SimpleSymbol("substitute")

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

    val (datatypeDeclarations, pathDatatypeExists, classDatatypeExists) = constructTypeDeclarations(classNames, variableNames, fieldNames, paths)

    //TODO
    SMTLibScript(Seq())
  }

  private def constructTypeDeclarations(classes: List[String], variables: List[String], fields: List[String], paths: List[Path]): (SMTLibScript, Boolean, Boolean) = {
    var declarations: SMTLibScript = SMTLibScript(Seq(SMTLib.buildEnumerationType(SortNameVariable, variables)))

    val doAddClassDatatype = classes.nonEmpty
    if (doAddClassDatatype)
      declarations =  declarations :+ SMTLib.buildEnumerationType(SortNameClass, classes)

    val doAddPathDatatype = fields.nonEmpty
    if (doAddPathDatatype)
      declarations =  declarations :+ SMTLib.buildEnumerationType(SortNameField, fields) :+ SMTLib.buildEnumerationType(SortNamePath, paths)

    (declarations, doAddPathDatatype, doAddClassDatatype)
  }

  def generateSubstitutionFunction(paths: List[Path], vars: List[String], depthLimit: Int, pathDatatypeExists: Boolean): SMTLibCommand = {
    val source = freshPath()
    val target = freshVariable()
    val replace = freshPath()
    val result = freshPath()

    val path: Sort = if (pathDatatypeExists) SortPath else SortVariable

    DefineFun(FunctionDef(
      FunctionSubstitution,
      Seq(
        SortedVar(source, path),
        SortedVar(target, SortVariable),
        SortedVar(replace, path),
        SortedVar(result, path)
      ),
      Bool,
      generateSubstitutionFunctionBody(paths, vars.map(s => Id(Symbol(s))), source, target, replace, result, depthLimit)
    ))
  }

  def generateSubstitutionFunctionBody(paths: List[Path], vars: List[Id], sourceName: SMTLibSymbol, targetName: SMTLibSymbol, replaceName: SMTLibSymbol, resultName: SMTLibSymbol, depthLimit: Int): Term = {
    var relation: List[Term] = Nil

    paths.foreach(source =>
      vars.foreach(target =>
        paths.foreach { replace =>
          val result: Path = substitute(target, source, replace) // TODO: check if target != source.base, skip substitution in that case

          if (result.depth <= depthLimit) {
            val elem: Term = And(
              Eq(sourceName, PathToSMTLibSymbol(source)),
              Eq(targetName, IdToSMTLibSymbol(target)),
              Eq(replaceName, PathToSMTLibSymbol(replace)),
              Eq(resultName, PathToSMTLibSymbol(result))
            )

            relation = elem :: relation
          }
        }
      )
    )

    Or(relation: _*)
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

  def addFieldsToPath(path: Path, fields: List[String]): List[Path] = fields.map(f => FieldPath(path, Id(Symbol(f))))

  private def extractVariableNames(constraints: List[Constraint]): List[String] = constraints flatMap {
    case PathEquivalence(p, q) => List(p.baseName, q.baseName)
    case InstanceOf(p, _) => List(p.baseName)
    case InstantiatedBy(p, _) => List(p.baseName)
  } distinct

  private def IdToSMTLibSymbol(id: Id): SMTLibSymbol = SimpleSymbol(id.name.name)

  private def PathToSMTLibSymbol(path: Path): SMTLibSymbol = SimpleSymbol(path.toString)

  private def ConstraintToTerm(constraint: Constraint): Term = constraint match {
    case PathEquivalence(p, q) => Apply(FunctionPathEquivalence, Seq(PathToSMTLibSymbol(p), PathToSMTLibSymbol(q)))
    case InstanceOf(p, cls) => Apply(FunctionInstanceOf, Seq(PathToSMTLibSymbol(p), IdToSMTLibSymbol(cls)))
    case InstantiatedBy(p, cls) => Apply(FunctionInstantiatedBy, Seq(PathToSMTLibSymbol(p), IdToSMTLibSymbol(cls)))
  }

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
