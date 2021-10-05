package dcc.entailment
import dcc.Util.substitute
import dcc.syntax.{Constraint, ConstraintEntailment, Declaration, FieldPath, Id, InstanceOf, InstantiatedBy, Path, PathEquivalence, Util}
import dcc.syntax.Program.{DefinedClassNames, DefinedFieldNames, Program}
import smt.smtlib.{SMTLib, SMTLibCommand, SMTLibScript}
import smt.smtlib.syntax.{Apply, Assert, DeclareFun, DefineFun, Forall, FunctionDef, SMTLibSymbol, SimpleSymbol, Sort, SortedVar, Term, Unsat}
import smt.smtlib.theory.BoolPredefined.{And, Bool, Eq, Implies, Not, Or, True}
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
  //private val SortField: Sort = SimpleSymbol(SortNameField)
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
    //  - functions
    //    - constraints propositions
    //    - substitution function (partial function)
    //  - calculus rules (quantifiers vs. ground formulae)
    //    - static rules: C-Refl, C-Class, C-Subst
    //    - dynamic rules: C-Prog
    //  - entailment judgement

    // Reset counters for fresh name generation
    resetFreshNameCounter()

    val variableNames = extractVariableNames(conclusion :: context)
    val fieldNames = DefinedFieldNames(program)
    val classNames = DefinedClassNames(program)

    // TODO: determine depth limit based on program/entailment context/other means
    val depthLimit = 1
    val paths = enumeratePaths(variableNames, fieldNames, depthLimit)

    val (datatypeDeclarations, pathDatatypeExists, classDatatypeExists) = constructTypeDeclarations(classNames, variableNames, fieldNames, paths)

    val constraintPropositionDeclarations = constructConstraintPropositions(pathDatatypeExists, classDatatypeExists)
    val substitutionFunctionDeclaration = generateSubstitutionFunction(paths, variableNames, depthLimit, pathDatatypeExists)

    val staticCalculusRules = constructStaticCalculusRules(pathDatatypeExists, classDatatypeExists)
    val dynamicCalculusRules = constructDynamicCalculusRules(pathDatatypeExists, classDatatypeExists)

    val entailmentJudgement = constructEntailmentJudgement(context, conclusion)

    datatypeDeclarations ++
      constraintPropositionDeclarations ++
      substitutionFunctionDeclaration ++
      staticCalculusRules ++
//      dynamicCalculusRules ++ // TODO: add those again when the prog rule generation SMT error is fixed
      entailmentJudgement
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

  private def constructConstraintPropositions(pathDatatypeExists: Boolean, classDatatypeExists: Boolean): SMTLibScript = {
    val path: Sort = if (pathDatatypeExists) SortPath else SortVariable

    var declarations: SMTLibScript = SMTLibScript(Seq(DeclareFun(FunctionPathEquivalence, Seq(path, path), Bool)))

    if (classDatatypeExists)
      declarations = declarations :++ Seq(
        DeclareFun(FunctionInstanceOf, Seq(path, SortClass), Bool),
        DeclareFun(FunctionInstantiatedBy, Seq(path, SortClass), Bool)
      )

    declarations
  }

  private def constructStaticCalculusRules(pathDatatypeExists: Boolean, classDatatypeExists: Boolean): SMTLibScript = {
    val path = if (pathDatatypeExists) SortPath else SortVariable

    // C-Refl
    val pRefl: SMTLibSymbol = freshPath()
    val cReflexivity = Assert(Forall(Seq(SortedVar(pRefl, path)), Apply(FunctionPathEquivalence, Seq(pRefl, pRefl))))

    // C-Class
    val pClass: SMTLibSymbol = freshPath()
    val clsClass: SMTLibSymbol = freshClassVar()
    val cClass = Assert(Forall(
      Seq(
        SortedVar(pClass, path),
        SortedVar(clsClass, SortClass)
      ),
      Implies(Apply(FunctionInstantiatedBy, Seq(pClass, clsClass)), Apply(FunctionInstanceOf, Seq(pClass, clsClass)))
    ))

    // Substitution Signature:
    //   SortedVar(source, path),
    //   SortedVar(target, SortVariable),
    //   SortedVar(replace, path),
    //   SortedVar(result, path)

    // C-Subst-Eq
    val xSubstEq = freshVariable()
    val pSubstEq = freshPath()
    val qSubstEq = freshPath()
    val rSubstEq = freshPath()
    val sSubstEq = freshPath()
    val p_subst_r_Eq = freshPath()
    val q_subst_r_Eq = freshPath()
    val p_subst_s_Eq = freshPath()
    val q_subst_s_Eq = freshPath()
    val cSubstPathEq = Assert(Forall(
      Seq(
        SortedVar(pSubstEq, path),
        SortedVar(qSubstEq, path),
        SortedVar(xSubstEq, SortVariable),
        SortedVar(rSubstEq, path),
        SortedVar(sSubstEq, path),
        SortedVar(p_subst_r_Eq, path),
        SortedVar(q_subst_r_Eq, path),
        SortedVar(p_subst_s_Eq, path),
        SortedVar(q_subst_s_Eq, path)
      ),
      Implies(
        And(
          Apply(FunctionPathEquivalence, Seq(sSubstEq, rSubstEq)),
          Apply(FunctionSubstitution, Seq(pSubstEq, xSubstEq, rSubstEq, p_subst_r_Eq)),
          Apply(FunctionSubstitution, Seq(qSubstEq, xSubstEq, rSubstEq, q_subst_r_Eq)),
          Apply(FunctionSubstitution, Seq(pSubstEq, xSubstEq, sSubstEq, p_subst_s_Eq)),
          Apply(FunctionSubstitution, Seq(qSubstEq, xSubstEq, sSubstEq, q_subst_s_Eq)),
          Apply(FunctionPathEquivalence, Seq(p_subst_r_Eq, q_subst_r_Eq))
        ),
        Apply(FunctionPathEquivalence, Seq(p_subst_s_Eq, q_subst_s_Eq))
      )
    ))

    // C-Subst-InstOf
    val xSubstOf = freshVariable()
    val pSubstOf = freshPath()
    val clsSubstOf = freshClassVar()
    val rSubstOf = freshPath()
    val sSubstOf = freshPath()
    val p_subst_r_Of = freshPath()
    val p_subst_s_Of = freshPath()
    val cSubstInstOf = Assert(Forall(
      Seq(
        SortedVar(pSubstOf, path),
        SortedVar(clsSubstOf, SortClass),
        SortedVar(xSubstOf, SortVariable),
        SortedVar(rSubstOf, path),
        SortedVar(sSubstOf, path),
        SortedVar(p_subst_r_Of, path),
        SortedVar(p_subst_s_Of, path)
      ),
      Implies(
        And(
          Apply(FunctionPathEquivalence, Seq(sSubstOf, rSubstOf)),
          Apply(FunctionSubstitution, Seq(pSubstOf, xSubstOf, rSubstOf, p_subst_r_Of)),
          Apply(FunctionSubstitution, Seq(pSubstOf, xSubstOf, sSubstOf, p_subst_s_Of)),
          Apply(FunctionInstanceOf, Seq(p_subst_r_Of, clsSubstOf))
        ),
        Apply(FunctionInstanceOf, Seq(p_subst_s_Of, clsSubstOf))
      )
    ))

    // C-Subst-InstBy
    val xSubstBy = freshVariable()
    val pSubstBy = freshPath()
    val clsSubstBy = freshClassVar()
    val rSubstBy = freshPath()
    val sSubstBy = freshPath()
    val p_subst_r_By = freshPath()
    val p_subst_s_By = freshPath()
    val cSubstInstBy = Assert(Forall(
      Seq(
        SortedVar(pSubstBy, path),
        SortedVar(clsSubstBy, SortClass),
        SortedVar(xSubstBy, SortVariable),
        SortedVar(rSubstBy, path),
        SortedVar(sSubstBy, path),
        SortedVar(p_subst_r_By, path),
        SortedVar(p_subst_s_By, path)
      ),
      Implies(
        And(
          Apply(FunctionPathEquivalence, Seq(sSubstBy, rSubstBy)),
          Apply(FunctionSubstitution, Seq(pSubstBy, xSubstBy, rSubstBy, p_subst_r_By)),
          Apply(FunctionSubstitution, Seq(pSubstBy, xSubstBy, sSubstBy, p_subst_s_By)),
          Apply(FunctionInstantiatedBy, Seq(p_subst_r_By, clsSubstBy))
        ),
        Apply(FunctionInstantiatedBy, Seq(p_subst_s_By, clsSubstBy))
      )
    ))

    if (classDatatypeExists)
      SMTLibScript(Seq(
        cReflexivity,
        cClass,
        cSubstPathEq,
        cSubstInstOf,
        cSubstInstBy
      ))
    else
      SMTLibScript(Seq(
        cReflexivity,
        cSubstPathEq
      ))
  }

  private def constructDynamicCalculusRules(pathDatatypeExists: Boolean, classDatatypeExists: Boolean): SMTLibScript = {
    if (!classDatatypeExists)
      SMTLibScript.EMPTY
    else
      // TODO: why does collect not work here (see simplified example in Foo)
//      SMTLibScript(program.collect(constructProgRule(_, pathDatatypeExists)))
      SMTLibScript(program.filter(constructProgRule.isDefinedAt(_, pathDatatypeExists)).map(constructProgRule(_, pathDatatypeExists)))
  }

  val constructProgRule: PartialFunction[(Declaration, Boolean), SMTLibCommand] = {
    case (ConstraintEntailment(x, context, InstanceOf(y, cls)), pathDatatypeExists: Boolean) if x==y && context.nonEmpty =>
      val path = if (pathDatatypeExists) SortPath else SortVariable

      val metaPathName = freshPath()
      val pDCC: Path = MetaPath(metaPathName.symbol)
      val pSMT: SMTLibSymbol = SimpleSymbol(pDCC.baseName)

      // directly substitute if the path only describes a variable
      val smtSubstContext = context.map {
        case PathEquivalence(p, q) if p.depth == 0 && q.depth == 0 =>
          ConstraintToTerm(PathEquivalence(substitute(x, pDCC, p), substitute(x, pDCC, q)))
        case PathEquivalence(p, q) if p.depth == 0 => // TODO: what to do with the unsubstituted variable? we need to apply the substitution function of the solver, but the problem is the binding variable x as it is not part of the variable list. possible solution: ground this and enumerate over all paths?
          PathEquivalence(substitute(x, pDCC, p), q)
        case PathEquivalence(p, q) if q.depth == 0 =>
          PathEquivalence(p, substitute(x, pDCC, q))
        case PathEquivalence(p, q) => PathEquivalence(p, q)
        case InstanceOf(p, cls) if p.depth == 0 =>
          InstanceOf(substitute(x, pDCC, p), cls)
        case InstanceOf(p, cls) =>
          InstanceOf(p, cls)
        case InstantiatedBy(p, cls) if p.depth == 0 =>
          InstantiatedBy(substitute(x, pDCC, p), cls)
        case InstantiatedBy(p, cls) =>
          InstantiatedBy(p, cls)
      }

      // TODO: do not perform the substitution prior (results in p25.p, which doesnt work anymore with enumerated paths)
      // TODO: possible solution: ground the path variable of the quantifier and perform the substitution for each path instance
//      val lhs =
//        if (context.size == 1)
//          ConstraintToTerm(dcc.Util.substitute(x, pDCC, context.head))
//        else
//          And(context.map(constraint => ConstraintToTerm(dcc.Util.substitute(x, pDCC, constraint))): _*)

      val lhs =
        if (context.size == 1)
          ConstraintToTerm(dcc.Util.substitute(x, pDCC, context.head))
        else
          And(context.map(constraint => ConstraintToTerm(dcc.Util.substitute(x, pDCC, constraint))): _*)

      Assert(Forall(
        Seq(SortedVar(pSMT, path)),
        Implies(
          lhs,
          Apply(FunctionInstanceOf, Seq(pSMT, IdToSMTLibSymbol(cls)))
        )
      ))
  }

  private def constructEntailmentJudgement(context: List[Constraint], conclusion: Constraint): SMTLibScript = {
    // prefix the paths, since we prefixed the paths during enumeration (TODO make this better, see also def enumeratePaths and generateSubstitutionFunctionBody)
    def prefixPath(p: Path): Path = p match {
      case FieldPath(p, f) => FieldPath(prefixPath(p), f)
      case Id(name) => Id(Symbol(s"pth_${name.name}"))
      case MetaPath(s) => MetaPath(s)
    }

    def prefixConstraint(c: Constraint): Constraint = c match {
      case PathEquivalence(p, q) => PathEquivalence(prefixPath(p), prefixPath(q))
      case InstanceOf(p, cls) => InstanceOf(prefixPath(p), cls)
      case InstantiatedBy(p, cls) => InstantiatedBy(prefixPath(p), cls)
    }

    val prefixedContext = context.map(prefixConstraint)
    val prefixedConclusion = prefixConstraint(conclusion)

    val term =
      if (context.isEmpty)
        True
      else if (context.size == 1)
        ConstraintToTerm(prefixedContext.head)
      else
        And(prefixedContext.map(ConstraintToTerm): _*)

    SMTLibScript(Seq(
      Assert(Not(Implies(
        term,
        ConstraintToTerm(prefixedConclusion)
      )))
    ))
  }

  // Substitution Function would be a partial function with the depth limit in place,
  // it is transformed into a total function by modeling it as a relation that is false for substitutions exceeding the depth limit
  def generateSubstitutionFunction(paths: List[Path], vars: List[String], depthLimit: Int, pathDatatypeExists: Boolean): SMTLibScript = {
    val source = freshPath()
    val target = freshVariable()
    val replace = freshPath()
    val result = freshPath()

    val path: Sort = if (pathDatatypeExists) SortPath else SortVariable

    SMTLibScript(Seq(
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
    ))
  }

  def generateSubstitutionFunctionBody(paths: List[Path], vars: List[Id], sourceName: SMTLibSymbol, targetName: SMTLibSymbol, replaceName: SMTLibSymbol, resultName: SMTLibSymbol, depthLimit: Int): Term = {
    var relation: List[Term] = Nil

    paths.foreach(source =>
      vars.foreach(target =>
        paths.foreach { replace =>
          // prefix target such that it matches the prefixed paths TODO: make this better (see also def enumeratePaths and constructEntailmentJudgement)
          val prefixedTarget = Id(Symbol("pth_")) + target

          val result: Path = if (prefixedTarget.baseName != source.baseName) source else substitute(prefixedTarget, replace, source)

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
    var paths: List[Path] = vars.map(s => Id(Symbol(s"pth_$s"))) // prefix to not have ambiguous names between variables and paths (TODO: make this better, see also def constructEntailmentJudgement and generateSubstitutionFunctionBody)

    (1 to depthLimit) foreach { _ =>
      paths = paths ++ addFieldsToPaths(paths, fields)
    }

    paths.distinct
  }

  private def addFieldsToPaths(paths: List[Path], fields: List[String]): List[Path] = paths.flatMap{p => addFieldsToPath(p, fields)}

  private def addFieldsToPath(path: Path, fields: List[String]): List[Path] = fields.map(f => FieldPath(path, Id(Symbol(f))))

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
  private def freshVariable(): SimpleSymbol = {
    varCounter = varCounter + 1
    SimpleSymbol(s"x$varCounter")
  }
  private var pathCounter = 0
  private def freshPath(): SimpleSymbol = {
    pathCounter = pathCounter + 1
    SimpleSymbol(s"p$pathCounter")
  }
  private var classCounter = 0
  private def freshClassVar(): SimpleSymbol = {
    classCounter = classCounter + 1
    SimpleSymbol(s"cls$classCounter")
  }

  private def resetFreshNameCounter(): Unit = {
    varCounter = 0
    pathCounter = 0
    classCounter = 0
  }

  private case class MetaPath(s: String) extends Path {
    override def toString: String = s
    override def baseName: String = s
    override def fieldNames: List[String] = Nil
    override def depth: Int = 0
  }
}
