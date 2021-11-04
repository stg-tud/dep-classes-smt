package dcc.entailment
import dcc.Util.substitute
import dcc.syntax.{Constraint, ConstraintEntailment, Declaration, FieldPath, Id, InstanceOf, InstantiatedBy, Path, PathEquivalence, Util}
import dcc.syntax.Program.{DefinedClassNames, DefinedFieldNames, Program}
import smt.smtlib.{SMTLib, SMTLibCommand, SMTLibScript}
import smt.smtlib.syntax.{Apply, Assert, DeclareFun, DefineFun, FunctionDef, SMTLibSymbol, SimpleSymbol, Sort, SortedVar, Sugar, Term, Unsat}
import smt.smtlib.theory.BoolPredefined.{And, Bool, Eq, Implies, Not, Or, True}
import smt.solver.Z3Solver

import scala.language.postfixOps

class GroundPathDepthLimitEncoding(program: Program, debug: Int = 0) extends Entailment {
  // Path prefix to be used for the basename in the SMT encoding
  private val PathPrefix: String = "pth_"

  // Sort names
  private val SortNameClass: String = "Class"
  private val SortNameVariable: String = "Variable"
  private val SortNamePath: String = "Path"

  // Sorts
  private val SortClass: Sort = SimpleSymbol(SortNameClass)
  private val SortVariable: Sort = SimpleSymbol(SortNameVariable)
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

    val depthLimit = determineDepthLimit(context, conclusion)
    val paths = enumeratePaths(variableNames, fieldNames, depthLimit)

    val (datatypeDeclarations, pathDatatypeExists, classDatatypeExists) = constructTypeDeclarations(classNames, variableNames, fieldNames, paths)

//    println("Variable names:")
//    variableNames foreach (x => println(s"\t$x"))
//    println("classNames names:")
//    classNames foreach (x => println(s"\t$x"))
//    println("paths:")
//    paths foreach (x => println(s"\t$x"))

    val constraintPropositionDeclarations = constructConstraintPropositions(pathDatatypeExists, classDatatypeExists)
    val substitutionFunctionDeclaration = generateSubstitutionFunction(paths, variableNames, depthLimit, pathDatatypeExists)

    val cReflRules = constructReflRules(paths)
    val cClassRules = constructClassRules(paths, classNames map (cls => Id(Symbol(cls)))) // TODO: update class name extraction to return IDs
    val cSubstPathEqRules = constructSubstPathEqRules(paths, variableNames map (x => Id(Symbol(x)))) // TODO: update variable name extraction to return IDs
    val cSubstInstOfRules = constructSubstInstOfRules(paths, classNames map (cls => Id(Symbol(cls))), variableNames map (x => Id(Symbol(x))))
    val cSubstInstByRules = constructSubstInstByRules(paths, classNames map (cls => Id(Symbol(cls))), variableNames map (x => Id(Symbol(x))))
    val cProgRules = constructCProgRules(paths, depthLimit, classDatatypeExists)

    val entailmentJudgement = constructEntailmentJudgement(context, conclusion)

/*    println("datatypes:")
    datatypeDeclarations.commands foreach (x => println(s"\t${x.pretty}"))
    println("constraints:")
    constraintPropositionDeclarations.commands foreach (x => println(s"\t${x.pretty}"))
    println("substitution:")
    substitutionFunctionDeclaration.commands foreach (x => println(s"\t${x.pretty}"))
    println("refl rules:")
    cReflRules.commands foreach (x => println(s"\t${x.pretty}"))*/

    datatypeDeclarations ++
      constraintPropositionDeclarations ++
      substitutionFunctionDeclaration ++
      cReflRules ++
      cClassRules ++
      cSubstPathEqRules ++
      cSubstInstOfRules ++
      cSubstInstByRules ++
      cProgRules ++
      entailmentJudgement
  }

  private def determineDepthLimit(context: List[Constraint], conclusion: Constraint): Int = {
    (derivationPaths(context, conclusion) map (_.depth)).foldRight(0)((i, is) => math.max(i, is))
  }

  // S
  private def programEntailmentPaths(x: Id): Set[Path] = (program flatMap {
    // if x==x1 (unifying binder equals entailment binder) substitution doesn't change a thing
    case ConstraintEntailment(x1, ctx, InstanceOf(y, _)) if x1==y => ctx.flatMap(_.containedPaths).map(substitute(x1, x, _))
    case _ => Nil
  }).toSet

  // S'
  private def CAProgPaths(constraints: List[Constraint], x: Id): Set[Path] = programEntailmentPaths(x) flatMap {
    p => constraints.flatMap(_.containedPaths).map(substitute(x, _, p)).toSet
  }

  // S''
  private def derivationPaths(context: List[Constraint], conclusion: Constraint): Set[Path] = {
    // using a static binder for unification is fine
    val unifyingVar: Id = Id(Symbol(s"unify"))

    CAProgPaths(context, unifyingVar) union ((conclusion::context) flatMap (_.containedPaths) toSet)
  }

  private def constructTypeDeclarations(classes: List[String], variables: List[String], fields: List[String], paths: List[Path]): (SMTLibScript, Boolean, Boolean) = {
    var declarations: SMTLibScript = SMTLibScript(Seq(SMTLib.buildEnumerationType(SortNameVariable, variables)))

    val doAddClassDatatype = classes.nonEmpty
    if (doAddClassDatatype)
      declarations =  declarations :+ SMTLib.buildEnumerationType(SortNameClass, classes)

    val doAddPathDatatype = fields.nonEmpty
    if (doAddPathDatatype)
      declarations =  declarations :+ SMTLib.buildEnumerationType(SortNamePath, paths)

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

  private def constructReflRules(paths: List[Path]): SMTLibScript =
    SMTLibScript(paths map cReflRuleTemplate)

  private def constructClassRules(paths: List[Path], classes: List[Id]): SMTLibScript =
    SMTLibScript(paths flatMap (p => classes map (cls => cClassRuleTemplate(p, cls))))

  private def constructSubstPathEqRules(paths: List[Path], vars: List[Id]): SMTLibScript =
    SMTLibScript(paths flatMap (
      p => paths flatMap (
        q => vars flatMap (
          x => paths flatMap (
            r => paths flatMap (
              s => paths flatMap (
                pr => paths flatMap (
                  qr => paths flatMap (
                    ps => paths map (
                      qs => cSubstPathEqTemplate(p, q, x, r, s, pr, qr, ps, qs)
                    )
                  )
                )
              )
            )
          )
        )
      )
    ))

  private def constructSubstInstOfRules(paths: List[Path], classes: List[Id], vars: List[Id]): SMTLibScript =
    SMTLibScript(paths flatMap (
      p => classes flatMap (
        cls => vars flatMap (
          x => paths flatMap (
            r => paths flatMap (
              s => paths flatMap (
                pr => paths map (
                  ps => cSubstInstOfTemplate(p, cls, x, r, s, pr, ps)
                )
              )
            )
          )
        )
      )
    ))

  private def constructSubstInstByRules(paths: List[Path], classes: List[Id], vars: List[Id]): SMTLibScript =
    SMTLibScript(paths flatMap (
      p => classes flatMap (
        cls => vars flatMap (
          x => paths flatMap (
            r => paths flatMap (
              s => paths flatMap (
                pr => paths map (
                  ps => cSubstInstByTemplate(p, cls, x, r, s, pr, ps)
                  )
                )
              )
            )
          )
        )
      ))

  private def constructCProgRules(paths: List[Path], depthLimit: Int, classDatatypeExists: Boolean): SMTLibScript = {
    if (!classDatatypeExists)
      SMTLibScript.EMPTY
    else
    // TODO: why does collect not work here (see simplified example in Foo)
    //      SMTLibScript(program.collect(constructProgRule(_, pathDatatypeExists)))
      program.filter(constructProgRules.isDefinedAt(_, paths, depthLimit)).foldRight(SMTLibScript.EMPTY)( (elem, acc) => constructProgRules(elem, paths, depthLimit) ++ acc)
  }

  val constructProgRules: PartialFunction[(Declaration, List[Path], Int), SMTLibScript] = {
    case (ConstraintEntailment(x, context, InstanceOf(y, cls)), paths, depthLimit) if x==y && context.nonEmpty =>
      var rules: SMTLibScript = SMTLibScript.EMPTY

      paths.foreach{ pDCC =>
        val ctx = context.map(substitute(x, pDCC, _))
        // We need to respect the depth limit when substituting

        // Possibility 1: Ignore the depth limit. Obviously bad.
        // val rule = cProgRuleTemplate(context, pDCC, cls)
        // rules = rules :+ rule

        // Possibility 2: Only remove the constraints from the context that exceed the depth limit
        //                No good solution. Could lead to possible wrong conclusions. E.g. (assert (=> (and (instance-of pth_x.p Succ)) (instance-of pth_x.p Nat)))
        // val rule = cProgRuleTemplate(ctx.filter(_.maxPathDepth <= depthLimit), pDCC, cls)
        // rules = rules :+ rule

        // Possibility 3: Discard the rule if one of the paths exceeds the limit.
        if (ctx.forall(_.maxPathDepth <= depthLimit)){
          val rule = cProgRuleTemplate(ctx, pDCC, cls)
          rules = rules :+ rule
        }
      }

      rules
  }

  private def cReflRuleTemplate(path: Path):SMTLibCommand = {
    val p = PathToSMTLibSymbol(path)
    Assert(PathEq(p, p))
  }

  private def cClassRuleTemplate(path: Path, cls: Id): SMTLibCommand = {
    val p = PathToSMTLibSymbol(path)
    val c = IdToSMTLibSymbol(cls)

    Assert(Implies(
      InstBy(p, c),
      InstOf(p, c)
    ))
  }

  // TODO: Optimize subst rule templates:
  //  - remove substitution results from parameters (pr, ps, ...)
  //  - calculate substitution results based on the given inputs
  //  - this leads to the substitute predicate to be true
  //  - remove the substitute predicate checks in the encoding (as they are guaranteed to be true)
  //  - if this is possible in all rules using the substitute predicate, remove it altogether (check prog rule)
  private def cSubstPathEqTemplate(p: Path, q: Path, x: Id, r: Path, s: Path, pr: Path, qr: Path, ps: Path, qs: Path): SMTLibCommand = {
    val pSMTLib = PathToSMTLibSymbol(p)
    val qSMTLib = PathToSMTLibSymbol(q)
    val xSMTLib = IdToSMTLibSymbol(x)
    val rSMTLib = PathToSMTLibSymbol(r)
    val sSMTLib = PathToSMTLibSymbol(s)
    val prSMTLib = PathToSMTLibSymbol(pr)
    val qrSMTLib = PathToSMTLibSymbol(qr)
    val psSMTLib = PathToSMTLibSymbol(ps)
    val qsSMTLib = PathToSMTLibSymbol(qs)

    Assert(Implies(
      And(
        PathEq(sSMTLib, rSMTLib),
        Subst(pSMTLib, xSMTLib, rSMTLib, prSMTLib),
        Subst(qSMTLib, xSMTLib, rSMTLib, qrSMTLib),
        Subst(pSMTLib, xSMTLib, sSMTLib, psSMTLib),
        Subst(qSMTLib, xSMTLib, sSMTLib, qsSMTLib),
        PathEq(prSMTLib, qrSMTLib)
      ),
      PathEq(psSMTLib, qsSMTLib)
    ))
  }

  private def cSubstInstOfTemplate(p: Path, cls: Id, x: Id, r: Path, s: Path, pr: Path, ps: Path): SMTLibCommand = {
    val pSMTLib = PathToSMTLibSymbol(p)
    val clsSMTLib = IdToSMTLibSymbol(cls)
    val xSMTLib = IdToSMTLibSymbol(x)
    val rSMTLib = PathToSMTLibSymbol(r)
    val sSMTLib = PathToSMTLibSymbol(s)
    val prSMTLib = PathToSMTLibSymbol(pr)
    val psSMTLib = PathToSMTLibSymbol(ps)

    Assert(Implies(
      And(
        PathEq(sSMTLib, rSMTLib),
        Subst(pSMTLib, xSMTLib, rSMTLib, prSMTLib),
        Subst(pSMTLib, xSMTLib, sSMTLib, psSMTLib),
        InstOf(prSMTLib, clsSMTLib)
      ),
      InstOf(psSMTLib, clsSMTLib)
    ))
  }

  private def cSubstInstByTemplate(p: Path, cls: Id, x: Id, r: Path, s: Path, pr: Path, ps: Path): SMTLibCommand = {
    val pSMTLib = PathToSMTLibSymbol(p)
    val clsSMTLib = IdToSMTLibSymbol(cls)
    val xSMTLib = IdToSMTLibSymbol(x)
    val rSMTLib = PathToSMTLibSymbol(r)
    val sSMTLib = PathToSMTLibSymbol(s)
    val prSMTLib = PathToSMTLibSymbol(pr)
    val psSMTLib = PathToSMTLibSymbol(ps)

    Assert(Implies(
      And(
        PathEq(sSMTLib, rSMTLib),
        Subst(pSMTLib, xSMTLib, rSMTLib, prSMTLib),
        Subst(pSMTLib, xSMTLib, sSMTLib, psSMTLib),
        InstBy(prSMTLib, clsSMTLib)
      ),
      InstBy(psSMTLib, clsSMTLib)
    ))
  }

  private def cProgRuleTemplate(context: List[Constraint], path: Path, cls: Id): SMTLibCommand = {

    val lhs =
      if (context.size == 1) {
        ConstraintToTerm(context.head)
      } else {
        And(context.map(ConstraintToTerm): _*)
      }

    Assert(
      Implies(
        lhs,
        Apply(FunctionInstanceOf, Seq(PathToSMTLibSymbol(path), IdToSMTLibSymbol(cls)))
      )
    )
  }

  private def constructEntailmentJudgement(context: List[Constraint], conclusion: Constraint): SMTLibScript = {
    // prefix the paths, since we prefixed the paths during enumeration

    def prefixConstraint(c: Constraint): Constraint = c match {
      case PathEquivalence(p, q) => PathEquivalence(p.prefixBaseName(PathPrefix), q.prefixBaseName(PathPrefix))
      case InstanceOf(p, cls) => InstanceOf(p.prefixBaseName(PathPrefix), cls)
      case InstantiatedBy(p, cls) => InstantiatedBy(p.prefixBaseName(PathPrefix), cls)
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
          // prefix target such that it matches the prefixed paths
          val prefixedTarget = Id(Symbol(PathPrefix)) + target

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

  private def PathEq(p: Term, q: Term): Term = Sugar.Op(FunctionPathEquivalence)(p, q)

  private def InstOf(p: Term, cls: Term): Term = Sugar.Op(FunctionInstanceOf)(p, cls)

  private def InstBy(p: Term, cls: Term): Term = Sugar.Op(FunctionInstantiatedBy)(p, cls)

  private def Subst(source: Term, target: Term, replace: Term, result: Term): Term = Sugar.Op(FunctionSubstitution)(source, target, replace, result)

  def enumeratePaths(vars: List[String], fields: List[String], depthLimit: Int): List[Path] = {
    // Initialize paths with variables
    var paths: List[Path] = vars.map(s => Id(Symbol(s"$PathPrefix$s"))) // prefix to not have ambiguous names between variables and paths

    // start with 1, as zero length is init
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
//  private var classCounter = 0
//  private def freshClassVar(): SimpleSymbol = {
//    classCounter = classCounter + 1
//    SimpleSymbol(s"cls$classCounter")
//  }

  private def resetFreshNameCounter(): Unit = {
    varCounter = 0
    pathCounter = 0
//    classCounter = 0
  }
}
