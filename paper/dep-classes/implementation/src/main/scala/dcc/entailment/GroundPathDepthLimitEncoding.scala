package dcc.entailment
import dcc.Util.{prefixedSubstitute, substitute}
import dcc.syntax.{Constraint, ConstraintEntailment, Declaration, FieldPath, Id, InstanceOf, InstantiatedBy, Path, PathEquivalence, Util}
import dcc.syntax.Program.{DefinedClassNames, DefinedFieldNames, Program}
import smt.smtlib.{SMTLib, SMTLibCommand, SMTLibScript}
import smt.smtlib.syntax.{Apply, Assert, DeclareFun, SMTLibSymbol, SimpleSymbol, Sort, Sugar, Term, Unsat}
import smt.smtlib.theory.BoolPredefined.{And, Bool, Implies, Not, True}
import smt.solver.Z3Solver

import scala.collection.mutable
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

    val variableNames = extractVariableNames(conclusion :: context)
    val fieldNames = DefinedFieldNames(program)
    val classNames = DefinedClassNames(program)

    val depthLimit = determineDepthLimit(context, conclusion)
    val paths = enumeratePaths(variableNames, fieldNames, depthLimit)

    val (datatypeDeclarations, pathDatatypeExists, classDatatypeExists) = constructTypeDeclarations(classNames, variableNames, fieldNames, paths)

    val constraintPropositionDeclarations = constructConstraintPropositions(pathDatatypeExists, classDatatypeExists)

    // TODO:
    //  - update class name extraction to return IDs?
    //  - update variable name extraction to return IDs?
    val staticRules = constructStaticRules(paths=paths, classes=classNames map (cls => Id(Symbol(cls))), vars = variableNames map (x => Id(Symbol(x))), depthLimit)

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
      staticRules ++
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

  // rule generation with only one iteration:
  //  ✓ C-Refl
  //  ✓ C-Class
  //  ✓ C-SubstPathEq
  //  ✓ C-SubstInstBy
  //  ✓ C-SubstInstBy
  private def constructStaticRules(paths: List[Path], classes: List[Id], vars: List[Id], depthLimit: Int): SMTLibScript = {
    val rules: mutable.HashSet[SMTLibCommand] = new mutable.HashSet[SMTLibCommand]()

    for (p <- paths) {
      rules += cReflRuleTemplate(p)
      for (cls <- classes) {
        rules += cClassRuleTemplate(p, cls)
        for (x <- vars) {
          for (r <- paths) {
            for (s <- paths) {
              // TODO: merge InstOf and InstBy templates?
              if (cSubstInstOfTemplate.isDefinedAt(p, cls, x, r, s, depthLimit))
                rules += cSubstInstOfTemplate(p, cls, x, r, s, depthLimit)

              if (cSubstInstByTemplate.isDefinedAt(p, cls, x, r, s, depthLimit))
                rules += cSubstInstByTemplate(p, cls, x, r, s, depthLimit)

              for (q <- paths) {
                if (cSubstPathEqTemplate.isDefinedAt(p, q, x, r, s, depthLimit))
                  rules += cSubstPathEqTemplate(p, q, x, r, s, depthLimit)
              }
            }
          }
        }
      }
    }

    SMTLibScript(rules.toList)
  }

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

  // Optimize subst rule templates:
  //  ✓ remove substitution results from parameters (pr, ps, ...)
  //  ✓ calculate substitution results based on the given inputs
  //  ✓ this leads to the substitute predicate to be true
  //  ✓ change templates to be partial functions, that are only defined if the substitution result is within the depth limit
  //  ✓ remove the substitute predicate checks in the encoding (as they are guaranteed to be true)
  //  ✓ if this is possible in all rules using the substitute predicate, remove it altogether
  private val cSubstPathEqTemplate: PartialFunction[(Path, Path, Id, Path, Path, Int), SMTLibCommand] = {
    case (p, q, x, r, s, limit)
      if prefixSubst(p, x, r).depth <= limit && // TODO: move check to call site to avoid calculating the substitution twice?
        prefixSubst(q, x, r).depth <= limit &&
        prefixSubst(p, x, s).depth <= limit &&
        prefixSubst(q, x, s).depth <= limit =>
      val rSMTLib = PathToSMTLibSymbol(r)
      val sSMTLib = PathToSMTLibSymbol(s)
      val prSMTLib = PathToSMTLibSymbol(prefixSubst(p, x, r))
      val qrSMTLib = PathToSMTLibSymbol(prefixSubst(q, x, r))
      val psSMTLib = PathToSMTLibSymbol(prefixSubst(p, x, s))
      val qsSMTLib = PathToSMTLibSymbol(prefixSubst(q, x, s))

      Assert(Implies(
        And(
          PathEq(sSMTLib, rSMTLib),
          PathEq(prSMTLib, qrSMTLib)
        ),
        PathEq(psSMTLib, qsSMTLib)
      ))
  }

  private val cSubstInstOfTemplate: PartialFunction[(Path, Id, Id, Path, Path, Int), SMTLibCommand] = {
    case (p, cls, x, r, s, limit)
      if prefixSubst(p, x, r).depth <= limit &&
        prefixSubst(p, x, s).depth <= limit =>
      val clsSMTLib = IdToSMTLibSymbol(cls)
      val rSMTLib = PathToSMTLibSymbol(r)
      val sSMTLib = PathToSMTLibSymbol(s)
      val prSMTLib = PathToSMTLibSymbol(prefixSubst(p, x, r))
      val psSMTLib = PathToSMTLibSymbol(prefixSubst(p, x, s))

      Assert(Implies(
        And(
          PathEq(sSMTLib, rSMTLib),
          InstOf(prSMTLib, clsSMTLib)
        ),
        InstOf(psSMTLib, clsSMTLib)
      ))
  }

  private val cSubstInstByTemplate: PartialFunction[(Path, Id, Id, Path, Path, Int), SMTLibCommand] = {
    case (p, cls, x, r, s, limit)
      if prefixSubst(p, x, r).depth <= limit &&
        prefixSubst(p, x, s).depth <= limit =>
      val clsSMTLib = IdToSMTLibSymbol(cls)
      val rSMTLib = PathToSMTLibSymbol(r)
      val sSMTLib = PathToSMTLibSymbol(s)
      val prSMTLib = PathToSMTLibSymbol(prefixSubst(p, x, r))
      val psSMTLib = PathToSMTLibSymbol(prefixSubst(p, x, s))

      Assert(Implies(
        And(
          PathEq(sSMTLib, rSMTLib),
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

  private def PathEq(p: Term, q: Term): Term = Sugar.Op(FunctionPathEquivalence)(p, q)

  private def InstOf(p: Term, cls: Term): Term = Sugar.Op(FunctionInstanceOf)(p, cls)

  private def InstBy(p: Term, cls: Term): Term = Sugar.Op(FunctionInstantiatedBy)(p, cls)

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

  private val prefixSubst = (source: Path, target: Id, replace: Path) => prefixedSubstitute(PathPrefix)(source, target, replace)
}
