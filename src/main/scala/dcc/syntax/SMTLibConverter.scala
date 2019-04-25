package dcc.syntax

import dcc.syntax.Program.Program
import dcc.Util._
import smtlib.SMTLibCommand
import smtlib.solver.Axioms
import smtlib.syntax._

object SMTLibConverter {
  def convertConstraint(c: Constraint): Term = c match {
    case PathEquivalence(p, q) => Axioms.pathEq(convertPath(p), convertPath(q))
    case InstanceOf(p, cls) => Axioms.instanceOf(convertPath(p), cls.toString)
    case InstantiatedBy(p, cls) => Axioms.instantiatedBy(convertPath(p), cls.toString)
  }

  def convertPath(p: Path): Term = p match {
    case Id(x) => Apply(SimpleSymbol("var"), Seq(SMTLibString(x.name)))
    case FieldPath(q, Id(f)) => Apply(SimpleSymbol("pth"), Seq(convertPath(q), SMTLibString(f.name)))
  }

  // TODO: change type back to Term?
  def convertId(x: Id): SMTLibString = SMTLibString(x.toString)

  def makeProgramEntailmentLookupFunction(p: Program, paths: List[Path]): SMTLibCommand = {
    val x = SimpleSymbol("c")
    val body = makeProgramEntailmentLookupFunctionBody(paths.flatMap(instantiateProgramEntailments(p, _)), x)

    DefineFun(FunctionDef(
      SimpleSymbol("lookup-program-entailment"),
      Seq(SortedVar(x, SimpleSymbol("Constraint"))),
      SimpleSymbol("CsList"), //Sorts(SimpleSymbol("List"), Seq(Sorts(SimpleSymbol("List"), Seq(SimpleSymbol("Constraint"))))),
      body
    ))
  }

  private def instantiateProgramEntailments(p: Program, path: Path, entailments: Map[Constraint, List[List[Constraint]]] = Map()): Map[Constraint, List[List[Constraint]]] = p match {
    case Nil => entailments
    case ConstraintEntailment(x, as, a) :: rst =>
      val cs: List[Constraint] = substitute(x, path, as)
      val c: Constraint = substitute(x, path, a)

      entailments.get(c) match {
        case None => instantiateProgramEntailments(rst, path, entailments + (c -> List(cs)))
        case Some(ccs) => instantiateProgramEntailments(rst, path, entailments + (c -> (cs :: ccs)))
      }
    case _ :: rst => instantiateProgramEntailments(rst, path, entailments)
  }

//  private def instantiateProgramEntailments(p: Program, variable: Id, entailments: Map[Constraint, List[List[Constraint]]] = Map()): Map[Constraint, List[List[Constraint]]] = p match {
//    case Nil => entailments
//    case ConstraintEntailment(x, as, a) :: rst =>
//      val cs = alphaConversion(x, variable, as)
//      val c = renameIdInConstraint(x, variable, a)
//
//      entailments.get(c) match {
//        case None => instantiateProgramEntailments(rst, variable, entailments + (c -> List(cs)))
//        case Some(ccs) => instantiateProgramEntailments(rst, variable, entailments + (c -> (cs :: ccs)))
//      }
//    case _ :: rst => instantiateProgramEntailments(rst, variable, entailments)
//  }

  private def makeProgramEntailmentLookupFunctionBody(entailments: List[(Constraint, List[List[Constraint]])], x: Term): Term = entailments match {
    case Nil => SimpleSymbol("nan") //IdentifierAs(SimpleSymbol("nil"), Sorts(SimpleSymbol("List"), Seq(Sorts(SimpleSymbol("List"), Seq(SimpleSymbol("Constraint")))))) // TODO: change to something else for no hit?
    case (c, ccs) :: rst =>
      Ite(
        Eq(x, convertConstraint(c)),
        Axioms.makeCsList(
          ccs.map(cs => Axioms.makeList(cs.map(convertConstraint)))
        ),
        makeProgramEntailmentLookupFunctionBody(rst, x)
      )
  }

  def convertEntailment(ctx: List[Constraint], c: Constraint): Term = {
    val ctxSMTLib: Term = SMTLibConverter.makeList(ctx.map(SMTLibConverter.convertConstraint))
    val cSMTLib: Term = SMTLibConverter.convertConstraint(c)

    Apply(SimpleSymbol("entails"), Seq(ctxSMTLib, cSMTLib))
  }

  def makeAsserts(terms: List[Term]): List[SMTLibCommand] =
    terms.map(t => Assert(t))

  def convertVariablesPathsClasses(constraints: List[Constraint]): (List[Term], List[Term], List[Term]) = {
    val (vars, paths, classes) = extractVariablesPathsClasses(constraints)

    (
      vars.map(x => Apply(SimpleSymbol("variable"), Seq(SMTLibString(x)))),
      paths.map(p => Apply(SimpleSymbol("path-exists"), Seq(convertPath(p)))),
      classes.map(cls => Apply(SimpleSymbol("class"), Seq(SMTLibString(cls))))
    )
  }

  private def extractVariablesPathsClasses
    (constraints: List[Constraint],
     vars: List[String] = List(),
     paths: List[Path] = List(),
     classes: List[String] = List())
    : (List[String], List[Path], List[String]) = constraints match {
      case Nil => (vars, paths, classes)
      case PathEquivalence(p, q) :: rst =>
        var vars1: List[String] = vars
        var paths1: List[Path] = paths

        if(!vars1.contains(objectName(p)))
          vars1 = objectName(p) :: vars1
        if(!vars1.contains(objectName(q)))
          vars1 = objectName(q) :: vars1

        if(!paths1.contains(p))
          paths1 = p :: paths1
        if(!paths1.contains(q))
          paths1 = q :: paths1

        extractVariablesPathsClasses(rst, vars1, paths1, classes)
      case InstanceOf(p, cls) :: rst =>
        var vars1: List[String] = vars
        var paths1: List[Path] = paths
        var classes1: List[String] = classes

        if(!vars1.contains(objectName(p)))
          vars1 = objectName(p) :: vars1

        if(!paths1.contains(p))
          paths1 = p :: paths1

        if(!classes1.contains(cls.toString))
          classes1 = cls.toString :: classes1

        extractVariablesPathsClasses(rst, vars1, paths1, classes1)
      case InstantiatedBy(p, cls) :: rst =>
        var vars1: List[String] = vars
        var paths1: List[Path] = paths
        var classes1: List[String] = classes

        if(!vars1.contains(objectName(p)))
          vars1 = objectName(p) :: vars1

        if(!paths1.contains(p))
          paths1 = p :: paths1

        if(!classes1.contains(cls.toString))
          classes1 = cls.toString :: classes1

        extractVariablesPathsClasses(rst, vars1, paths1, classes1)
  }

  def convertVariables(constraints: List[Constraint]): List[Term] =
    extractVariables(constraints).map(x => Apply(SimpleSymbol("variable"), Seq(SMTLibString(x))))

  private def extractVariables(constraints: List[Constraint], vars: List[String] = List()): List[String] = constraints match {
    case Nil => vars
    case PathEquivalence(p, q) :: rst if !vars.contains(objectName(p)) && !vars.contains(objectName(q)) =>
      extractVariables(rst, objectName(p) :: objectName(q) :: vars)
    case PathEquivalence(p, _) :: rst if !vars.contains(objectName(p)) =>
      extractVariables(rst, objectName(p) :: vars)
    case PathEquivalence(_, q) :: rst if !vars.contains(objectName(q)) =>
      extractVariables(rst, objectName(q) :: vars)
    case InstanceOf(p, cls) :: rst if !vars.contains(objectName(p)) =>
      extractVariables(rst, objectName(p) :: vars)
    case InstantiatedBy(p, cls) :: rst if !vars.contains(objectName(p)) =>
      extractVariables(rst, objectName(p) :: vars)
    case _ :: rst => extractVariables(rst, vars)
  }

  private def objectName(p: Path): String = p match {
    case Id(x) => x.name
    case FieldPath(q, _) => objectName(q)
  }

  // TODO: remove. use Axioms.makeList (moved from here)
  private def makeList(terms: Seq[Term]): Term = terms match {
    case Nil => SimpleSymbol("nil")
    case t :: rst => Apply(SimpleSymbol("insert"), Seq(t, makeList(rst)))
  }
}
