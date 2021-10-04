package dcc.entailment
import dcc.syntax.Program.Program
import dcc.syntax.{Constraint, ConstraintEntailment, ConstructorDeclaration, FieldPath, Id, InstanceOf, InstantiatedBy, Path, PathEquivalence, Util}
import smt.smtlib.SMTLib.{is, selector}
import smt.smtlib.{SMTLib, SMTLibCommand, SMTLibScript}
import smt.smtlib.syntax.{Apply, Assert, ConstructorDatatype, ConstructorDec, DeclareDatatype, DeclareFun, DefineFun, DefineFunRec, Forall, FunctionDef, SMTLibSymbol, SelectorDec, SimpleSymbol, Sort, SortedVar, Term, Unsat}
import smt.smtlib.theory.BoolPredefined.{And, Bool, Eq, Implies, Ite, Not, True}
import smt.solver.Z3Solver

import scala.language.postfixOps

class SimplifiedSemanticEntailment(program: Program, debug: Int = 0) extends Entailment {
  // TODO: refresh variable names to ensure that we do not have ambiguous constructor names?
  //  eg Variable = {p} and Field = {p}

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

  // Datatype constructor- and selector names
  private val DatatypeConstructorPathBase: SimpleSymbol = SimpleSymbol("path-base")
  private val DatatypeSelectorPathBase: SimpleSymbol = SimpleSymbol("path-id")
  private val DatatypeConstructorPathExtension: SimpleSymbol = SimpleSymbol("path-ext")
  private val DatatypeSelectorPathExtensionObject: SimpleSymbol = SimpleSymbol("path-object")
  private val DatatypeSelectorPathExtensionField: SimpleSymbol = SimpleSymbol("path-field")

  // Function names
  private val FunctionPathEquivalence: SMTLibSymbol = SimpleSymbol("path-equivalence")
  private val FunctionInstanceOf: SMTLibSymbol = SimpleSymbol("instance-of")
  private val FunctionInstantiatedBy: SMTLibSymbol = SimpleSymbol("instantiated-by")
  private val FunctionSubstitution: SMTLibSymbol = SimpleSymbol("substitute")

  override def entails(context: List[Constraint], constraint: Constraint): Boolean = {
    if (debug > 0)
      println(s"entailment: ${if (context.isEmpty) "·" else Util.commaSeparate(context)} |- $constraint")

    val smt = encoding(context, Some(constraint))
    val solver = new Z3Solver(smt, debug = if (debug>2) true else false) // TODO: change debug in solver to be an int as well (none, print FO formulae, print solver I/O)

    solver.checkSat match {
      case Left(Unsat)  => true
      case Left(_)      => false
      case Right(errors) =>
        System.err.println("Entailment check error:")
        errors foreach { error => System.err.println(error.format) }
        false
    }
  }

  override def entails(context: List[Constraint], constraints: List[Constraint]): Boolean = constraints.forall(entails(context, _))

  def encoding(context: List[Constraint], conclusion: Option[Constraint]): SMTLibScript = {
    // Reset counters for fresh name generation
    resetFreshNameCounter()

    val variableNames = extractVariableNames(if (conclusion.isEmpty) context else conclusion.get :: context)
    val fieldNames = getFieldNamesDefinedInProgram // SMT error if an entailment includes a field not mentioned an a constructor
    val classNames = getClassNamesDefinedInProgram // SMT error if an entailment includes a class not mentioned in the program

    val (datatypeDeclarations, pathDatatypeExists, classDatatypeExists) = constructTypeDeclarations(classNames, variableNames, fieldNames)
    val functionDeclarations = constructFunctionDeclarations(pathDatatypeExists, classDatatypeExists)
    val calculusRules = constructCalculusRules(pathDatatypeExists, classDatatypeExists)
    val entailmentJudgement = constructEntailmentJudgement(context, conclusion, pathDatatypeExists)

    val smt: SMTLibScript = datatypeDeclarations ++ functionDeclarations ++ entailmentJudgement ++ calculusRules
    if (debug > 1) {
      println("First-Order Encoding:")
      println(smt.pretty)
    }
    smt
  }

  private def constructPathDatatype: SMTLibCommand = DeclareDatatype(SimpleSymbol(SortNamePath), ConstructorDatatype(Seq(
    ConstructorDec(DatatypeConstructorPathBase, Seq(SelectorDec(DatatypeSelectorPathBase, SortVariable))),
    ConstructorDec(DatatypeConstructorPathExtension, Seq(SelectorDec(DatatypeSelectorPathExtensionObject, SortPath), SelectorDec(DatatypeSelectorPathExtensionField, SortField)))
  )))

  private def constructTypeDeclarations(classes: List[String], variables: List[String], fields: List[String]): (SMTLibScript, Boolean, Boolean) = {
    // variables can never be empty (theoretically we could call _encoding_ with an empty list, but from a DCC perspective we have at least one variable at all times)
    var declarations: SMTLibScript = SMTLibScript(Seq(SMTLib.buildEnumerationType(SortNameVariable, variables)))

    if (fields.nonEmpty)
      declarations = declarations :+ SMTLib.buildEnumerationType(SortNameField, fields)

    val doAddClassDatatype = classes.nonEmpty
    if (doAddClassDatatype)
      declarations = declarations :+ SMTLib.buildEnumerationType(SortNameClass, classes)

    if (fields.isEmpty)
      (declarations, false, doAddClassDatatype)
    else
      (declarations :+ constructPathDatatype, true, doAddClassDatatype)
  }

  private def constructFunctionDeclarations(pathDatatypeExists: Boolean, classDatatypeExists: Boolean): SMTLibScript = {
    val path: Sort = if (pathDatatypeExists) SortPath else SortVariable

    var declarations: SMTLibScript = SMTLibScript(Seq(DeclareFun(FunctionPathEquivalence, Seq(path, path), Bool)))

    if (classDatatypeExists)
      declarations = declarations :++ Seq(
        DeclareFun(FunctionInstanceOf, Seq(path, SortClass), Bool),
        DeclareFun(FunctionInstantiatedBy, Seq(path, SortClass), Bool)
      )

    declarations :+ constructSubstitutionFunction(pathDatatypeExists)
  }

  private def constructSubstitutionFunction(pathDatatypeExists: Boolean): SMTLibCommand = {
    val p: SMTLibSymbol = freshPath()
    val q: SMTLibSymbol = freshPath()
    val x: SMTLibSymbol = freshVariable()

    if (pathDatatypeExists) {
      DefineFunRec(FunctionDef(
        FunctionSubstitution,
        Seq(
          SortedVar(p, SortPath),
          SortedVar(x, SortVariable),
          SortedVar(q, SortPath)
        ),
        SortPath,
        Ite(
          is(DatatypeConstructorPathBase, p),
          Ite(Eq(x, selector(DatatypeSelectorPathBase, p)), q, p),
          Apply(
            DatatypeConstructorPathExtension,
            Seq(
              Apply(
                FunctionSubstitution,
                Seq(selector(DatatypeSelectorPathExtensionObject, p), x, q)),
              selector(DatatypeSelectorPathExtensionField, p)))
        )
      ))
    } else {
      DefineFun(FunctionDef(
        FunctionSubstitution,
        Seq(
          SortedVar(p, SortVariable),
          SortedVar(x, SortVariable),
          SortedVar(q, SortVariable)
        ),
        SortVariable,
        Ite(Eq(x, p), q, p)
      ))
    }
  }

  private def constructCalculusRules(pathDatatypeExists: Boolean, classDatatypeExists: Boolean): SMTLibScript = {
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

    // C-Subst-Eq
    val xSubstEq = freshVariable()
    val pSubstEq = freshPath()
    val qSubstEq = freshPath()
    val rSubstEq = freshPath()
    val sSubstEq = freshPath()
    val cSubstPathEq = Assert(Forall(
      Seq(
        SortedVar(pSubstEq, path),
        SortedVar(qSubstEq, path),
        SortedVar(xSubstEq, SortVariable),
        SortedVar(rSubstEq, path),
        SortedVar(sSubstEq, path)
      ),
      Implies(
        And(
          Apply(FunctionPathEquivalence, Seq(sSubstEq, rSubstEq)),
          Apply(FunctionPathEquivalence, Seq(
            Apply(FunctionSubstitution, Seq(pSubstEq, xSubstEq, rSubstEq)),
            Apply(FunctionSubstitution, Seq(qSubstEq, xSubstEq, rSubstEq)),
          ))
        ),
        Apply(FunctionPathEquivalence, Seq(
          Apply(FunctionSubstitution, Seq(pSubstEq, xSubstEq, sSubstEq)),
          Apply(FunctionSubstitution, Seq(qSubstEq, xSubstEq, sSubstEq))
        ))
      )
    ))

    // C-Subst-InstOf
    val xSubstOf = freshVariable()
    val pSubstOf = freshPath()
    val clsSubstOf = freshClassVar()
    val rSubstOf = freshPath()
    val sSubstOf = freshPath()
    val cSubstInstOf = Assert(Forall(
      Seq(
        SortedVar(pSubstOf, path),
        SortedVar(clsSubstOf, SortClass),
        SortedVar(xSubstOf, SortVariable),
        SortedVar(rSubstOf, path),
        SortedVar(sSubstOf, path)
      ),
      Implies(
        And(
          Apply(FunctionPathEquivalence, Seq(sSubstOf, rSubstOf)),
          Apply(FunctionInstanceOf, Seq(
            Apply(FunctionSubstitution, Seq(pSubstOf, xSubstOf, rSubstOf)),
            clsSubstOf
          ))
        ),
        Apply(FunctionInstanceOf, Seq(
          Apply(FunctionSubstitution, Seq(pSubstOf, xSubstOf, sSubstOf)),
          clsSubstOf
        ))
      )
    ))

    // C-Subst-InstBy
    val xSubstBy = freshVariable()
    val pSubstBy = freshPath()
    val clsSubstBy = freshClassVar()
    val rSubstBy = freshPath()
    val sSubstBy = freshPath()
    val cSubstInstBy = Assert(Forall(
      Seq(
        SortedVar(pSubstBy, path),
        SortedVar(clsSubstBy, SortClass),
        SortedVar(xSubstBy, SortVariable),
        SortedVar(rSubstBy, path),
        SortedVar(sSubstBy, path)
      ),
      Implies(
        And(
          Apply(FunctionPathEquivalence, Seq(sSubstBy, rSubstBy)),
          Apply(FunctionInstantiatedBy, Seq(
            Apply(FunctionSubstitution, Seq(pSubstBy, xSubstBy, rSubstBy)),
            clsSubstBy
          ))
        ),
        Apply(FunctionInstantiatedBy, Seq(
          Apply(FunctionSubstitution, Seq(pSubstBy, xSubstBy, sSubstBy)),
          clsSubstBy
        ))
      )
    ))

    if (classDatatypeExists)
      SMTLibScript(Seq(
        cReflexivity,
        cClass,
        cSubstPathEq,
        cSubstInstOf,
        cSubstInstBy
      )) ++ constructProgRules(pathDatatypeExists)
    else
      SMTLibScript(Seq(
        cReflexivity, cSubstPathEq
      ))
  }

  private def constructProgRules(pathDatatypeExists: Boolean): SMTLibScript = SMTLibScript(
    program.filter(_.isInstanceOf[ConstraintEntailment]).map(declaration => constructProgRule(declaration.asInstanceOf[ConstraintEntailment], pathDatatypeExists))
  )

  // TODO: test this specifically, to see if the output is what we want (quantified-path.f.g.h)
  // TODO: check whether it's ok to discard entailment declarations with paths containing fields if the path datatype doesn't exist based on the input entailment
  //  e.g. 'x :: Zero, y :: Succ |- y :: Nat' hints that it should be ok, as we couldn't apply C-Prog-Succ anyways
  //  but is this the case for all possibilities? (are there even other possibilities than these of the form above?)
  //  requirement: includes constraints containing classes, doesn't include fields
  //  :
  //  if so: filter those out and only use input constraints to search for fields and not the constructors declared in the program
  private def constructProgRule(declaration: ConstraintEntailment, pathDatatypeExists: Boolean): SMTLibCommand = declaration match {
    case ConstraintEntailment(x, context, InstanceOf(y, cls)) if x==y && context.nonEmpty =>
      val path = if (pathDatatypeExists) SortPath else SortVariable

      // TODO: how to pick a fresh name for this name (without a new function)?
      val pDCC: Path = MetaPath("quantified-path")
      val pSMT: SMTLibSymbol = SimpleSymbol(pDCC.baseName)

      Assert(Forall(
        Seq(SortedVar(pSMT, path)),
        Implies(
          if (context.size == 1)
            ConstraintToTerm(dcc.Util.substitute(x, pDCC, context.head), pathDatatypeExists)
          else
            And(context.map(constraint => ConstraintToTerm(dcc.Util.substitute(x, pDCC, constraint), pathDatatypeExists)): _*)
          ,
          Apply(FunctionInstanceOf, Seq(pSMT, IdToSMTLibSymbol(cls)))
        )
      ))
    case _ => Assert(True) // Not well formed
  }

  private def constructEntailmentJudgement(context: List[Constraint], conclusion: Option[Constraint], pathDatatypeExists: Boolean): SMTLibScript = conclusion match {
    case Some(value) if context.size == 1 =>
      SMTLibScript(Seq(Assert(Not(Implies(
        ConstraintToTerm(context.head, pathDatatypeExists),
        ConstraintToTerm(value, pathDatatypeExists)
      )))))
    case Some(value) =>
      val ctx: Seq[Term] = context map (constraint => ConstraintToTerm(constraint, pathDatatypeExists))
      SMTLibScript(Seq(Assert(Not(Implies(
        And(ctx: _*),
        ConstraintToTerm(value, pathDatatypeExists)
      )))))
//    case Some(value) => SMTLibScript(context map (constraint => Assert(ConstraintToTerm(constraint, pathDatatypeExists)))) :+ Assert(Not(ConstraintToTerm(value, pathDatatypeExists)))
    case None        => SMTLibScript(context map (constraint => Assert(ConstraintToTerm(constraint, pathDatatypeExists))))
  }

  // A valid class name is either introduces through a constructor or as the conclusion of an constraint entailment
  private def getClassNamesDefinedInProgram: List[String] = program flatMap {
    case ConstructorDeclaration(cls, _, _) => cls.name.name :: Nil
    case ConstraintEntailment(_, _, InstanceOf(_, cls)) => cls.name.name :: Nil
    case _ => Nil
  } distinct

  // All valid field names must be introduced via constructors.
  private def getFieldNamesDefinedInProgram: List[String] = program flatMap {
    case ConstructorDeclaration(_, _, as) => extractFieldNames(as)
    case _ => Nil
  }

  private def extractVariableNames(constraints: List[Constraint]): List[String] = constraints flatMap {
    case PathEquivalence(p, q) => List(p.baseName, q.baseName)
    case InstanceOf(p, _) => List(p.baseName)
    case InstantiatedBy(p, _) => List(p.baseName)
  } distinct

  private def extractFieldNames(constraints: List[Constraint]): List[String] = constraints flatMap {
    case PathEquivalence(p, q) => p.fieldNames ++ q.fieldNames
    case InstanceOf(p, _) => p.fieldNames
    case InstantiatedBy(p, _) => p.fieldNames
  } distinct

  private def IdToSMTLibSymbol(id: Id): SMTLibSymbol = SimpleSymbol(id.name.name)

  private def PathToTerm(path: Path, pathDatatypeExists: Boolean): Term = path match {
    case FieldPath(p, f) => Apply(DatatypeConstructorPathExtension, Seq(PathToTerm(p, pathDatatypeExists), IdToSMTLibSymbol(f)))
    case x@Id(_) => if (pathDatatypeExists) Apply(DatatypeConstructorPathBase, Seq(IdToSMTLibSymbol(x))) else IdToSMTLibSymbol(x)
    case MetaPath(s) => SimpleSymbol(s)
  }

  private def ConstraintToTerm(constraint: Constraint, pathDatatypeExists: Boolean): Term = constraint match {
    case PathEquivalence(p, q) => Apply(FunctionPathEquivalence, Seq(PathToTerm(p, pathDatatypeExists), PathToTerm(q, pathDatatypeExists)))
    case InstanceOf(p, cls) => Apply(FunctionInstanceOf, Seq(PathToTerm(p, pathDatatypeExists), IdToSMTLibSymbol(cls)))
    case InstantiatedBy(p, cls) => Apply(FunctionInstantiatedBy, Seq(PathToTerm(p, pathDatatypeExists), IdToSMTLibSymbol(cls)))
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

  private case class MetaPath(s: String) extends Path {
    override def toString: String = s
    override def baseName: String = s
    override def fieldNames: List[String] = Nil
    override def depth: Int = 0
  }
}
