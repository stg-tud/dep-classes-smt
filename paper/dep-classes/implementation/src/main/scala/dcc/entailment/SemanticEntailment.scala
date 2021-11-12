package dcc.entailment

import dcc.Util.substitute
import dcc.entailment.EntailmentSort.EntailmentSort
import dcc.entailment.SemanticEntailment.{Class, ConstraintToTerm, Field, IdToSymbol, MetaPath, Path, PathToTerm, Variable, constructorPth, constructorVar, functionInstanceOf, functionInstantiatedBy, functionPathEquivalence, functionSubstitution, selectorField, selectorId, selectorObj, sortClass, sortField, sortPath, sortVariable, substitutePath}
import dcc.syntax.{Constraint, ConstraintEntailment, FieldPath, Id, InstanceOf, InstantiatedBy, Path, PathEquivalence, Util}
import dcc.syntax.Program.{DefinedClasses, DefinedFields, Program}
import smt.smtlib.SMTLib.{buildEnumerationType, is, selector}
import smt.smtlib.syntax.Sugar.Op
import smt.smtlib.{SMTLibCommand, SMTLibScript}
import smt.smtlib.syntax.{Apply, Assert, ConstructorDatatype, ConstructorDec, DeclareDatatype, DeclareFun, DefineFun, DefineFunRec, Forall, FunctionDef, SMTLibSymbol, SelectorDec, SimpleSymbol, Sort, SortedVar, Term, Unsat}
import smt.smtlib.theory.BoolPredefined._
import smt.solver.Z3Solver

import scala.annotation.tailrec
import scala.language.postfixOps

// TODO: add timeout flag as Option[Int] default None
class SemanticEntailment(program: Program, debug: Int = 0) extends Entailment {
  override def typ: EntailmentSort = EntailmentSort.Semantic

  override def entails(context: List[Constraint], constraint: Constraint): Boolean = {
    if (debug > 0)
      println(s"entailment: ${Util.commaSeparate(context)} |- $constraint")

    val (smt, isPathDefined) = axioms(context, Some(constraint))
    val solver = new Z3Solver(smt, debug = if (debug > 2) true else false)

    if (context.isEmpty)
      solver.addCommand(Assert(Not(ConstraintToTerm(constraint, isPathDefined))))
    else
      solver.addCommand(Assert(Not(
        Implies(
          if (context.size == 1)
            ConstraintToTerm(context.head, isPathDefined)
          else
            Apply(SimpleSymbol("and"), context map {c => ConstraintToTerm(c, isPathDefined)}),
          ConstraintToTerm(constraint, isPathDefined)
        )
      )))

    // timeout needs to be set high enough that the solver will encounter a counter example if there exists one
    // it's not possible to get rid of the timeout altogether, because of recursion/quantifiers over infinite structures
    solver.timeout = 10000
    val response = solver.checkSat
    response match {
      case Left(Unsat) => true
      case Left(_) => false
      case Right(errors) =>
        errors foreach { e => System.err.println(e.format) }
        false
    }
  }

  override def entails(context: List[Constraint], constraints: List[Constraint]): Boolean = constraints.forall(entails(context, _))

  /**
    * SMTLib commands capturing the semantic translation of the constraint system
    * @return SMTLib script representing the semantic translation
    */
  def axioms(context: List[Constraint], constraint: Option[Constraint]): (SMTLibScript, Boolean) = {
    // TODO: Change this to be able to determine if the path datatype is defined without the pair return type
    val classes: List[Id] = DefinedClasses(program)
    val variables: List[Id] = getVariableNames ++ extractVariableNames(if (constraint.isEmpty) context else constraint.get :: context) distinct
    val fields: List[Id] = DefinedFields(program)

    val constraintEntailments: List[ConstraintEntailment] = program.filter(_.isInstanceOf[ConstraintEntailment]).map(_.asInstanceOf[ConstraintEntailment])

    val pathDatatype = generatePathDatatype(fields.nonEmpty)

    val sorts =
      if (pathDatatype.isDefined)
        generateEnumerationTypes(classes, variables, fields) :+ pathDatatype.get
      else
        generateEnumerationTypes(classes, variables, fields)

    val functions: SMTLibScript = generateFunctionDeclarations(pathDatatype.isDefined, classes.nonEmpty) :+
      generateSubstitutionFunction(pathDatatype.isDefined)

    (sorts ++
      functions ++
      generateCalculusRules(pathDatatype.isDefined, classes.nonEmpty) ++
      generateProgRules(constraintEntailments, pathDatatype.isDefined), pathDatatype.isDefined)
  }

  private def getVariableNames: List[Id] = program flatMap {
    case ConstraintEntailment(x, as, a) => x :: extractVariableNames(a :: as)
    case _ => Nil
  } distinct

  private def extractVariableNames(constraints: List[Constraint]): List[Id] = constraints flatMap {
    case InstanceOf(p, _) => List(p.base)
    case InstantiatedBy(p, _) => List(p.base)
    case PathEquivalence(p, q) => List(p.base, q.base)
  } distinct

  private def generateEnumerationTypes(classes: List[Id], variables: List[Id], fields: List[Id]): SMTLibScript = {
    if (classes.isEmpty && fields.isEmpty) {
      //  no classes → don't add class datatype
      //  no fields → don't add field datatype
      SMTLibScript(Seq(buildEnumerationType(sortVariable, variables)))
    }
    else if ( classes.isEmpty ) {
      //  no classes → don't add class datatype
      SMTLibScript(Seq(
        buildEnumerationType(sortVariable, variables),
        buildEnumerationType(sortField, fields)
      ))
    }
    else if (fields.isEmpty) {
      //  no fields → don't add field datatype
      SMTLibScript(Seq(
        buildEnumerationType(sortClass, classes),
        buildEnumerationType(sortVariable, variables)
      ))
    }
    else {
      SMTLibScript(Seq(
        buildEnumerationType(sortClass, classes),
        buildEnumerationType(sortVariable, variables),
        buildEnumerationType(sortField, fields)
      ))
    }
  }

  private def generatePathDatatype(fields: Boolean): Option[SMTLibCommand] = if (fields) {
    Some(DeclareDatatype(SimpleSymbol(sortPath), ConstructorDatatype(Seq(
      ConstructorDec(constructorVar, Seq(SelectorDec(selectorId, Variable))),
      ConstructorDec(constructorPth, Seq(SelectorDec(selectorObj, Path), SelectorDec(selectorField, Field)))
    ))))
  } else {
    None
  }

  private def generateFunctionDeclarations(isPathDefined: Boolean, isClassDefined: Boolean): SMTLibScript = {
    val sort: Sort = if (isPathDefined) Path else Variable

    if (isClassDefined) {
      SMTLibScript(Seq(
        DeclareFun(functionInstanceOf, Seq(sort, Class), Bool),
        DeclareFun(functionInstantiatedBy, Seq(sort, Class), Bool),
        DeclareFun(functionPathEquivalence, Seq(sort, sort), Bool)
      ))
    } else {
      SMTLibScript(Seq(DeclareFun(functionPathEquivalence, Seq(sort, sort), Bool)))
    }
  }

  def generateConstraintPredicates(context: List[Constraint], isPathDefined: Boolean, isClassDefined: Boolean): SMTLibScript = {
    val sort: Sort = if (isPathDefined) Path else Variable

    val pathEquivalencePredicate =
      generatePathEquivalencePredicate(
        context.foldRight(Nil: List[PathEquivalence]){
          case (PathEquivalence(p, q), rest) => PathEquivalence(p, q) :: rest
          case (_, rest) => rest
      }, isPathDefined)

    if (isClassDefined) {
      // All constraint types
      SMTLibScript(Seq(
        generateInstantiatedByPredicate(
          context.foldRight(Nil: List[InstantiatedBy]) {
            case (InstantiatedBy(p, cls), rest) => InstantiatedBy(p, cls) :: rest
            case (_, rest) => rest
          }, isPathDefined),
        DeclareFun(functionInstanceOf, Seq(sort, Class), Bool), // TODO: define instance of constraint
        pathEquivalencePredicate
      ))
    } else {
      // Only path equivalence
      SMTLibScript(Seq(pathEquivalencePredicate))
    }
  }

  private def generateInstantiatedByPredicate(contextInstantiations: List[InstantiatedBy], isPathDefined: Boolean): SMTLibCommand = {
    val path: Sort = if (isPathDefined) Path else Variable

    DefineFun(FunctionDef(
      functionInstantiatedBy,
      Seq(
        SortedVar(SimpleSymbol("path-p"), path),
        SortedVar(SimpleSymbol("class-c"), Class)
      ),
      Bool,
      contextInstantiations.foldRight(False: Term) {
        case (InstantiatedBy(p, cls), rest) =>
          Or(
            And(
              Eq(SimpleSymbol("path-p"), PathToTerm(p, isPathDefined)),
              Eq(SimpleSymbol("class-c"), IdToSymbol(cls))
            ),
            rest
          )
      }
    ))
  }

  private def generatePathEquivalencePredicate(contextPathEquivalences: List[PathEquivalence], isPathDefined: Boolean): SMTLibCommand = {
    val path: Sort = if (isPathDefined) Path else Variable

    val symmetricTransitiveClosure: List[Constraint] = buildReflexiveSymmetricTransitiveClosure(contextPathEquivalences.toSet).toList

    val body: Term =
      if (symmetricTransitiveClosure.isEmpty)
        Eq(SimpleSymbol("path-p"), SimpleSymbol("path-q"))
      else
        Or(
          Eq(SimpleSymbol("path-p"), SimpleSymbol("path-q")),
          symmetricTransitiveClosure.foldRight(False: Term) {
            case (PathEquivalence(p, q), rest) =>
              Or(
                And(
                  Eq(SimpleSymbol("path-p"), PathToTerm(p, isPathDefined)),
                  Eq(SimpleSymbol("path-q"), PathToTerm(q, isPathDefined))
                ),
                rest
              )
            case (_, rest) => rest
          }
        )

    DefineFun(FunctionDef(
      functionPathEquivalence,
      Seq(
        SortedVar(SimpleSymbol("path-p"), path),
        SortedVar(SimpleSymbol("path-q"), path)
      ),
      Bool,
      body
    ))
  }

  @tailrec
  private def buildReflexiveSymmetricTransitiveClosure(pathEquivalences: Set[PathEquivalence]): Set[PathEquivalence] = {
    def addTransitiveConstraints(source: PathEquivalence, target: Set[PathEquivalence]): Set[PathEquivalence] = {
      val PathEquivalence(a, b) = source
      target.foldLeft(Set.empty[PathEquivalence]) {
        case (rest, PathEquivalence(`b`, c)) => rest + PathEquivalence(a, c)
        case (rest, _) => rest
      }
    }

    val symmetricClosure = pathEquivalences ++ pathEquivalences.map {
      case PathEquivalence(p, q) => PathEquivalence(q, p)
    }

    var closureCandidate: Set[PathEquivalence] = symmetricClosure
    symmetricClosure.foreach {
      constraint => closureCandidate = closureCandidate union addTransitiveConstraints(constraint, closureCandidate)
    }

    if (closureCandidate == pathEquivalences)
      closureCandidate
    else
      buildReflexiveSymmetricTransitiveClosure(closureCandidate)
  }

//  private def generateFunctionDeclarations(isPathDefined: Boolean, isClassDefined: Boolean): SMTLibScript = {
//    val sort: Sort = if (isPathDefined) Path else Variable
//
//    if (isClassDefined)
//      SMTLibScript(Seq(
//        DeclareFun(functionInstanceOf, Seq(sort, Class), Bool),
//        DeclareFun(functionInstantiatedBy, Seq(sort, Class), Bool),
//        DeclareFun(functionPathEquivalence, Seq(sort, sort), Bool)
//      ))
//    else
//      SMTLibScript(Seq(DeclareFun(functionPathEquivalence, Seq(sort, sort), Bool)))
//  }

  private def generateSubstitutionFunction(isPathDefined: Boolean): SMTLibCommand = {
    val p: SMTLibSymbol = SimpleSymbol("path-p")
    val q: SMTLibSymbol = SimpleSymbol("path-q")
    val x: SMTLibSymbol = SimpleSymbol("var-x")

    if (isPathDefined) {
      DefineFunRec(FunctionDef(
        functionSubstitution,
        Seq(
          SortedVar(p, Path),
          SortedVar(x, Variable),
          SortedVar(q, Path)
        ),
        Path,
        Ite(
          is(constructorVar, p),
          Ite(Eq(x, selector(selectorId, p)), q, p),
          Apply(
            constructorPth,
            Seq(
              Apply(
                functionSubstitution,
                Seq(selector(selectorObj, p), x, q)),
              selector(selectorField, p)))
        )
      ))
    } else {
      DefineFun(FunctionDef(
        functionSubstitution,
        Seq(
          SortedVar(p, Variable),
          SortedVar(x, Variable),
          SortedVar(q, Variable)
        ),
        Variable,
        Ite(Eq(x, p), q, p)
      ))
    }
  }

  private def generateCalculusRules(isPathDefined: Boolean, isClassDefined: Boolean): SMTLibScript = {
    val sort: Sort = if (isPathDefined) Path else Variable

    val p: SMTLibSymbol = SimpleSymbol("path-p")
    val q: SMTLibSymbol = SimpleSymbol("path-q")
    val r: SMTLibSymbol = SimpleSymbol("path-r")
    val s: SMTLibSymbol = SimpleSymbol("path-s")
    val a: SMTLibSymbol = SimpleSymbol("cs-a")
    val c: SMTLibSymbol = SimpleSymbol("class-c")
    val x: SMTLibSymbol = SimpleSymbol("var-x")


    val cReflexivity:SMTLibCommand = Assert(Forall(Seq(SortedVar(p, sort)), Apply(functionPathEquivalence, Seq(p, p))))

    val cClass: SMTLibCommand = Assert(Forall(
      Seq(
        SortedVar(a, Bool),
        SortedVar(p, sort),
        SortedVar(c, Class)
      ),
      Implies(
        Implies(a, Apply(functionInstantiatedBy, Seq(p, c))),
        Implies(a, Apply(functionInstanceOf, Seq(p, c)))
      )
    ))

    val cSubstPathEquivalence: SMTLibCommand = Assert(Forall(
      Seq(
        SortedVar(a, Bool),
        SortedVar(p, sort),
        SortedVar(q, sort),
        SortedVar(r, sort),
        SortedVar(s, sort),
        SortedVar(x, Variable)
      ),
      Implies(
        And(
          Implies(a, Apply(functionPathEquivalence, Seq(
            Apply(functionSubstitution, Seq(p, x, r)),
            Apply(functionSubstitution, Seq(q, x, r))))),
          Implies(a, Apply(functionPathEquivalence, Seq(s, r)))
        ),
        Implies(a, Apply(functionPathEquivalence, Seq(
          Apply(functionSubstitution, Seq(p, x, s)),
          Apply(functionSubstitution, Seq(q, x, s))
        )))
      )
    ))

    val cSubstInstanceOf: SMTLibCommand = Assert(Forall(
      Seq(
        SortedVar(a, Bool),
        SortedVar(p, sort),
        SortedVar(c, Class),
        SortedVar(r, sort),
        SortedVar(s, sort),
        SortedVar(x, Variable)
      ),
      Implies(
        And(
          Implies(a, Apply(functionInstanceOf, Seq(Apply(functionSubstitution, Seq(p, x, r)), c))),
          Implies(a, Apply(functionPathEquivalence, Seq(s, r)))
        ),
        Implies(a, Apply(functionInstanceOf, Seq(Apply(functionSubstitution, Seq(p, x, s)), c)))
      )
    ))

    val cSubstInstantiatedBy: SMTLibCommand = Assert(Forall(
      Seq(
        SortedVar(a, Bool),
        SortedVar(p, sort),
        SortedVar(c, Class),
        SortedVar(r, sort),
        SortedVar(s, sort),
        SortedVar(x, Variable)
      ),
      Implies(
        And(
          Implies(a, Apply(functionInstantiatedBy, Seq(Apply(functionSubstitution, Seq(p, x, r)), c))),
          Implies(a, Apply(functionPathEquivalence, Seq(s, r)))
        ),
        Implies(a, Apply(functionInstantiatedBy, Seq(Apply(functionSubstitution, Seq(p, x, s)), c)))
      )
    ))

    if (isClassDefined)
      SMTLibScript(Seq(
        cReflexivity,
        cClass,
        cSubstPathEquivalence,
        cSubstInstanceOf,
        cSubstInstantiatedBy
      ))
    else
      SMTLibScript(Seq(
        cReflexivity,
        cSubstPathEquivalence
      ))
  }



  private def generateProgRules(constraintEntailments: List[ConstraintEntailment], isPathDefined: Boolean): SMTLibScript = {
    val sort: Sort = if (isPathDefined) Path else Variable
    val path: MetaPath = MetaPath("path-p")
    val b: SMTLibSymbol = SimpleSymbol("cs-a")
    val p: SMTLibSymbol = SimpleSymbol(path.baseName)

    def substituteConstraintToTerm(constraint: Constraint, x: Id): Term = constraint match {
      case PathEquivalence(Id(_), Id(_)) => ConstraintToTerm(substitute(x, path, constraint), isPathDefined)
      case PathEquivalence(p@Id(_), q) => Op(functionPathEquivalence)(PathToTerm(substitute(x, path, p), isPathDefined), PathToTerm(q, isPathDefined))
      case PathEquivalence(p, q@Id(_)) => Op(functionPathEquivalence)(PathToTerm(p, isPathDefined), PathToTerm(substitute(x, path, q), isPathDefined))
      case PathEquivalence(p, q) => Op(functionPathEquivalence)(substitutePath(PathToTerm(p, isPathDefined), IdToSymbol(x), PathToTerm(path, isPathDefined)), substitutePath(PathToTerm(q, isPathDefined), IdToSymbol(x), PathToTerm(path, isPathDefined)))
      case InstanceOf(Id(_), _) => ConstraintToTerm(substitute(x, path, constraint), isPathDefined)
      case InstanceOf(p, cls) => Op(functionInstanceOf)(substitutePath(PathToTerm(p, isPathDefined), IdToSymbol(x), PathToTerm(path, isPathDefined)), IdToSymbol(cls))
      case InstantiatedBy(Id(_), _) => ConstraintToTerm(substitute(x, path, constraint), isPathDefined)
      case InstantiatedBy(p, cls) => Op(functionInstantiatedBy)(substitutePath(PathToTerm(p, isPathDefined), IdToSymbol(x), PathToTerm(path, isPathDefined)), IdToSymbol(cls))
    }

    SMTLibScript(constraintEntailments map {
      case ConstraintEntailment(x, as, InstanceOf(y, cls)) if x==y =>
        Assert(Forall(Seq(
          SortedVar(b, Bool),
          SortedVar(p, sort)
        ),
          Implies(
            Implies(b,
              if (as.size==1)
                substituteConstraintToTerm(as.head, x)
              else
                // TODO: check if conjunction on rhs is correct: /\ (bs => a_i) === bs => /\ a_i ? seems so
                Apply(SimpleSymbol("and"), as map { constraint => substituteConstraintToTerm(constraint, x)})),
            Implies(b, Apply(functionInstanceOf, Seq(p, IdToSymbol(cls))))
          )
        ))
      case _ => Assert(True) // constraint entailment not well-formed
    })
  }
}

object SemanticEntailment {
  // Sort names
  private val sortClass: String = "Class"
  private val sortVariable: String = "Variable"
  private val sortField: String = "Field"
  private val sortPath: String = "Path"

  // Function names
  private val functionPathEquivalence: SMTLibSymbol = SimpleSymbol("path-equivalence")
  private val functionInstanceOf: SMTLibSymbol = SimpleSymbol("instance-of")
  private val functionInstantiatedBy: SMTLibSymbol = SimpleSymbol("instantiated-by")
  private val functionSubstitution: SMTLibSymbol = SimpleSymbol("substitute")

  // Datatype constructor names
  private val constructorVar: SimpleSymbol = SimpleSymbol("var")
  private val constructorPth: SimpleSymbol = SimpleSymbol("pth")

  // Selector names
  private val selectorId: SMTLibSymbol = SimpleSymbol("id")
  private val selectorObj: SMTLibSymbol = SimpleSymbol("obj")
  private val selectorField: SMTLibSymbol = SimpleSymbol("field")

  private case class MetaPath(s: String) extends Path {
    override def toString: String = s
    override def base: Id = Id(Symbol(s))
    override def baseName: String = s
    override def prefixBaseName(prefix: String): Path = MetaPath(prefix+s)
    override def fields: List[Id] = Nil
    override def depth: Int = 0
  }

  // Sorts explicitly added in this translation.
  val Variable: Sort = SimpleSymbol(sortVariable)
  val Field: Sort = SimpleSymbol(sortField)
  val Class: Sort = SimpleSymbol(sortClass)
  val Path: Sort = SimpleSymbol(sortPath)

//  object Variable extends Sort {
//    override def format(): String = sortVariable
//  }
//
//  object Field extends Sort {
//    override def format(): String = sortField
//  }
//
//  object Class extends Sort {
//    override def format(): String = sortClass
//  }
//
//  object Path extends Sort {
//    override def format(): String = sortPath
//  }

  def IdToSymbol(x: Id): SMTLibSymbol = SimpleSymbol(x.toString)

  def PathToTerm(path: Path, isPathDefined: Boolean): Term = path match {
    case x@Id(_) => if (isPathDefined) Op(constructorVar)(IdToSymbol(x)) else IdToSymbol(x)
    case FieldPath(p, f) => Op(constructorPth)(PathToTerm(p, isPathDefined), IdToSymbol(f))
    case MetaPath(p) => SimpleSymbol(p)
  }

  def ConstraintToTerm(constraint: Constraint, isPathDefined: Boolean): Term = constraint match {
    case PathEquivalence(p, q) => Op(functionPathEquivalence)(PathToTerm(p, isPathDefined), PathToTerm(q, isPathDefined))
    case InstanceOf(p, c) => Op(functionInstanceOf)(PathToTerm(p, isPathDefined), IdToSymbol(c))
    case InstantiatedBy(p, c) => Op(functionInstantiatedBy)(PathToTerm(p, isPathDefined), IdToSymbol(c))
  }

  private def substitutePath(p: Term, x: Term, q: Term): Term = Op(functionSubstitution)(p, x, q)
}