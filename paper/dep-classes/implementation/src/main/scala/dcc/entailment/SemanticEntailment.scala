package dcc.entailment

import dcc.Util.substitute
import dcc.entailment.SemanticEntailment.{Class, ConstraintToTerm, Field, IdToSymbol, MetaPath, Path, PathToTerm, Variable, constructorPth, constructorVar, functionInstanceOf, functionInstantiatedBy, functionPathEquivalence, functionSubstitution, selectorField, selectorId, selectorObj, sortClass, sortField, sortPath, sortVariable, substitutePath}
import dcc.syntax.{AbstractMethodDeclaration, Constraint, ConstraintEntailment, ConstructorDeclaration, FieldPath, Id, InstanceOf, InstantiatedBy, MethodImplementation, Path, PathEquivalence}
import dcc.syntax.Program.Program
import dcc.types.Type
import smt.smtlib.SMTLib.{buildEnumerationType, is, selector}
import smt.smtlib.syntax.Sugar.Op
import smt.smtlib.{SMTLibCommand, SMTLibScript}
import smt.smtlib.syntax.{Apply, Assert, ConstructorDatatype, ConstructorDec, DeclareDatatype, DeclareFun, DefineFun, DefineFunRec, Forall, FunctionDef, SMTLibSymbol, SelectorDec, SimpleSymbol, Sort, SortedVar, Term, Unsat}
import smt.smtlib.theory.BoolPredefined._
import smt.solver.Z3Solver

import scala.language.postfixOps

// TODO: add debug flag similar to SMTSolver
class SemanticEntailment(val program: Program) extends Entailment {
  override def entails(context: List[Constraint], constraint: Constraint): Boolean = {
    val (smt, isPathDefined) = axioms(constraint::context)
    val solver = new Z3Solver(smt, debug=true)

    solver.addCommand(Assert(Not(
      Implies(
        if (context.isEmpty)
          True
        else
          Apply(SimpleSymbol("and"), context map {c => ConstraintToTerm(c, isPathDefined)}),
        ConstraintToTerm(constraint, isPathDefined)
    ))))
//    solver.addCommand(CheckSat)

    val response = solver.checkSat()
    response match {
      case Left(Unsat) => true
      case Left(_) => false
      case Right(errors) =>
        errors foreach { e => System.err.println(e.format()) }
        false
    }
//    val (exit, messages) = solver.execute()
//
//    if (exit != 0) {
//      messages foreach System.err.println
//      false
//    } else if (messages.nonEmpty && messages.head == "unsat")
//      true
//    else
//      false
  }

  override def entails(context: List[Constraint], constraints: List[Constraint]): Boolean = constraints.forall(entails(context, _))

  /**
    * SMTLib commands capturing the semantic translation of the constraint system
    * @return SMTLib script representing the semantic translation
    */
  def axioms(constraints: List[Constraint]): (SMTLibScript, Boolean) = {
    // TODO: Change this to be able to determine if the path datatype is defined without the pair return type
    val classes: List[String] = getClasses // TODO: search in constraints for class names? All existing classes should be mentioned in the program,
                                           //       currently produces smt error if we ask for an 'valid' entailment with a class that is not mentioned in the program
    val variables: List[String] = getVariableNames ++ extractVariableNames(constraints) distinct
    val fields: List[String] = getFieldNames ++ extractFieldNames(constraints) distinct

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

  private def getClasses: List[String] = program flatMap {
    case ConstructorDeclaration(cls, _, as) => cls.toString :: extractClasses(as)
    case ConstraintEntailment(_, as, a) => extractClasses(a::as)
    case MethodImplementation(_, _, as, Type(_, bs), _) => extractClasses(as) ++ extractClasses(bs)
    case AbstractMethodDeclaration(_, _, as, Type(_, bs)) => extractClasses(as) ++ extractClasses(bs)
  } distinct

  private def extractClasses(constraints: List[Constraint]): List[String] = constraints map {
    case InstanceOf(_, cls) => cls.toString
    case InstantiatedBy(_, cls) =>cls.toString
    case PathEquivalence(_, _) => ""
  } distinct

  private def getVariableNames: List[String] = program flatMap {
    case ConstraintEntailment(x, as, a) => x.toString :: extractVariableNames(a :: as)
    case _ => Nil
  } distinct

  private def extractVariableNames(constraints: List[Constraint]): List[String] = constraints flatMap {
    case InstanceOf(p, _) => List(p.baseName)
    case InstantiatedBy(p, _) => List(p.baseName)
    case PathEquivalence(p, q) => List(p.baseName, q.baseName)
  } distinct

  private def getFieldNames: List[String] = program flatMap {
    case ConstraintEntailment(_, as, a) => extractFieldNames(a :: as)
    case _ => Nil
  } distinct

  private def extractFieldNames(constraints: List[Constraint]): List[String] = constraints flatMap {
    case InstanceOf(p, _) => p.fieldNames
    case InstantiatedBy(p, _) => p.fieldNames
    case PathEquivalence(p, q) => p.fieldNames ++ q.fieldNames
  } distinct

  private def generateEnumerationTypes(classes: List[String], variables: List[String], fields: List[String]): SMTLibScript = {
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

    if (isClassDefined)
      SMTLibScript(Seq(
        DeclareFun(functionInstanceOf, Seq(sort, Class), Bool),
        DeclareFun(functionInstantiatedBy, Seq(sort, Class), Bool),
        DeclareFun(functionPathEquivalence, Seq(sort, sort), Bool)
      ))
    else
      SMTLibScript(Seq(DeclareFun(functionPathEquivalence, Seq(sort, sort), Bool)))
  }

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
      case ConstraintEntailment(x, as, InstanceOf(y, c)) if x==y =>
        Assert(Forall(Seq(
          SortedVar(b, Bool),
          SortedVar(p, sort)
        ),
          Implies(
            Implies(b,
              if (as.size==1)
                substituteConstraintToTerm(as.head, x)
              else
                // TODO: check if conjunction on rhs is correct: /\ (bs => a_i) === bs => /\ a_i ?
                Apply(SimpleSymbol("and"), as map { constraint => substituteConstraintToTerm(constraint, x)})),
            Implies(b, Apply(functionInstanceOf, Seq(p, IdToSymbol(c))))
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
    override def baseName: String = s
    override def fieldNames: List[String] = Nil
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