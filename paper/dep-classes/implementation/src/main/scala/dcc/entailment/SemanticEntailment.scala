package dcc.entailment

import dcc.Util.substitute
import dcc.entailment.SemanticEntailment.{Class, ConstraintToTerm, Field, IdToSymbol, MetaPath, Path, PathToTerm, Variable, constructorPth, constructorVar, functionInstanceOf, functionInstantiatedBy, functionPathEquivalence, functionSubstitution, selectorField, selectorId, selectorObj, substitutePath}
import dcc.syntax.{AbstractMethodDeclaration, Constraint, ConstraintEntailment, ConstructorDeclaration, FieldPath, Id, InstanceOf, InstantiatedBy, MethodImplementation, Path, PathEquivalence, Type}
import dcc.syntax.Program.Program
import smt.smtlib.SMTLib.{buildEnumerationType, is, selector}
import smt.smtlib.syntax.Primitives.True
import smt.smtlib.{SMTLibCommand, SMTLibScript}
import smt.smtlib.syntax.{And, Apply, Assert, Bool, CheckSat, ConstructorDatatype, ConstructorDec, DeclareDatatype, DeclareFun, DefineFunRec, Eq, Forall, FunctionDef, Implies, Ite, Not, Op1, Op2, Op3, SMTLibSymbol, SelectorDec, SimpleSymbol, Sort, SortedVar, Term}
import smt.solver.Z3Solver

import scala.language.postfixOps

class SemanticEntailment(val program: Program) {
  private val staticDatatypeDeclarations: SMTLibScript = SMTLibScript(Seq(
    DeclareDatatype(SimpleSymbol("Path"), ConstructorDatatype(Seq(
      ConstructorDec(constructorVar, Seq(SelectorDec(selectorId, Variable))),
      ConstructorDec(constructorPth, Seq(SelectorDec(selectorObj, Path), SelectorDec(selectorField, Field)))
    )))
  ))

  private val staticFunctionDeclarations: SMTLibScript = SMTLibScript(Seq(
    DeclareFun(functionInstanceOf, Seq(Path, Class), Bool),
    DeclareFun(functionInstantiatedBy, Seq(Path, Class), Bool),
    DeclareFun(functionPathEquivalence, Seq(Path, Path), Bool)
  ))

  private val staticFunctionDefinitions: SMTLibScript = {
    val p: SMTLibSymbol = SimpleSymbol("path-p")
    val q: SMTLibSymbol = SimpleSymbol("path-q")
    val x: SMTLibSymbol = SimpleSymbol("var-x")

    SMTLibScript(Seq(
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
    ))
  }

  private val staticCalculusRules: SMTLibScript = {
    val p: SMTLibSymbol = SimpleSymbol("path-p")
    val q: SMTLibSymbol = SimpleSymbol("path-q")
    val r: SMTLibSymbol = SimpleSymbol("path-r")
    val s: SMTLibSymbol = SimpleSymbol("path-s")
    val a: SMTLibSymbol = SimpleSymbol("cs-a")
    val c: SMTLibSymbol = SimpleSymbol("class-c")
    val x: SMTLibSymbol = SimpleSymbol("var-x")


    val cRefl:SMTLibCommand = Assert(Forall(Seq(SortedVar(p, Path)), Apply(functionPathEquivalence, Seq(p, p))))

    val cClass: SMTLibCommand = Assert(Forall(
      Seq(
        SortedVar(a, Bool),
        SortedVar(p, Path),
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
        SortedVar(p, Path),
        SortedVar(q, Path),
        SortedVar(r, Path),
        SortedVar(s, Path),
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
        SortedVar(p, Path),
        SortedVar(c, Class),
        SortedVar(r, Path),
        SortedVar(s, Path),
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
        SortedVar(p, Path),
        SortedVar(c, Class),
        SortedVar(r, Path),
        SortedVar(s, Path),
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

    SMTLibScript(Seq(
      cRefl,
      cClass,
      cSubstPathEquivalence,
      cSubstInstanceOf,
      cSubstInstantiatedBy
    ))
  }

  def entails(context: List[Constraint], c: Constraint): Boolean = {
    val smt = axioms(c::context)
    val solver = new Z3Solver(smt, debug=true)

    solver.addCommand(Assert(Not(Implies(
      Apply(SimpleSymbol("and"), context map ConstraintToTerm),
      ConstraintToTerm(c)
    ))))
    solver.addCommand(CheckSat)

    val (exit, messages) = solver.execute()

    if (exit != 0) {
      messages foreach System.err.println
      false
    } else if (messages.nonEmpty && messages.head == "unsat")
      true
    else
      false
  }

  /**
    * SMTLib commands capturing the semantic translation of the constraint system
    * @return SMTLib script representing the semantic translation
    */
  def axioms(constraints: List[Constraint]): SMTLibScript = {
    // TODO: what to do if there are no classes, variables or fields?
    //  will result in an z3 error: invalid datatype declaration, datatype does not have any constructors
    val classes: List[String] = getClasses
    val variables: List[String] = extractVariableNames(constraints) // TODO: add variables/fields from program declaration
    val fields: List[String] = extractFieldNames(constraints)

    val constraintEntailments: List[ConstraintEntailment] = program.filter(_.isInstanceOf[ConstraintEntailment]).map(_.asInstanceOf[ConstraintEntailment])

    generateEnumerationTypes(classes, variables, fields) ++
      staticDatatypeDeclarations ++
      staticFunctionDeclarations ++
      staticFunctionDefinitions ++
      staticCalculusRules ++
      generateProgRules(constraintEntailments)
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

  private def extractVariableNames(constraints: List[Constraint]): List[String] = constraints flatMap {
    case InstanceOf(p, _) => List(p.baseName)
    case InstantiatedBy(p, _) => List(p.baseName)
    case PathEquivalence(p, q) => p.baseName :: q.baseName :: Nil
  } distinct

  private def extractFieldNames(constraints: List[Constraint]): List[String] = constraints flatMap {
    case InstanceOf(p, _) => p.fieldNames
    case InstantiatedBy(p, _) => p.fieldNames
    case PathEquivalence(p, q) => p.fieldNames ++ q.fieldNames
  } distinct

//  private def extractPathNames(constraints: List[Constraint]): List[String] = constraints flatMap {
//    case InstanceOf(p, _) => List(p.toString)
//    case InstantiatedBy(p, _) => List(p.toString)
//    case PathEquivalence(p, q) => p.toString :: q.toString :: Nil
//  } distinct

  private def generateEnumerationTypes(classes: List[String], variables: List[String], fields: List[String]): SMTLibScript =
    SMTLibScript(Seq(
      buildEnumerationType("Class", classes),
      buildEnumerationType("Variable", variables),
      buildEnumerationType("Field", fields)
    ))

  private def generateProgRules(constraintEntailments: List[ConstraintEntailment]): SMTLibScript = {
    val path: MetaPath = MetaPath("path-p")
    val b: SMTLibSymbol = SimpleSymbol("cs-a")
    val p: SMTLibSymbol = SimpleSymbol(path.baseName)

    def substituteConstraintToTerm(constraint: Constraint, x: Id): Term = constraint match {
      case PathEquivalence(Id(_), Id(_)) => ConstraintToTerm(substitute(x, path, constraint))
      case PathEquivalence(p@Id(_), q) => Op2(functionPathEquivalence, PathToTerm(substitute(x, path, p)), PathToTerm(q))
      case PathEquivalence(p, q@Id(_)) => Op2(functionPathEquivalence, PathToTerm(p), PathToTerm(substitute(x, path, q)))
      case PathEquivalence(p, q) => Op2(functionPathEquivalence, substitutePath(PathToTerm(p), IdToSymbol(x), PathToTerm(path)), substitutePath(PathToTerm(q), IdToSymbol(x), PathToTerm(path)))
      case InstanceOf(Id(_), _) => ConstraintToTerm(substitute(x, path, constraint))
      case InstanceOf(p, cls) => Op2(functionInstanceOf, substitutePath(PathToTerm(p), IdToSymbol(x), PathToTerm(path)), IdToSymbol(cls))
      case InstantiatedBy(Id(_), _) => ConstraintToTerm(substitute(x, path, constraint))
      case InstantiatedBy(p, cls) => Op2(functionInstantiatedBy, substitutePath(PathToTerm(p), IdToSymbol(x), PathToTerm(path)), IdToSymbol(cls))
    }

    SMTLibScript(constraintEntailments map {
      case ConstraintEntailment(x, as, InstanceOf(y, c)) if x==y =>
        Assert(Forall(Seq(
          SortedVar(b, Bool),
          SortedVar(p, Path)
        ),
          Implies(
            Implies(b,
              if (as.size==1)
                //ConstraintToTerm(substitute(x, path, as.head))
                //Op3(functionSubstitution, IdToSymbol(x), PathToTerm(path), ConstraintToTerm(as.head))
                substituteConstraintToTerm(as.head, x)
              else
                //Apply(SimpleSymbol("and"), as map { constraint => ConstraintToTerm(substitute(x, path, constraint)) })), // TODO: check if conjunction on rhs is correct: /\ (bs => a_i) === bs => /\ a_i ?
                //Apply(SimpleSymbol("and"), as map { constraint => Op3(functionSubstitution, IdToSymbol(x), PathToTerm(path), ConstraintToTerm(constraint)) })),
                Apply(SimpleSymbol("and"), as map { constraint => substituteConstraintToTerm(constraint, x)})),
            Implies(b, Apply(functionInstanceOf, Seq(p, IdToSymbol(c))))
          )
        ))
      case _ => Assert(True) // constraint entailment not well-formed
    })
  }
}

object SemanticEntailment {
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

  def IdToSymbol(x: Id): SMTLibSymbol = SimpleSymbol(x.toString)

  def PathToTerm(path: Path): Term = path match {
    case x@Id(_) => Op1(constructorVar, IdToSymbol(x))
    case FieldPath(p, f) => Op2(constructorPth, PathToTerm(p), IdToSymbol(f))
    case MetaPath(p) => SimpleSymbol(p)
  }

  def ConstraintToTerm(constraint: Constraint): Term = constraint match {
    case PathEquivalence(p, q) => Op2(functionPathEquivalence, PathToTerm(p), PathToTerm(q))
    case InstanceOf(p, c) => Op2(functionInstanceOf, PathToTerm(p), IdToSymbol(c))
    case InstantiatedBy(p, c) => Op2(functionInstantiatedBy, PathToTerm(p), IdToSymbol(c))
  }

  private def substitutePath(p: Term, x: Term, q: Term): Term = Op3(functionSubstitution, p, x, q)
}