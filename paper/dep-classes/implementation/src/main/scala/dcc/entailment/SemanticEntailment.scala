package dcc.entailment

import dcc.Util.substitute
import dcc.entailment.SemanticEntailment.{Class, ConstraintToTerm, Field, IdToSymbol, Path, Variable, constructorPth, constructorVar, functionInstanceOf, functionInstantiatedBy, functionPathEquivalence, functionSubstitution, selectorField, selectorId, selectorObj}
import dcc.syntax.{Constraint, ConstraintEntailment, FieldPath, Id, InstanceOf, InstantiatedBy, Path, PathEquivalence}
import dcc.syntax.Program.Program
import smt.smtlib.SMTLib.{buildEnumerationType, is, selector}
import smt.smtlib.syntax.Primitives.True
import smt.smtlib.{SMTLibCommand, SMTLibScript}
import smt.smtlib.syntax.{And, Apply, Assert, Bool, ConstructorDatatype, ConstructorDec, DeclareDatatype, DeclareFun, DefineFunRec, Eq, Forall, FunctionDef, Implies, Ite, Op2, SMTLibSymbol, SelectorDec, SimpleSymbol, Sort, SortedVar, Term}

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
    val p: SMTLibSymbol = SimpleSymbol("p")
    val q: SMTLibSymbol = SimpleSymbol("q")
    val x: SMTLibSymbol = SimpleSymbol("x")

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
    val p: SMTLibSymbol = SimpleSymbol("p")
    val q: SMTLibSymbol = SimpleSymbol("q")
    val r: SMTLibSymbol = SimpleSymbol("r")
    val s: SMTLibSymbol = SimpleSymbol("s")
    val a: SMTLibSymbol = SimpleSymbol("a")
    val c: SMTLibSymbol = SimpleSymbol("c")
    val x: SMTLibSymbol = SimpleSymbol("x")


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
    axioms(c::context)
    true
  }

  /**
    * SMTLib commands capturing the semantic translation of the constraint system
    * @return SMTLib script representing the semantic translation
    */
  def axioms(constraints: List[Constraint]): SMTLibScript = {
    // TODO: traverse program for classes, constraints for vars and fields
    val classes: List[String] = List("Nat", "Succ", "Zero")
    val variables: List[String] = List("x", "y", "z")
    val fields: List[String] = List("f", "g", "h")

    val constraintEntailments: List[ConstraintEntailment] = program.filter(_.isInstanceOf[ConstraintEntailment]).map(_.asInstanceOf[ConstraintEntailment])

    generateEnumerationTypes(classes, variables, fields) ++
      staticDatatypeDeclarations ++
      staticFunctionDeclarations ++
      staticFunctionDefinitions ++
      staticCalculusRules ++
      generateProgRules(constraintEntailments)
  }

  private def generateEnumerationTypes(classes: List[String], variables: List[String], fields: List[String]): SMTLibScript =
    SMTLibScript(Seq(
      buildEnumerationType("Class", classes),
      buildEnumerationType("Variable", variables),
      buildEnumerationType("Field", fields)
    ))

  private def generateProgRules(constraintEntailments: List[ConstraintEntailment]): SMTLibScript = {
    val path: String = "p"
    val b: SMTLibSymbol = SimpleSymbol("b")
    val p: SMTLibSymbol = SimpleSymbol(path)

    SMTLibScript(constraintEntailments map {
      case ConstraintEntailment(x, as, InstanceOf(y, c)) if x==y =>
        Assert(Forall(Seq(
          SortedVar(b, Bool),
          SortedVar(p, Path)
        ),
          Implies(
            Implies(b,
              if (as.size==1)
                ConstraintToTerm(substitute(x, Id(Symbol(path)), as.head))
              else
                Apply(SimpleSymbol("and"), as map { constraint => ConstraintToTerm(substitute(x, Id(Symbol(path)), constraint)) })), // TODO: check if conjunction on rhs is correct: /\ (bs => a_i) === bs => /\ a_i ?
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
    case x@Id(_) => IdToSymbol(x)
    case FieldPath(p, f) => Op2(constructorPth, PathToTerm(p), IdToSymbol(f))
  }

  def ConstraintToTerm(constraint: Constraint): Term = constraint match {
    case PathEquivalence(p, q) => Op2(functionPathEquivalence, PathToTerm(p), PathToTerm(q))
    case InstanceOf(p, c) => Op2(functionInstanceOf, PathToTerm(p), IdToSymbol(c))
    case InstantiatedBy(p, c) => Op2(functionInstantiatedBy, PathToTerm(p), IdToSymbol(c))
  }
}