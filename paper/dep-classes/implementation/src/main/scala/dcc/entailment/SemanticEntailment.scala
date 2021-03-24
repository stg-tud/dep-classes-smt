package dcc.entailment

import dcc.entailment.SemanticEntailment.{Class, Field, Path, Variable}
import dcc.syntax.Constraint
import dcc.syntax.Program.Program
import smt.smtlib.SMTLib.{buildEnumerationType, is, selector}
import smt.smtlib.{SMTLibCommand, SMTLibScript}
import smt.smtlib.syntax.{And, Apply, Assert, Bool, ConstructorDatatype, ConstructorDec, DeclareDatatype, DeclareFun, DefineFunRec, Eq, Forall, FunctionDef, Implies, Ite, SMTLibSymbol, SelectorDec, SimpleSymbol, Sort, SortedVar}

class SemanticEntailment(val program: Program) {
  private val functionPathEquivalence: SMTLibSymbol = SimpleSymbol("path-equivalence")
  private val functionInstanceOf: SMTLibSymbol = SimpleSymbol("instance-of")
  private val functionInstantiatedBy: SMTLibSymbol = SimpleSymbol("instantiated-by")
  private val functionSubstitution: SMTLibSymbol = SimpleSymbol("substitute")

  private val constructorVar: SimpleSymbol = SimpleSymbol("var")
  private val constructorPth: SimpleSymbol = SimpleSymbol("pth")

  private val selectorId: SMTLibSymbol = SimpleSymbol("id")
  private val selectorObj: SMTLibSymbol = SimpleSymbol("obj")
  private val selectorField: SMTLibSymbol = SimpleSymbol("field")
  
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

    generateEnumerationTypes(classes, variables, fields) ++
      staticDatatypeDeclarations ++
      staticFunctionDeclarations ++
      staticFunctionDefinitions ++
      staticCalculusRules
  }

  private def generateEnumerationTypes(classes: List[String], variables: List[String], fields: List[String]): SMTLibScript =
    SMTLibScript(Seq(
      buildEnumerationType("Class", classes),
      buildEnumerationType("Variable", variables),
      buildEnumerationType("Field", fields)
    ))
}

object SemanticEntailment {
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
}