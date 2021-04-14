package dcc.entailment

import org.scalatest.funsuite.AnyFunSuite
import dcc.program.{Empty, NaturalNumbers}
import dcc.syntax.{FieldPath, InstanceOf, PathEquivalence}
import dcc.syntax.Implicit.StringToId
import smt.smtlib.syntax.{Apply, Assert, ConstructorDatatype, ConstructorDec, DeclareDatatype, DeclareFun, DefineFun, DefineFunRec, Forall, FunctionDef, SelectorDec, SortedVar}
import smt.smtlib.syntax.Implicit.stringToSimpleSymbol
import smt.smtlib.theory.BoolPredefined.{And, Eq, Implies, Ite}

class TestSemanticEntailmentAxiomGeneration extends AnyFunSuite{
  private val Variable: String = "Variable"
  private val Field: String = "Field"
  private val Path: String = "Path"
  private val Class: String = "Class"

  private val path_equivalence: String = "path-equivalence"
  private val instance_of: String = "instance-of"
  private val instantiated_by: String = "instantiated-by"

  private val substitute: String = "substitute"

  test("empty program, no constraints") {
    val entailment: SemanticEntailment = new SemanticEntailment(Empty.program)
    val (axioms, pathDatatypeExists) = entailment.axioms(Nil)

    assert(!pathDatatypeExists)
    assert(axioms.commands.size == 5)
    assert(axioms.commands.contains(
      DeclareDatatype(Variable, ConstructorDatatype(Seq()))))
    assert(axioms.commands.contains(
      DeclareFun(path_equivalence, Seq(Variable, Variable), "Bool")))
    assert(axioms.commands.contains(
      DefineFun(FunctionDef(
        "substitute",
        Seq(SortedVar("path-p", Variable), SortedVar("var-x", Variable), SortedVar("path-q", Variable)), Variable,
        Ite(Eq("var-x", "path-p"), "path-q", "path-p")
      ))
    ))
    assert(axioms.commands.contains(
      Assert(Forall(Seq(SortedVar("path-p", Variable)),
        Apply(path_equivalence, Seq("path-p", "path-p"))
      ))
    ))
    assert(axioms.commands.contains(
      Assert(Forall(
        Seq(SortedVar("cs-a", "Bool"), SortedVar("path-p", Variable), SortedVar("path-q", Variable), SortedVar("path-r", Variable), SortedVar("path-s", Variable), SortedVar("var-x", Variable)),
        Implies(
          And(
            Implies("cs-a", Apply(path_equivalence, Seq(Apply(substitute, Seq("path-p", "var-x", "path-r")), Apply(substitute, Seq("path-q", "var-x", "path-r"))))),
            Implies("cs-a", Apply(path_equivalence, Seq("path-s", "path-r")))
          ),
          Implies("cs-a", Apply(path_equivalence, Seq(
            Apply(substitute, Seq("path-p", "var-x", "path-s")),
            Apply(substitute, Seq("path-q", "var-x", "path-s"))
          )))
        )
      ))
    ))
  }

  test("empty program, path constraints") {
    val entailment: SemanticEntailment = new SemanticEntailment(Empty.program)
    val (axioms, pathDatatypeExists) = entailment.axioms(List(PathEquivalence("X", FieldPath("Y", "F"))))

    assert(pathDatatypeExists)
    assert(axioms.commands.size == 7)

    // Data types are generated correctly
    assert(axioms.commands.contains(
      DeclareDatatype(Variable, ConstructorDatatype(Seq(ConstructorDec("X", Seq()), ConstructorDec("Y", Seq()))))))
    assert(axioms.commands.contains(
      DeclareDatatype(Field, ConstructorDatatype(Seq(ConstructorDec("F", Seq()))))))
    assert(axioms.commands.contains(
      DeclareDatatype(Path, ConstructorDatatype(Seq(
        ConstructorDec("var", Seq(SelectorDec("id", Variable))),
        ConstructorDec("pth", Seq(SelectorDec("obj", Path), SelectorDec("field", Field))))))))

    // Functions are generated correctly
    assert(axioms.commands.contains(
      DeclareFun(path_equivalence, Seq(Path, Path), "Bool")))
    assert(axioms.commands.contains(
      DefineFunRec(FunctionDef(
        "substitute",
        Seq(SortedVar("path-p", Path), SortedVar("var-x", Variable), SortedVar("path-q", Path)), Path,
        Ite(
          Apply("is-var", Seq("path-p")),
          Ite(
            Eq("var-x", Apply("id", Seq("path-p"))),
            "path-q",
            "path-p"),
          Apply("pth", Seq(
            Apply(substitute, Seq(Apply("obj", Seq("path-p")), "var-x", "path-q")),
            Apply("field", Seq("path-p"))
          ))
        )
      ))
    ))

    // Quantifiers are generated correctly
    assert(axioms.commands.contains( // Path Reflexivity
      Assert(Forall(Seq(SortedVar("path-p", Path)),
        Apply(path_equivalence, Seq("path-p", "path-p"))
      ))
    ))
    assert(axioms.commands.contains( // Substitution for path equivalence
      Assert(Forall(
        Seq(SortedVar("cs-a", "Bool"), SortedVar("path-p", Path), SortedVar("path-q", Path), SortedVar("path-r", Path), SortedVar("path-s", Path), SortedVar("var-x", Variable)),
        Implies(
          And(
            Implies("cs-a", Apply(path_equivalence, Seq(Apply(substitute, Seq("path-p", "var-x", "path-r")), Apply(substitute, Seq("path-q", "var-x", "path-r"))))),
            Implies("cs-a", Apply(path_equivalence, Seq("path-s", "path-r")))
          ),
          Implies("cs-a", Apply(path_equivalence, Seq(
            Apply(substitute, Seq("path-p", "var-x", "path-s")),
            Apply(substitute, Seq("path-q", "var-x", "path-s"))
          )))
        )
      ))
    ))
  }

  test("natural numbers program") {
    val entailment: SemanticEntailment = new SemanticEntailment(NaturalNumbers.program)
    val (axioms, pathDatatypeExists) = entailment.axioms(List(InstanceOf(FieldPath("y", "p"), "Nat")))

    assert(pathDatatypeExists)
    assert(axioms.commands.size == 15)

    // Data types are generated correctly
    assert(axioms.commands.contains(
      DeclareDatatype(Variable, ConstructorDatatype(Seq(ConstructorDec("x", Seq()), ConstructorDec("y", Seq()))))))
    assert(axioms.commands.contains(
      DeclareDatatype(Field, ConstructorDatatype(Seq(ConstructorDec("p", Seq()))))))
    assert(axioms.commands.contains(
      DeclareDatatype(Class, ConstructorDatatype(Seq(ConstructorDec("Zero", Seq()), ConstructorDec("Nat", Seq()), ConstructorDec("Succ", Seq()))))))
    assert(axioms.commands.contains(
      DeclareDatatype(Path, ConstructorDatatype(Seq(
        ConstructorDec("var", Seq(SelectorDec("id", Variable))),
        ConstructorDec("pth", Seq(SelectorDec("obj", Path), SelectorDec("field", Field))))))))

    // Functions are generated correctly
    assert(axioms.commands.contains(
      DeclareFun(path_equivalence, Seq(Path, Path), "Bool")))
    assert(axioms.commands.contains(
      DeclareFun(instance_of, Seq(Path, Class), "Bool")))
    assert(axioms.commands.contains(
      DeclareFun(instantiated_by, Seq(Path, Class), "Bool")))
    assert(axioms.commands.contains(
      DefineFunRec(FunctionDef(
        "substitute",
        Seq(SortedVar("path-p", Path), SortedVar("var-x", Variable), SortedVar("path-q", Path)), Path,
        Ite(
          Apply("is-var", Seq("path-p")),
          Ite(
            Eq("var-x", Apply("id", Seq("path-p"))),
            "path-q",
            "path-p"),
          Apply("pth", Seq(
            Apply(substitute, Seq(Apply("obj", Seq("path-p")), "var-x", "path-q")),
            Apply("field", Seq("path-p"))
          ))
        )
      ))
    ))

    // Quantifiers are generated correctly
    assert(axioms.commands.contains( // Path Reflexivity
      Assert(Forall(Seq(SortedVar("path-p", Path)),
        Apply(path_equivalence, Seq("path-p", "path-p"))
      ))
    ))
    assert(axioms.commands.contains( // Class instances
      Assert(Forall(Seq(SortedVar("cs-a", "Bool"), SortedVar("path-p", Path), SortedVar("class-c", Class)),
        Implies(
          Implies("cs-a", Apply(instantiated_by, Seq("path-p", "class-c"))),
          Implies("cs-a", Apply(instance_of, Seq("path-p", "class-c")))
        )
      ))
    ))
    assert(axioms.commands.contains( // Substitution for path equivalence
      Assert(Forall(
        Seq(SortedVar("cs-a", "Bool"), SortedVar("path-p", Path), SortedVar("path-q", Path), SortedVar("path-r", Path), SortedVar("path-s", Path), SortedVar("var-x", Variable)),
        Implies(
          And(
            Implies("cs-a", Apply(path_equivalence, Seq(Apply(substitute, Seq("path-p", "var-x", "path-r")), Apply(substitute, Seq("path-q", "var-x", "path-r"))))),
            Implies("cs-a", Apply(path_equivalence, Seq("path-s", "path-r")))
          ),
          Implies("cs-a", Apply(path_equivalence, Seq(
            Apply(substitute, Seq("path-p", "var-x", "path-s")),
            Apply(substitute, Seq("path-q", "var-x", "path-s"))
          )))
        )
      ))
    ))
    assert(axioms.commands.contains( // Substitution for instance of
      Assert(Forall(
        Seq(SortedVar("cs-a", "Bool"), SortedVar("path-p", Path), SortedVar("class-c", Class), SortedVar("path-r", Path), SortedVar("path-s", Path), SortedVar("var-x", Variable)),
        Implies(
          And(
            Implies("cs-a", Apply(instance_of, Seq(Apply(substitute, Seq("path-p", "var-x", "path-r")), "class-c"))),
            Implies("cs-a", Apply(path_equivalence, Seq("path-s", "path-r")))
          ),
          Implies("cs-a", Apply(instance_of, Seq(
            Apply(substitute, Seq("path-p", "var-x", "path-s")),
            "class-c"
          )))
        )
      ))
    ))
    assert(axioms.commands.contains( // Substitution for instantiated by
      Assert(Forall(
        Seq(SortedVar("cs-a", "Bool"), SortedVar("path-p", Path), SortedVar("class-c", Class), SortedVar("path-r", Path), SortedVar("path-s", Path), SortedVar("var-x", Variable)),
        Implies(
          And(
            Implies("cs-a", Apply(instantiated_by, Seq(Apply(substitute, Seq("path-p", "var-x", "path-r")), "class-c"))),
            Implies("cs-a", Apply(path_equivalence, Seq("path-s", "path-r")))
          ),
          Implies("cs-a", Apply(instantiated_by, Seq(
            Apply(substitute, Seq("path-p", "var-x", "path-s")),
            "class-c"
          )))
        )
      ))
    ))
    assert(axioms.commands.contains(
      Assert(Forall( // program entailment Zero => Nat
        Seq(SortedVar("cs-a", "Bool"), SortedVar("path-p", Path)),
        Implies(
          Implies("cs-a", Apply(instance_of, Seq("path-p", "Zero"))),
          Implies("cs-a", Apply(instance_of, Seq("path-p", "Nat")))
        )
      ))
    ))
    assert(axioms.commands.contains(
      Assert(Forall( // program entailment Succ => Nat
        Seq(SortedVar("cs-a", "Bool"), SortedVar("path-p", Path)),
        Implies(
          Implies("cs-a", And(
            Apply(instance_of, Seq("path-p", "Succ")),
            Apply(instance_of, Seq(Apply(substitute, Seq(Apply("pth", Seq(Apply("var", Seq("x")), "p")), "x", "path-p")), "Nat"))
          )),
          Implies("cs-a", Apply(instance_of, Seq("path-p", "Nat")))
        )
      ))
    ))
  }
}
