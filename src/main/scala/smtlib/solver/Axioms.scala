package smtlib.solver

import smtlib.SMTLibScript
import smtlib.syntax._
import smtlib.syntax.Implicit._

object Axioms {
  private val Constraints = Sorts("List", Seq("Constraint"))

  /**
    * Path datatype declaration
    * Constrcutors:
    *   var String
    *   pth Path String
    */
  val pathDatatype = DeclareDatatype("Path", ConstructorDatatype(Seq(
    ConstructorDec("var", Seq(SelectorDec("id", "String"))),
    ConstructorDec("pth", Seq(
      SelectorDec("obj", "Path"),
      SelectorDec("field", "String")
    ))
  )))

  /**
    * Constraint datatype declaration
    * Constructors:
    *   path-eq Path Path
    *   instance-of Path String
    *   instantiated-by Path String
    */
  val constraintDatatype = DeclareDatatype(SimpleSymbol("Constraint"), ConstructorDatatype(Seq(
    ConstructorDec(SimpleSymbol("path-eq"), Seq(
      SelectorDec(SimpleSymbol("p-left"), SimpleSymbol("Path")),
      SelectorDec(SimpleSymbol("p-right"), SimpleSymbol("Path")))),
    ConstructorDec(SimpleSymbol("instance-of"), Seq(
      SelectorDec(SimpleSymbol("instance"), SimpleSymbol("Path")),
      SelectorDec(SimpleSymbol("cls"), SimpleSymbol("String")))),
    ConstructorDec(SimpleSymbol("instantiated-by"), Seq(
      SelectorDec(SimpleSymbol("object"), SimpleSymbol("Path")), // TODO: find better selecor names for this
      SelectorDec(SimpleSymbol("clsname"), SimpleSymbol("String"))))
  )))

  /** List concatenation */
  val concat = DefineFunRec(
                FunctionDef(
                  "conc",
                  Seq(
                    SortedVar("l1", Constraints),
                    SortedVar("l2", Constraints)
                  ),
                  Constraints,
                  Match("l1",
                    Seq(
                      MatchCase(Pattern("nil", Seq()),
                        "l2"),
                      MatchCase(Pattern("insert", Seq("hd", "tl")),
                        Apply("insert", Seq(
                          "hd",
                          Apply("conc", Seq("tl", "l2"))
                        )))
                    ))
                ))

  /** String is a class name  */
  val classProp = DeclareFun("class", Seq("String"), Bool)

  /** String is a variable name  */
  val varProp = DeclareFun("variable", Seq("String"), Bool)

  /**
    * Substitution function for paths.
    * param p1 Base path for the substitution
    * param id Variable name to be substituted
    * param p2 Path to be substituted
    * return `id` substituted with `p2` in `p1`
    */
  val substPath = DefineFunRec(
                    FunctionDef(
                      "subst-path",
                      Seq(
                        SortedVar("p1", "Path"),
                        SortedVar("id", "String"),
                        SortedVar("p2", "Path")
                      ),
                      "Path",
                      Match("p1",
                        Seq(
                          MatchCase(Pattern("var", Seq("x")),
                            Ite(Eq("id", "x"), "p2", "p1")),
                          MatchCase(Pattern("pth", Seq("p", "f")),
                            Apply("pth", Seq(Apply("subst-path", Seq("p", "id", "p2")), "f"))
                          )
                        ))
                    ))

  /**
    * Substitution function for constraints.
    * param c Base constraint for the substitution
    * param id Variable name to be substituted
    * param p Path to be substituted
    * return `id` substituted with `p` in `c`
    */
  val substConstraint = DefineFun(
                          FunctionDef(
                            "subst-constraint",
                            Seq(
                              SortedVar("c", "Constraint"),
                              SortedVar("id", "String"),
                              SortedVar("p", "Path")
                            ),
                            "Constraint",
                            Match("c",
                              Seq(
                                MatchCase(Pattern("path-eq", Seq("p1", "p2")),
                                  Apply("path-eq", Seq(
                                    Apply("subst-path", Seq("p1", "id", "p")),
                                    Apply("subst-path", Seq("p2", "id", "p"))))),
                                MatchCase(Pattern("instance-of", Seq("p1", "cls1")),
                                  Apply("instance-of", Seq(
                                    Apply("subst-path", Seq("p1", "id", "p")),
                                    "cls1"
                                  ))),
                                MatchCase(Pattern("instantiated-by", Seq("p1", "cls1")),
                                  Apply("instantiated-by", Seq(
                                    Apply("subst-path", Seq("p1", "id", "p")),
                                    "cls1"
                                  )))
                              ))
                          ))

  /**
    * Substitution proposition
    * param c1 Base constraint for the substitution
    * param x Variable name to be substituted
    * param p Path to be substituted
    * param c2 Constraint to be the result of the substitution
    * return true if `x` substituted with `p` in `c1` equals `c2`,
    *        false otherwise
    */
  val substProp = DefineFun(
                    FunctionDef(
                      "subst",
                      Seq(
                        SortedVar("c1", "Constraint"),
                        SortedVar("x", "String"),
                        SortedVar("p", "Path"),
                        SortedVar("c2", "Constraint")
                      ),
                      Bool,
                      Eq(
                        Apply("subst-constraint", Seq("c1", "x", "p")),
                        "c2"
                      )
                    ))

  // DCC

  /**
    * dcc's sequent calculus judgement
    * entails :: List Constraint -> Constraint -> Bool
    */
  val entails = DeclareFun("entails", Seq(Constraints, "Constraint"), Bool)

  // C-Ident
  private val identTerm = Forall(Seq(SortedVar("c", "Constraint")),
                            Apply("entails", Seq(
                              Apply("insert", Seq("c", "nil")),
                              "c"
                            )))
  val cIdent = Assert(identTerm)

  // C-Refl
  private val reflTerm = Forall(Seq(SortedVar("p", "Path")),
                            Apply("entails", Seq(
                              "nil",
                              Apply("path-eq", Seq("p", "p"))
                            )))
  val cRefl = Assert(reflTerm)

  // C-Class
  private val classTerm = Forall(
                            Seq(
                              SortedVar("cs", Constraints),
                              SortedVar("p", "Path"),
                              SortedVar("c", "String")),
                            Implies(
                              And(
                                Apply("class", Seq("c")),
                                Apply("entails", Seq(
                                  "cs",
                                  Apply("instantiated-by", Seq("p", "c"))
                                ))
                              ),
                              Apply("entails", Seq(
                                "cs",
                                Apply("instance-of", Seq("p", "c"))
                              ))
                            ))
  val cClass = Assert(classTerm)

  // C-Cut
  val cutTerm = Forall(
                  Seq(
                    SortedVar("cs1", Constraints),
                    SortedVar("cs2", Constraints),
                    SortedVar("b", "Constraint"),
                    SortedVar("c", "Constraint")
                  ),
                  Implies(
                    And(
                      Apply("entails", Seq("cs1", "c")),
                      Apply("entails", Seq(
                        Apply("insert", Seq("c", "cs2")),
                        "b"
                      ))),
                    Apply("entails", Seq(
                      Apply("conc", Seq("cs1", "cs2")),
                      "b"
                    ))))
  val cCut = Assert(cutTerm)

  // C-Subst
  val substTerm = Forall(
                    Seq(
                      SortedVar("cs", Constraints),
                      SortedVar("x", "String"),
                      SortedVar("a", "Constraint"),
                      SortedVar("p1", "Path"),
                      SortedVar("p2", "Path"),
                      SortedVar("a1", "Constraint"),
                      SortedVar("a2", "Constraint")
                    ),
                    Implies(
                      And(
                        Apply("entails", Seq(
                          "cs",
                          Apply("path-eq", Seq("p1", "p2"))
                        )),
                        And(
                          Not(Eq("p1", "p2")),
                          And(
                            Apply("subst", Seq("a", "x", "p1", "a1")),
                            And(
                              Apply("subst", Seq("a", "x", "p2", "a2")),
                              And(
                                Apply("variable", Seq("x")),
                                Apply("entails", Seq("cs", "a1"))
                              )
                            )))),
                      Apply("entails", Seq("cs", "a2"))
                    ))
  val cSubst = Assert(substTerm)

//  val isPathEqProp = DeclareFun(SimpleSymbol("isPathEq"), Seq(SimpleSymbol("Constraint")), Bool)
////  val isPathEqProp = DefineFun(FunctionDef(SimpleSymbol("isPathEq"), Seq(SortedVar(SimpleSymbol("c"), SimpleSymbol("Constraint"))), Bool,
////    Exists(Seq(SortedVar(SimpleSymbol("p1"), SimpleSymbol("Path")), SortedVar(SimpleSymbol("p2"), SimpleSymbol("Path"))),
////      Eq("c", Apply(SimpleSymbol("pathEq"), Seq(SimpleSymbol("p1"), SimpleSymbol("p2")))))))
//  val isInstanceOfProp = DeclareFun(SimpleSymbol("isInstanceOf"), Seq(SimpleSymbol("Constraint")), Bool)
//  val isInstantiatedByProp = DeclareFun(SimpleSymbol("isInstantiatedBy"), Seq(SimpleSymbol("Constraint")), Bool)

  // TODO: add property that ordering in constraints list doesnt matter?

  val datatypes = Seq(pathDatatype, constraintDatatype)
  val funs = Seq(concat)
  val baseProps = Seq(classProp, varProp)
  val subst = Seq(substPath, substConstraint, substProp)
  val sequentCalculus = Seq(entails, cIdent, cRefl, cClass, cCut, cSubst)

  def all: SMTLibScript = SMTLibScript(datatypes ++ funs ++ baseProps ++ subst ++ sequentCalculus)
}

object AxiomsTest extends App {
  val solver = new Z3Solver(Axioms.all, true)

  solver.addCommand(CheckSat)

  solver.execute()
}
