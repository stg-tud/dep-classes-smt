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

  /** c \in cs */
  val elem = DefineFunRec(
              FunctionDef(
                "elem",
                Seq(
                  SortedVar("c", "Constraint"),
                  SortedVar("cs", Constraints)
                ),
                Bool,
                Match("cs", Seq(
                  MatchCase(Pattern("nil"),
                    False()),
                  MatchCase(Pattern("insert", Seq("hd", "tl")),
                    Ite(
                      Eq("c", "hd"),
                      True(),
                      Apply("elem", Seq("c", "tl"))
                    )
                  )
                ))
              ))

  // TODO: asdf
//  val bigAnd = DefineFunRec(
//                FunctionDef("big-and",
//                  Seq(
//                    SortedVar("l", Constraints)
//                  ),
//                  Bool,
//                  Match("l", Seq(
//                    MatchCase(Pattern("nil"), True()),
//                    MatchCase(Pattern("insert", Seq("hd", "tl")), And("hd", Apply("big-and", Seq("tl"))))
//                  ))))

  /** String is a class name  */
  val classProp = DeclareFun("class", Seq("String"), Bool)

  /** String is a variable name  */
  val varProp = DeclareFun("variable", Seq("String"), Bool)

  /** \all x. cs => c in program */
  val inProgProp = DeclareFun("in-program", Seq("String", Constraints, "Constraint"), Bool)

  /**
    * Substitution function for paths.
    * param p1 Base path for the substitution
    * param x Variable name to be substituted
    * param p2 Path to be substituted
    * return `x` substituted with `p2` in `p1`
    */
  val substPath = DefineFunRec(
                    FunctionDef(
                      "subst-path",
                      Seq(
                        SortedVar("p1", "Path"),
                        SortedVar("x", "String"),
                        SortedVar("p2", "Path")
                      ),
                      "Path",
                      Match("p1",
                        Seq(
                          MatchCase(Pattern("var", Seq("y")),
                            Ite(Eq("x", "y"), "p2", "p1")),
                          MatchCase(Pattern("pth", Seq("p", "f")),
                            Apply("pth", Seq(Apply("subst-path", Seq("p", "x", "p2")), "f"))
                          )
                        ))
                    ))

  /**
    * Substitution function for constraints.
    * param c Base constraint for the substitution
    * param x Variable name to be substituted
    * param p Path to be substituted
    * return `x` substituted with `p` in `c`
    */
  val substConstraint = DefineFun(
                          FunctionDef(
                            "subst-constraint",
                            Seq(
                              SortedVar("c", "Constraint"),
                              SortedVar("x", "String"),
                              SortedVar("p", "Path")
                            ),
                            "Constraint",
                            Match("c",
                              Seq(
                                MatchCase(Pattern("path-eq", Seq("p1", "p2")),
                                  Apply("path-eq", Seq(
                                    Apply("subst-path", Seq("p1", "x", "p")),
                                    Apply("subst-path", Seq("p2", "x", "p"))))),
                                MatchCase(Pattern("instance-of", Seq("p1", "cls1")),
                                  Apply("instance-of", Seq(
                                    Apply("subst-path", Seq("p1", "x", "p")),
                                    "cls1"
                                  ))),
                                MatchCase(Pattern("instantiated-by", Seq("p1", "cls1")),
                                  Apply("instantiated-by", Seq(
                                    Apply("subst-path", Seq("p1", "x", "p")),
                                    "cls1"
                                  )))
                              ))
                          ))

  val substConstraints = DefineFunRec(
                          FunctionDef(
                            "subst-constraints",
                            Seq(
                              SortedVar("cs", Constraints),
                              SortedVar("x", "String"),
                              SortedVar("p", "Path")
                            ),
                            Constraints,
                            Match("cs",
                              Seq(
                                MatchCase(Pattern("nil"),
                                  "nil"),
                                MatchCase(Pattern("insert", Seq("hd", "tl")),
                                  Apply("insert", Seq(
                                    Apply("subst-constraint", Seq("hd", "x", "p")),
                                    Apply("subst-constraints", Seq("tl", "x", "p"))
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
    * Entails :: List Constraint -> List Constraint -> Bool
    */
  val entails = DeclareFun("entails", Seq(Constraints, "Constraint"), Bool)
  val Entails = DefineFunRec(
                  FunctionDef(
                    "Entails",
                    Seq(
                      SortedVar("cs1", Constraints),
                      SortedVar("cs2", Constraints)
                    ),
                    Bool,
                    Match("cs2", Seq(
                      MatchCase(Pattern("nil"),
                        True()),
                      MatchCase(Pattern("insert", Seq("hd", "tl")),
                        And(
                          Apply("entails", Seq("cs1", "hd")),
                          Apply("Entails", Seq("cs1", "tl"))
                        ))
                    ))
                  ))

//  val Entails = DeclareFun("Entails", Seq(Constraints, Constraints), Bool)
//  val EntailsBody = Assert(
//                      Forall(Seq(
//                        SortedVar("cs1", Constraints),
//                        SortedVar("cs2", Constraints)),
//                        Implies(
//                          Forall(Seq(SortedVar("c", "Constraint")),
//                            And(
//                              Apply("elem", Seq("c", "cs2")),
//                              Apply("entails", Seq("cs1", "c"))
//                            )),
//                          Apply("Entails", Seq("cs1", "cs2")))))

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

  // C-Prog
  val progTerm = Forall(
                  Seq(
                    SortedVar("cs1", Constraints),
                    SortedVar("cs2", Constraints),
                    SortedVar("c", "Constraint"),
                    SortedVar("x", "String"),
                    SortedVar("p", "Path"),
                  ),
                  Implies(
                    And(
                      Apply("in-program", Seq(
                        "x",
                        "cs1",
                        "c"
                      )),
                      Apply("Entails", Seq(
                        "cs2",
                        Apply("subst-constraints", Seq("cs1", "x", "p"))
                      ))
                    ),
                    Apply("entails", Seq(
                      "cs2",
                      Apply("subst-constraint", Seq("c", "x", "p"))
                    ))
                  ))
  val cProg = Assert(progTerm)

  val entailsOrdering = Assert(
                          Forall(
                            Seq(
                              SortedVar("cs1", Constraints),
                              SortedVar("cs2", Constraints),
                              SortedVar("c", "Constraint")
                            ),
                            Implies(
                              And(
                                Forall(
                                  Seq(SortedVar("a", "Constraint")),
                                  And(
                                    Implies(
                                      Apply("elem", Seq("a", "cs1")),
                                      Apply("elem", Seq("a", "cs2"))
                                    ),
                                    Implies(
                                      Not(Apply("elem", Seq("a", "cs1"))),
                                      Not(Apply("elem", Seq("a", "cs2")))
                                    )
                                  )
                                ),
                                Apply("entails", Seq("cs1", "c"))
                              ),
                              Apply("entails", Seq("cs2", "c"))
                            )
                          ))

  val datatypes = Seq(pathDatatype, constraintDatatype)
  val funs = Seq(concat, elem)
  val baseProps = Seq(classProp, varProp, inProgProp)
  val subst = Seq(substPath, substConstraint, substConstraints, substProp)
  val sequentCalculus = Seq(entails, Entails, cIdent, cRefl, cClass, cCut, cSubst, cProg)
  val additionalProperties = Seq(entailsOrdering)

  def all: SMTLibScript = SMTLibScript(datatypes ++ funs ++ baseProps ++ subst ++ sequentCalculus ++ additionalProperties)
}

object AxiomsTest extends App {
  val solver = new Z3Solver(Axioms.all, true)

  solver.addCommand(DeclareConst("l", Sorts("List", Seq("Constraint"))))
  solver.addCommand(DeclareConst("c", "Constraint"))
  solver.addCommand(Assert(Eq("l", Apply("insert", Seq("c", "nil")))))

  solver.addCommand(CheckSat)

  solver.execute()
}
