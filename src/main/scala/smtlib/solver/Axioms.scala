package smtlib.solver

import smtlib.SMTLibScript
import smtlib.syntax._
import smtlib.syntax.Implicit._

// TODO: Z3 version 4.8.3 - 64 bit produces errors for nil (in pattern matching). mismatching number of variables supplied to constructor
// TODO: update calculus rules with path-exists property
object Axioms {
  // TODO: rename insert to cons for cvc4 as insert is predefined for sets
  // TODO: also change axioms to use sets instead of lists for cvc4 (http://cvc4.cs.stanford.edu/wiki/Sets)
  private val listDatatype = DeclareDatatype("List", ParDatatype(
    Seq( // symbols
      SimpleSymbol("T")
    ),
    Seq( // constructors
      ConstructorDec("nil", Seq()),
      ConstructorDec("insert", Seq(
        SelectorDec("head", "T"),
        SelectorDec("tail", Sorts("List", Seq("T")))
      ))
    )
  ))

  private val Constraints = Sorts("List", Seq("Constraint"))

  /**
    * Path datatype declaration
    * Constrcutors:
    *   var String
    *   pth Path String
    */
  private val pathDatatype = DeclareDatatype("Path", ConstructorDatatype(Seq(
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
  private val constraintDatatype = DeclareDatatype(SimpleSymbol("Constraint"), ConstructorDatatype(Seq(
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
  private val concat = DefineFunRec(
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
  private val elem = DefineFunRec(
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

  /** String is a class name  */
  private val classProp = DeclareFun("class", Seq("String"), Bool)

  /** String is a variable name  */
  private val varProp = DeclareFun("variable", Seq("String"), Bool)

  /** Path is valid */
  private val pathProp = DeclareFun("path-exists", Seq("Path"), Bool)

  /** \all x. cs => c in program */
  private val inProgProp = DeclareFun("in-program", Seq("String", Constraints, "Constraint"), Bool)

  /**
    * Substitution function for paths.
    * param p1 Base path for the substitution
    * param x Variable name to be substituted
    * param p2 Path to be substituted
    * return `x` substituted with `p2` in `p1`
    */
  private val substPath = DefineFunRec(
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
  private val substConstraint = DefineFun(
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

  /**
    * Substitution function for a list of constraints.
    * param cs Base constraints for the substitution
    * param x Variable name to be substituted
    * param p Path to be substituted
    * return `x` substituted with `p` in each `c` of `cs`
    */
  private val substConstraints = DefineFunRec(
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
  private val substProp = DefineFun(
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

  private val genPath = DefineFunRec(
                          FunctionDef(
                            "generalize-path",
                            Seq(
                              SortedVar("p1", "Path"),
                              SortedVar("p2", "Path"),
                              SortedVar("x", "String")
                            ),
                            "Path",
                            Ite( // p1 == p2 ? x : ...
                              Eq("p1", "p2"),
                              Apply("var", Seq("x")),
                              Match("p1",
                                Seq(
                                  MatchCase(Pattern("var", Seq("y")),
                                    "p1"),
                                  MatchCase(Pattern("pth", Seq("p", "f")),
                                    Apply("pth", Seq(
                                      Apply("generalize-path", Seq("p", "p2", "x")),
                                      "f"))
                                  )
                                )
                              )
                            )
                          ))

  private val genConstraint = DefineFun(
                                FunctionDef(
                                  "generalize-constraint",
                                  Seq(
                                    SortedVar("c", "Constraint"),
                                    SortedVar("p", "Path"),
                                    SortedVar("x", "String")
                                  ),
                                  "Constraint",
                                  Match("c", Seq(
                                    MatchCase(Pattern("path-eq", Seq("p1", "p2")),
                                      Apply("path-eq", Seq(
                                        Apply("generalize-path", Seq("p1", "p", "x")),
                                        Apply("generalize-path", Seq("p2", "p", "x"))
                                      ))),
                                    MatchCase(Pattern("instance-of", Seq("p1", "cls1")),
                                      Apply("instance-of", Seq(
                                        Apply("generalize-path", Seq("p1", "p", "x")),
                                        "cls1"
                                      ))),
                                    MatchCase(Pattern("instantiated-by", Seq("p1", "cls1")),
                                      Apply("instantiated-by", Seq(
                                        Apply("generalize-path", Seq("p1", "p", "x")),
                                        "cls1"
                                      ))),
                                  ))
                                ))

  private val genConstraints = DefineFunRec(
                                FunctionDef(
                                  "generalize-constraints",
                                  Seq(
                                    SortedVar("cs", Constraints),
                                    SortedVar("p", "Path"),
                                    SortedVar("x", "String")
                                  ),
                                  Constraints,
                                  Match("cs", Seq(
                                    MatchCase(Pattern("nil"), "nil"),
                                    MatchCase(Pattern("insert", Seq("hd", "tl")),
                                      Apply("insert", Seq(
                                        Apply("generalize-constraint", Seq("hd", "p", "x")),
                                        Apply("generalize-constraints", Seq("tl", "p", "x"))
                                      )))
                                  ))
                                ))

  val genProp = DefineFun(
                  FunctionDef(
                    "generalization",
                    Seq(
                      SortedVar("c1", "Constraint"),
                      SortedVar("p", "Path"),
                      SortedVar("x", "String"),
                      SortedVar("c2", "Constraint")
                    ),
                    Bool,
                    Eq(
                      Apply("generalize-constraint", Seq("c1", "p", "x")),
                      "c2"
                    )
                  ))

  // DCC

  /**
    * dcc's sequent calculus judgement
    * entails :: List Constraint -> Constraint -> Bool
    * Entails :: List Constraint -> List Constraint -> Bool
    */
  private val entailsProp = DeclareFun("entails", Seq(Constraints, "Constraint"), Bool)
  private val EntailsProp = DefineFunRec(
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

  // C-Ident
  private val identTerm = Forall(Seq(SortedVar("c", "Constraint")),
                            Apply("entails", Seq(
                              Apply("insert", Seq("c", "nil")),
                              "c"
                            )))
  private val cIdent = Assert(Annotate(identTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Ident"))))

  // C-Refl TODO: evaluate if (path-exists p) should be applied to this
  private val reflTerm = Forall(Seq(SortedVar("p", "Path")),
                            Apply("entails", Seq(
                              "nil",
                              Apply("path-eq", Seq("p", "p"))
                            )))
  private val cRefl = Assert(Annotate(reflTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Refl"))))

  // C-Class
  private val classTerm = Forall(
                            Seq(
                              SortedVar("cs", Constraints),
                              SortedVar("p", "Path"),
                              SortedVar("c", "String")),
                            Implies(
                              And(
                                Apply("class", Seq("c")),
                                And(
                                  Apply("path-exists", Seq("p")),
                                  Apply("entails", Seq(
                                    "cs",
                                    Apply("instantiated-by", Seq("p", "c"))
                                  ))
                                )
                              ),
                              Apply("entails", Seq(
                                "cs",
                                Apply("instance-of", Seq("p", "c"))
                              ))
                            ))
  private val cClass = Assert(Annotate(classTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Class"))))

  // C-Cut //TODO: look what can be done about the concat (ordering wise)
  private val cutTerm = Forall(
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
  private val cCut = Assert(Annotate(cutTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Cut"))))

  // C-Subst //TODO: reorder stuff in the and statements and add path-exists requirement to hopefully help with search space
//  private val substTerm = Forall(
//                    Seq(
//                      SortedVar("cs", Constraints),
//                      SortedVar("x", "String"),
//                      SortedVar("a", "Constraint"),
//                      SortedVar("p1", "Path"),
//                      SortedVar("p2", "Path"),
//                      SortedVar("a1", "Constraint"),
//                      SortedVar("a2", "Constraint")
//                    ),
//                    Implies(
//                      And(
//                        Apply("entails", Seq(
//                          "cs",
//                          Apply("path-eq", Seq("p1", "p2"))
//                        )),
//                        And(
//                          Not(Eq("p1", "p2")),
//                          And(
//                            Apply("subst", Seq("a", "x", "p1", "a1")),
//                            And(
//                              Apply("subst", Seq("a", "x", "p2", "a2")),
//                              And(
//                                Apply("variable", Seq("x")),
//                                Apply("entails", Seq("cs", "a1"))
//                              )
//                            )))),
//                      Apply("entails", Seq("cs", "a2"))
//                    ))
//  private val substTerm = Forall( // added restrictions on variables and paths to use.
//                  Seq(
//                    SortedVar("cs", Constraints),
//                    SortedVar("x", "String"),
//                    SortedVar("a", "Constraint"),
//                    SortedVar("p1", "Path"),
//                    SortedVar("p2", "Path"),
//                    SortedVar("a1", "Constraint"),
//                    SortedVar("a2", "Constraint")
//                  ),
//                  Implies(
//                    And(
//                      Apply("variable", Seq("x")),
//                      And(
//                        Apply("path-exists", Seq("p1")),
//                        And(
//                          Apply("path-exists", Seq("p2")),
//                          And(
//                            Not(Eq("p1", "p2")),
//                            And(
//                              Apply("entails", Seq(
//                                "cs",
//                                Apply("path-eq", Seq("p1", "p2"))
//                              )),
//                              And(
//                                Apply("subst", Seq("a", "x", "p1", "a1")),
//                                And(
//                                  Apply("subst", Seq("a", "x", "p2", "a2")),
//                                  Apply("entails", Seq("cs", "a1"))))))))),
//                    Apply("entails", Seq("cs", "a2"))
//                  ))
//  private val substTerm = Forall( // added p1=p2 elem cs
//                  Seq(
//                    SortedVar("cs", Constraints),
//                    SortedVar("x", "String"),
//                    SortedVar("a", "Constraint"),
//                    SortedVar("c", "Constraint"),
//                    SortedVar("p1", "Path"),
//                    SortedVar("p2", "Path"),
//                    SortedVar("a1", "Constraint"),
//                    SortedVar("a2", "Constraint")
//                  ),
//                  Implies(
//                    And(
//                      Apply("variable", Seq("x")),
//                      And(
//                        Apply("is-path-eq", Seq("c")),
//                        And(
//                          Apply("elem", Seq("c", "cs")),
//                          And(
//                            Eq("p1", Apply("p-right", Seq("c"))),
//                            And(
//                              Eq("p2", Apply("p-left", Seq("c"))),
//                              And(
//                                Not(Eq("p1", "p2")),
//                                And(
//                                  Apply("entails", Seq(
//                                    "cs",
//                                    Apply("path-eq", Seq("p1", "p2"))
//                                  )),
//                                  And(
//                                    Apply("subst", Seq("a", "x", "p1", "a1")),
//                                    And(
//                                      Apply("subst", Seq("a", "x", "p2", "a2")),
//                                      Apply("entails", Seq("cs", "a1"))))))
//                            )
//                          )
//                        ))),
//                    Apply("entails", Seq("cs", "a2"))
//                  ))
//  private val substTerm = Forall( // removed substitution from rhs of implication
//                            Seq(
//                              SortedVar("cs", Constraints),
//                              SortedVar("x", "String"),
//                              SortedVar("a", "Constraint"),
//                              SortedVar("c", "Constraint"),
//                              SortedVar("p1", "Path"),
//                              SortedVar("p2", "Path"),
//                              SortedVar("a1", "Constraint"),
//                            ),
//                            Implies(
//                              And(
//                                Apply("variable", Seq("x")),
//                                And(
//                                  Apply("is-path-eq", Seq("c")),
//                                  And(
//                                    Apply("elem", Seq("c", "cs")),
//                                    And(
//                                      Eq("p1", Apply("p-right", Seq("c"))),
//                                      And(
//                                        Eq("p2", Apply("p-left", Seq("c"))),
//                                        And(
//                                          Apply("entails", Seq("cs", "c")),
//                                          And(
//                                            Apply("subst", Seq("a", "x", "p1", "a1")),
//                                            Apply("entails", Seq("cs", "a1"))))
//                                      )
//                                    )
//                                  ))),
//                              Apply("entails", Seq("cs", "a"))
//                            ))
//  private val substTerm = Forall( // added generalization
//                            Seq(
//                              SortedVar("cs", Constraints),
//                              SortedVar("x", "String"),
//                              SortedVar("a", "Constraint"),
//                              SortedVar("c", "Constraint"),
//                              SortedVar("p1", "Path"),
//                              SortedVar("p2", "Path"),
//                              SortedVar("a1", "Constraint"),
//                              SortedVar("a2", "Constraint")
//                            ),
//                            Implies(
//                              And(
//                                Apply("variable", Seq("x")),
//                                And(
//                                  Apply("is-path-eq", Seq("c")),
//                                  And(
////                                    Apply("elem", Seq("c", "cs")),
////                                    And(
//                                      Eq("p1", Apply("p-right", Seq("c"))),
//                                      And(
//                                        Eq("p2", Apply("p-left", Seq("c"))),
//                                        And(
//                                          Apply("entails", Seq("cs", "c")),
//                                          And(
//                                            Apply("subst", Seq("a", "x", "p1", "a1")),
//                                            And(
//                                              Apply("generalization", Seq("a2", "p2", "x", "a")),
//                                              Apply("entails", Seq("cs", "a1"))
//                                            )))
//                                      //)
//                                    )
//                                  ))),
//                              Apply("entails", Seq("cs", "a2"))
//                            ))
//  private val substTerm = Forall( // reduced quantified variables TODO: is-path-eq is required after accessing the selectors. putting it before makes the axioms unsat
//                            Seq(
//                              SortedVar("x", "String"),
//                              SortedVar("a", "Constraint"),
//                              SortedVar("c", "Constraint"),
//                              SortedVar("cs", Constraints),
//                              SortedVar("a1", "Constraint"),
//                              SortedVar("a2", "Constraint")
//                            ),
//                            Let(
//                              Seq(
//                                VarBinding("p1", Apply("p-right", Seq("c"))),
//                                VarBinding("p2", Apply("p-left", Seq("c")))
//                              ),
//                              Implies(
//                                And(
//                                  Apply("variable", Seq("x")),
//                                  And(
//                                    Apply("is-path-eq", Seq("c")), //Apply("elem", Seq("c", "cs"))
//                                    And(
//                                      Apply("entails", Seq("cs", "c")),
//                                      And(
//                                        Apply("subst", Seq("a", "x", "p1", "a1")),
//                                        And(
//                                          Apply("generalization", Seq("a2", "p2", "x", "a")),
//                                          Apply("entails", Seq("cs", "a1"))
//                                        )))
//                                    )),
//                                Apply("entails", Seq("cs", "a2"))
//                              )))
  private val substTerm = Forall( // reduced quantified variables and moved requirements on c before using selectors
                            Seq(
                              SortedVar("x", "String"),
                              SortedVar("a", "Constraint"),
                              SortedVar("p1", "Path"),
                              SortedVar("p2", "Path"),
                              SortedVar("cs", Constraints),
                              SortedVar("a1", "Constraint"),
                              SortedVar("a2", "Constraint")
                            ),
                            Let(
                              Seq(
                                VarBinding("c", Apply("path-eq", Seq("p1", "p2")))
                              ),
                              Implies(
                                And(
                                  Apply("variable", Seq("x")),
                                  And(
                                    Apply("path-exists", Seq("p1")),
                                    And(
                                      Apply("path-exists", Seq("p2")),
                                      And(
                                        Apply("entails", Seq("cs", "c")),
                                        And(
                                          Apply("subst", Seq("a", "x", "p1", "a1")),
                                          And(
                                            Apply("generalization", Seq("a2", "p2", "x", "a")),
                                            Apply("entails", Seq("cs", "a1"))
                                          )
                                        )
                                      )
                                    )
                                  )
                                ),
                                Apply("entails", Seq("cs", "a2"))
                              )
                            ))
  private val cSubst = Assert(Annotate(substTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Subst"))))

  // C-Prog
  private val progTerm = Forall(
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
  private val cProg = Assert(Annotate(progTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Prog"))))

  // C-Weak
  private val weakTerm = Forall(
                            Seq(
                              SortedVar("cs", Constraints),
                              SortedVar("a", "Constraint"),
                              SortedVar("b", "Constraint")
                            ),
                            Implies(
                              Apply("entails", Seq("cs", "b")),
                              Apply("entails", Seq(
                                Apply("insert", Seq("a", "cs")),
                                "b"
                              ))
                            )
  )
  private val cWeak = Assert(Annotate(weakTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Weak"))))

  // C-Perm (permutation and contraction)
  private val permTerm = Forall(
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
                              ))
  private val cPerm = Assert(Annotate(permTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Perm"))))

  private val datatypes = Seq(pathDatatype, constraintDatatype)
  private val funs = Seq(concat, elem)
  private val subst = Seq(substPath, substConstraint, substConstraints, substProp)
  private val gen = Seq(genPath, genConstraint, genConstraints, genProp)
  private val baseProps = Seq(classProp, varProp, pathProp, inProgProp)
  private val dccProps = Seq(entailsProp, EntailsProp)
  private val structuralRules = Seq(cWeak, cPerm)
  private val dccRules = Seq(cIdent, cRefl, cClass, cCut, cSubst, cProg)

  def all: SMTLibScript = SMTLibScript(datatypes ++ funs ++ subst ++ gen ++ baseProps ++ dccProps ++ structuralRules ++ dccRules)
  def allWithList: SMTLibScript = SMTLibScript(listDatatype +: all.commands)

  def entails(premise: Seq[Term], conclusion: Term): Term = Apply("entails", Seq(premise.foldRight(SimpleSymbol("nil"):Term)((x, xs) => Apply("insert", Seq(x, xs))), conclusion))

  /**
    * Converts a String representing a Path
    * to a Term understandable by the SMTSolver.
    * @param s The String representing a path. E.g.: x.f.g
    * @return A Term representation of `s`. E.g: (pth (pth (var "x") "f") "g")
    */
  def path(s: String): Term = pth(s.split("\\.").toSeq)

  private def pth(p: Seq[String]): Term = p match {
    case Nil :+ x => Apply("var", Seq(SMTLibString(x)))
    case xs :+ x => Apply("pth", Seq(pth(xs), SMTLibString(x)))
  }

  def string(s: String) = SMTLibString(s)

  def cls(s: String) = Apply("class", Seq(SMTLibString(s)))
  def variable(s: String) = Apply("variable", Seq(SMTLibString(s)))
  def pathExists(t: Term) = Apply("path-exists", Seq(t))

  def assertClass(s: String) = Assert(cls(s))
  def assertVariable(s: String) = Assert(variable(s))
  def assertPath(t: Term) = Assert(pathExists(t))

  def pathEq(p1: Term, p2: Term): Term = Apply("path-eq", Seq(p1, p2))
  def instanceOf(p: Term, c: String) = Apply("instance-of", Seq(p, SMTLibString(c)))
  def instantiatedBy(p: Term, c: String) = Apply("instantiated-by", Seq(p, SMTLibString(c)))
}

object AxiomsTest extends App {
  val options = Seq(SetOption(ProduceProofs(true)), SetOption(ProduceUnsatCores(true)))

  val solver = new Z3Solver(Axioms.all, options,true)

//  val pths = Seq(
//    DeclareConst("p1", "Path"),
//    DeclareConst("p2", "Path"),
//    DeclareConst("p3", "Path")
//  )
//  solver.addCommands(pths)
//
//  val cs1 = DeclareConst("cs1", Sorts("List", Seq("Constraint")))
//  val cs2 = DeclareConst("cs2", Sorts("List", Seq("Constraint")))
//  val cs1Empty = Assert(Eq("cs1", "nil"))
//  val cs2Content = Assert(Eq("cs2", Apply("insert", Seq(Apply("path-eq", Seq("p1", "p2")), "nil"))))
//
//  solver.addCommand(cs1)
//  solver.addCommand(cs2)
//  solver.addCommand(cs1Empty)
//  solver.addCommand(cs2Content)
//
//
//  solver.addCommand(CheckSat)
//  solver.addCommand(GetProof)
//
//  solver.execute()

  solver.flush()
  val x = Axioms.path("x")
  val y = Axioms.path("y")
  val z = Axioms.path("z")

  val xy = Axioms.pathEq(x, y)
  val yz = Axioms.pathEq(y, z)
  val xz = Axioms.pathEq(x, z)

  val knowledge = Seq(
    Axioms.assertPath(x),
    Axioms.assertPath(y),
    Axioms.assertPath(z),
    Axioms.assertVariable("x"),
    Axioms.assertVariable("y"),
    Axioms.assertVariable("z")
  )

  val assertion = Assert(Not(Axioms.entails(Seq(xy, yz), xz)))

  solver.addCommands(knowledge ++ Seq(assertion, CheckSat, GetUnsatCore))
  val (exit, out) = solver.execute()
}
