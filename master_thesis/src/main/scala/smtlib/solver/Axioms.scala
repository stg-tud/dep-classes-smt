package smtlib.solver

import smtlib.{SMTLibCommand, SMTLibScript}
import smtlib.syntax._
import smtlib.syntax.Implicit._

// TODO: update calculus rules with path-exists property
object Axioms {
  // TODO: change axioms to use sets instead of lists for cvc4 (http://cvc4.cs.stanford.edu/wiki/Sets)
  private val Constraints = "CList" //Sorts("List", Seq("Constraint"))

  private val constraintListDatatype = DeclareDatatype("CList", ConstructorDatatype(Seq(
    ConstructorDec("empty", Seq()),
    ConstructorDec("construct", Seq(
      SelectorDec("first", "Constraint"),
      SelectorDec("rest", "CList")
    ))
  )))

  private val constraintsListDatatype = DeclareDatatype("CsList", ConstructorDatatype(Seq(
    ConstructorDec("nan", Seq()),
    ConstructorDec("cons", Seq(
      SelectorDec("hd", Constraints),
      SelectorDec("tl", "CsList")
    ))
  )))

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
                  Ite(
                    Apply("is-construct", Seq("l1")),
                    Apply("construct", Seq(
                      Apply("first", Seq("l1")),
                      Apply("conc", Seq(Apply("rest", Seq("l1")), "l2"))
                    )),
                    "l2"
                  )
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
                Ite(
                  Apply("is-construct", Seq("cs")),
                  Ite(
                    Eq("c", Apply("first", Seq("cs"))),
                    True(),
                    Apply("elem", Seq("c", Apply("rest", Seq("cs"))))
                  ),
                  False()
                )
              ))

  /** big or */
  private val bigOrEntails = DefineFunRec(
    FunctionDef(
      "big-or-Entails",
      Seq(
        SortedVar("ccs", "CsList"),
        SortedVar("cs", Constraints)
      ),
      Bool,
      Ite(
        Apply("is-cons", Seq("ccs")),
//        Let(
//          Seq(
//            VarBinding("cs1", Apply("hd", Seq("ccs"))),
//            VarBinding("ccs1", Apply("tl", Seq("ccs")))
//          ),
//          Ite(
//            Eq("ccs1", "nan"),
//            Apply("Entails", Seq("cs", "cs1")),
//            Or(
//              Apply("Entails", Seq("cs", "cs1")),
//              Apply("big-or-Entails", Seq("ccs1", "cs"))
//            )
//          )
//        ),
        Or(
          Apply("Entails", Seq("cs", Apply("hd", Seq("ccs")))),
          Apply("big-or-Entails", Seq(Apply("tl", Seq("ccs")), "cs"))
        ),
        False()
      )
    )
  )

  /** String is a class name  */
  private val classProp = DeclareFun("class", Seq("String"), Bool)

  /** String is a variable name  */
  private val varProp = DeclareFun("variable", Seq("String"), Bool)

  /** Path is valid */
  private val pathProp = DeclareFun("path-exists", Seq("Path"), Bool)

  /** \all x. cs => c in program */
//  private val inProgProp = DeclareFun("in-program", Seq("String", Constraints, "Constraint"), Bool)

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
                      Ite(
                        Apply("is-var", Seq("p1")),
                        Ite(Eq("x", Apply("id", Seq("p1"))), "p2", "p1"),
                        Apply("pth", Seq(
                          Apply("subst-path", Seq(Apply("obj", Seq("p1")), "x", "p2")),
                          Apply("field", Seq("p1"))
                        ))
                      )
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
                            Ite(
                              Apply("is-path-eq", Seq("c")),
                              Apply("path-eq", Seq(
                                Apply("subst-path", Seq(Apply("p-left", Seq("c")), "x", "p")),
                                Apply("subst-path", Seq(Apply("p-right", Seq("c")), "x", "p"))
                              )),
                              Ite(
                                Apply("is-instance-of", Seq("c")),
                                Apply("instance-of", Seq(
                                  Apply("subst-path", Seq(Apply("instance", Seq("c")), "x", "p")),
                                  Apply("cls", Seq("c"))
                                )),
                                Apply("instantiated-by", Seq(
                                  Apply("subst-path", Seq(Apply("object", Seq("c")), "x", "p")),
                                  Apply("clsname", Seq("c"))
                                ))
                              )
                            )
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
                            Ite(
                              Apply("is-construct", Seq("cs")),
                              Apply("construct", Seq(
                                Apply("subst-constraint", Seq(Apply("first", Seq("cs")), "x", "p")),
                                Apply("subst-constraints", Seq(Apply("rest", Seq("cs")), "x", "p"))
                              )),
                              "empty"
                            )
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
                              Ite(
                                Apply("is-var", Seq("p1")),
                                "p1",
                                Apply("pth", Seq(
                                  Apply("generalize-path", Seq(Apply("obj", Seq("p1")), "p2", "x")),
                                  Apply("field", Seq("p1"))))
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
                                  Ite(
                                    Apply("is-path-eq", Seq("c")),
                                    Apply("path-eq", Seq(
                                      Apply("generalize-path", Seq(Apply("p-left", Seq("c")), "p", "x")),
                                      Apply("generalize-path", Seq(Apply("p-right", Seq("c")), "p", "x"))
                                    )),
                                    Ite(
                                      Apply("is-instance-of", Seq("c")),
                                      Apply("instance-of", Seq(
                                        Apply("generalize-path", Seq(Apply("instance", Seq("c")), "p", "x")),
                                        Apply("cls", Seq("c"))
                                      )),
                                      Apply("instantiated-by", Seq(
                                        Apply("generalize-path", Seq(Apply("object", Seq("c")), "p", "x")),
                                        Apply("clsname", Seq("c"))
                                      ))
                                    )
                                  )
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
                                  Ite(
                                    Apply("is-construct", Seq("cs")),
                                    Apply("construct", Seq(
                                      Apply("generalize-constraint", Seq(Apply("first", Seq("cs")), "p", "x")),
                                      Apply("generalize-constraints", Seq(Apply("rest", Seq("cs")), "p", "x"))
                                    )),
                                    "empty"
                                  )
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
                    Ite(
                      Apply("is-construct", Seq("cs2")),
                      And(
                        Apply("entails", Seq("cs1", Apply("first", Seq("cs2")))),
                        Apply("Entails", Seq("cs1", Apply("rest", Seq("cs2"))))
                      ),
                      True()
                    )
                  ))

  // C-Ident
  private val identTerm = Forall(Seq(SortedVar("c", "Constraint")),
                            Apply("entails", Seq(
                              Apply("construct", Seq("c", "empty")),
                              "c"
                            )))
  private val cIdent = Assert(Annotate(identTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Ident"))))

  // C-Ident (direct closure)
  private val directIdentTerm = Forall(Seq(SortedVar("c", "Constraint"), SortedVar("cs", Constraints)),
    Implies(
      Apply("elem", Seq("c", "cs")),
      Apply("entails", Seq("cs", "c"))))
  private val cDirectIdent = Assert(Annotate(directIdentTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Ident"))))

  // C-Refl TODO: evaluate if (path-exists p) should be applied to this
  private val reflTerm = Forall(Seq(SortedVar("p", "Path")),
                            Apply("entails", Seq(
                              "empty",
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
                        Apply("construct", Seq("c", "cs2")),
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

  // C-Prog // TODO: change on this rule broke patheq symmetrict 2 test. why/how?
//  private val progTerm = Forall(
//                  Seq(
//                    SortedVar("cs1", Constraints),
//                    SortedVar("cs2", Constraints),
//                    SortedVar("c", "Constraint"),
//                    SortedVar("x", "String"),
//                    SortedVar("p", "Path"),
//                  ),
//                  Implies(
//                    And(
//                      Apply("in-program", Seq(
//                        "x",
//                        "cs1",
//                        "c"
//                      )),
//                      Apply("Entails", Seq(
//                        "cs2",
//                        Apply("subst-constraints", Seq("cs1", "x", "p"))
//                      ))
//                    ),
//                    Apply("entails", Seq(
//                      "cs2",
//                      Apply("subst-constraint", Seq("c", "x", "p"))
//                    ))
//                  ))
//  private val progTerm = Forall( // TODO: remove x, whats p for?
//                          Seq(
//                            SortedVar("cs1", Constraints),
//                            SortedVar("cs2", Constraints),
//                            SortedVar("c", "Constraint"),
//                            SortedVar("x", "String"),
//                            SortedVar("p", "Path"),
//                          ),
//                          Implies(
//                            And(
//                              Apply("variable", Seq("x")),
//                              And(
//                                Apply("path-exists", Seq("p")),
//                                And(
//                                  Apply("in-program", Seq(
//                                    "x",
//                                    "cs1",
//                                    "c"
//                                  )),
//                                  Apply("Entails", Seq(
//                                    "cs2",
//                                    Apply("subst-constraints", Seq("cs1", "x", "p"))
//                                  ))
//                                )
//                              )
//                            ),
//                            Apply("entails", Seq(
//                              "cs2",
//                              Apply("generalize-constraint", Seq("c", "p", "x"))
//                            ))
//                          ))
  // TODO: preprocess in-prog (enumerate constraints in rule)
  // TODO: remove variable from in-prog?
  // TODO: change in-prog to lookup function
//  private val progTerm = Forall(
//                  Seq(
//                    SortedVar("cs1", Constraints),
//                    SortedVar("c", "Constraint"),
//                    SortedVar("x", "String"),
//                  ),
//                  Implies(
//                    And(
//                      Apply("variable", Seq(SMTLibString("x"))),
//                      And(
//                        Apply("in-program", Seq(
//                          SMTLibString("x"),
//                          makeList(Seq(instanceOf(path("x"), "Zero"))),
//                          "c"
//                        )),
//                        Apply("Entails", Seq(
//                          "cs1",
//                          makeList(Seq(instanceOf(path("x"), "Zero")))
//                        ))
//                      )
//                    ),
//                    Apply("entails", Seq(
//                      "cs1",
//                      "c"
//                    ))
//                  ))
//  private val progTerm2 = Forall(
//    Seq(
//      SortedVar("cs1", Constraints),
//      SortedVar("c", "Constraint")
//    ),
//    Let(Seq(
//      VarBinding("cs2", Apply("lookup-program-entailment", Seq("c")))
//    ),
//      Implies(
//        And(
//          Not(Eq("cs2", "nil")), // TODO: change to whatever undefined will be for lookup
//          Apply("Entails", Seq("cs1", "cs2"))
//        ),
//        Apply("entails", Seq(
//          "cs1",
//          "c"
//        ))
//      ))
//  )
//  private val cProg = Assert(Annotate(progTerm2, Seq(KeyValueAttribute(Keyword("named"), "C-Prog"))))

  // C-Weak TODO: don't rely on structure of insert, should be able to weaken on arbitrary position
  private val weakTerm = Forall(
                            Seq(
                              SortedVar("cs", Constraints),
                              SortedVar("a", "Constraint"),
                              SortedVar("b", "Constraint")
                            ),
                            Implies(
                              Apply("entails", Seq("cs", "b")),
                              Apply("entails", Seq(
                                Apply("construct", Seq("a", "cs")),
                                "b"
                              ))
                            )
  )
  private val cWeak = Assert(Annotate(weakTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Weak"))))

  // C-Perm (permutation and contraction) TODO: find more practical solution that doesnt require pre knowledge (see test)
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

  private val datatypes = Seq(pathDatatype, constraintDatatype, constraintListDatatype, constraintsListDatatype)
  private val funs = Seq(concat, elem)
  private val subst = Seq(substPath, substConstraint, substConstraints/*, substProp*/)
  private val gen = Seq(genPath, genConstraint/*, genConstraints, genProp*/)
  private val baseProps = Seq(classProp, varProp, pathProp)
  private val dccProps = Seq(entailsProp, EntailsProp)
  private val structuralRules = Seq(cWeak, cPerm)
  private val dccClosureRules = Seq(cIdent)
  private val dccRules = Seq(cRefl, cClass, cCut/*, cSubst/*, cProg*/*/)
  private val dccPreprocFuns = Seq(bigOrEntails)
  private val dccDirectClosureRules = Seq(cDirectIdent)

  def all: SMTLibScript = SMTLibScript(datatypes ++ funs ++ subst ++ gen ++ baseProps ++ dccProps ++ structuralRules ++ dccClosureRules ++ dccRules ++ dccPreprocFuns)
  def allDirectClosure: SMTLibScript = SMTLibScript(datatypes ++ funs ++ subst ++ gen ++ baseProps ++ dccProps ++ structuralRules ++ dccDirectClosureRules ++ dccRules ++ dccPreprocFuns)
  //def allWithList: SMTLibScript = SMTLibScript(listDatatype +: all.commands)

  def entails(premise: Seq[Term], conclusion: Term): Term = Apply("entails", Seq(makeList(premise), conclusion))
  def entails(premise: Term, conclusion: Term): Term = Apply("entails", Seq(premise, conclusion))

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

  // TODO: finde den variablen dreher (gefunden: p1, p2 in path-eq)
  // TODO: prüfe ob subst/gen zu tauschen ausreicht
  def preprocessSubstRule(x: SMTLibString, p1: Term, p2: Term): Term =
//    Forall(
//      Seq(
//        SortedVar("a2", "Constraint"),
//        SortedVar("cs", Constraints),
//        SortedVar("a", "Constraint"),
//        SortedVar("a1", "Constraint")
//      ),
//      Implies(
//        And(
//          Apply("variable", Seq(x)),
//          And(
//            Apply("path-exists", Seq(p1)),
//            And(
//              Apply("path-exists", Seq(p2)),
//              And(
//                Apply("entails", Seq("cs", Apply("path-eq", Seq(p1, p2)))),
//                And(
//                  Apply("subst", Seq("a", x, p2, "a1")),
//                  And(
//                    Apply("generalization", Seq("a2", p1, x, "a")),
//                    Apply("entails", Seq("cs", "a1"))
//                  )
//                )
//              )
//            )
//          )
//        ),
//        Apply("entails", Seq("cs", "a2"))
//      )
//    )
  Forall(
    Seq(
      SortedVar("a2", "Constraint"),
      SortedVar("cs", Constraints)
    ),
    Let(Seq(
      VarBinding("a", Apply("generalize-constraint", Seq("a2", p1, x)))
    ),
      Let(
        Seq(VarBinding("a1", Apply("subst-constraint", Seq("a", x, p2)))),
        Implies(
          And(
            Apply("entails", Seq("cs", Apply("path-eq", Seq(p1, p2)))),
            Apply("entails", Seq("cs", "a1"))
          ),
          Apply("entails", Seq("cs", "a2"))
        )
      )
    )
  )


  // TODO: can there be more than one rule with the same name? → remove x_p1_p2 from annotation
  def annotateSubstRule(subst: Term, x: String, p1: String, p2: String): Term =
    Annotate(subst, Seq(KeyValueAttribute(Keyword("named"), s"C-Subst_${x}_${p1}_$p2")))

  def preprocessSubstRules(vars: Seq[SMTLibString], paths: Seq[(String, Term)]): Seq[SMTLibCommand] = {
    var substRules: Seq[SMTLibCommand] = Seq()
    val pathPairs: Seq[((String, Term), (String, Term))] = makePathPairs(paths)
    vars.foreach(x =>
      pathPairs.foreach { case (_p1, _p2) =>
        val (s1, p1) = _p1
        val (s2, p2) = _p2

        val substRule = annotateSubstRule(preprocessSubstRule(x, p1, p2), x.s, s1, s2)
        substRules = substRules :+ Assert(substRule)
      }
    )

    substRules
  }

  // TODO: remove symmetric entries?
  // TODO: remove reflexive entries?
  private def makePathPairs(paths: Seq[(String, Term)]): Seq[((String, Term), (String, Term))] = {
    var pairs: Seq[((String, Term), (String, Term))] = Seq()

    paths.foreach(p => pairs = pairs ++ paths.map(q => (p, q)))

    pairs
  }

  // TODO: evaluate generalization + substitution for C-Prog
  private val progTerm: Term =
    Forall(
      Seq(
        SortedVar("cs", Constraints),
        SortedVar("c", "Constraint")
      ),
      Let(Seq(
        VarBinding("ccs", Apply("lookup-program-entailment", Seq("c")))
      ),
        Implies(
          And(
            Not(Eq("ccs", "nan")), // TODO: change to whatever undefined will be for lookup
            Apply("big-or-Entails", Seq("ccs", "cs"))
          ),
          entails("cs", "c")
        )
      )
    )

  val cProg = Assert(Annotate(progTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Prog"))))

  /**
    * Generates a Term representing a List
    * @param terms The elements of the list to generate
    * @return A Term representing a list of `terms`
    */
  def makeList(terms: Seq[Term]): Term = terms.foldRight(SimpleSymbol("empty"):Term)((x, xs) => Apply("construct", Seq(x, xs)))
//    terms match {
//    case Nil => SimpleSymbol("nil")
//    case t :: rst => Apply(SimpleSymbol("insert"), Seq(t, makeList(rst)))
//  }

  //TODO: remove
//  /**
//    * Generates a Term representing a List
//    * @param terms The elements of the list to generate
//    * @param sort The element sort of the list to generate
//    * @return A Term representing a list of `terms`
//    */
  //def makeList(terms: Seq[Term], sort: Sort): Term = terms.foldRight(IdentifierAs("nil", Sorts("List", Seq(sort))):Term)((x, xs) => Apply("insert", Seq(x, xs)))

  def makeCsList(terms: Seq[Term]): Term = terms.foldRight(SimpleSymbol("nan"): Term)((x, xs) => Apply("cons", Seq(x, xs)))

  def makeOr(terms: Seq[Term]): Term = terms.foldRight(SimpleSymbol("false"):Term)((x, xs) => Or(x, xs))
//    terms match {
//    case Nil => SimpleSymbol("false")
//    case t :: rst => Or(t, makeOr(rst))
//  }

  def string(s: String) = SMTLibString(s)

  def cls(s: String) = Apply("class", Seq(SMTLibString(s)))
  def variable(s: String) = Apply("variable", Seq(SMTLibString(s)))
  def pathExists(t: Term) = Apply("path-exists", Seq(t))
//  def inProg(x: String, cs: Term, c: Term) = Apply("in-program", Seq(SMTLibString(x), cs, c))
//  def inProg(x: String, cs: Seq[Term], c: Term) = Apply("in-program", Seq(SMTLibString(x), makeList(cs), c))

  def assertClass(s: String) = Assert(cls(s))
  def assertVariable(s: String) = Assert(variable(s))
  def assertPath(t: Term) = Assert(pathExists(t))
//  def assertInProg(x: String, cs: Term, c: Term) = Assert(inProg(x, cs, c))
//  def assertInProg(x: String, cs: Seq[Term], c: Term) = Assert(inProg(x, cs, c))

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
