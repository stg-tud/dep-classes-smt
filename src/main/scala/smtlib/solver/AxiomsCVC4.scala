package smtlib.solver

import smtlib.SMTLibScript
import smtlib.syntax._
import smtlib.syntax.Implicit._

object AxiomsCVC4 {
  // TODO: rename insert to cons for cvc4 as insert is predefined for sets
  // TODO: also change axioms to use sets instead of lists for cvc4 (http://cvc4.cs.stanford.edu/wiki/Sets)
//  private val listDatatype = DeclareDatatype("List", ParDatatype(
//    Seq( // symbols
//      SimpleSymbol("T")
//    ),
//    Seq( // constructors
//      ConstructorDec("nil", Seq()),
//      ConstructorDec("cons", Seq(
//        SelectorDec("hd", "T"),
//        SelectorDec("tl", Sorts("List", Seq("T")))
//      ))
//    )
//  ))

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

  private val Constraints = Sorts("Set", Seq("Constraint"))

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

  // TODO: subst-constraints for sets

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

  // TODO: generalize-constraints for sets

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
    * entails :: Set Constraint -> Constraint -> Bool
    * Entails :: Set Constraint -> Set Constraint -> Bool
    */
  private val entailsProp = DeclareFun("entails", Seq(Constraints, "Constraint"), Bool)
  private val EntailsProp = DefineFun(
                  FunctionDef(
                    "Entails",
                    Seq(
                      SortedVar("cs1", Constraints),
                      SortedVar("cs2", Constraints)
                    ),
                    Bool,
                    Forall(Seq(
                      SortedVar("c", "Constraint")
                    ),
                      Implies(
                        Apply("member", Seq("c", "cs2")),
                        Apply("entails", Seq("cs1", "c"))
                      ))
                  ))

  // C-Ident
  private val identTerm = Forall(Seq(SortedVar("c", "Constraint")),
                            Apply("entails", Seq(
                              Apply("singleton", Seq("c")),
                              "c"
                            )))
  private val cIdent = Assert(Annotate(identTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Ident"))))

  // C-Refl
  private val reflTerm = Forall(Seq(SortedVar("p", "Path")),
                            Apply("entails", Seq(
                              IdentifierAs("emptyset", Constraints),
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

  // C-Cut
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
                      Apply("union", Seq("cs1", "cs2")),
                      "b"
                    ))))
  private val cCut = Assert(Annotate(cutTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Cut"))))

  // C-Subst
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
//                        Apply("subst-constraints", Seq("cs1", "x", "p")) TODO: implement subst-constraints
//                      ))
//                    ),
//                    Apply("entails", Seq(
//                      "cs2",
//                      Apply("subst-constraint", Seq("c", "x", "p"))
//                    ))
//                  ))
//  private val cProg = Assert(Annotate(progTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Prog"))))

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


  private val datatypes = Seq(pathDatatype, constraintDatatype)
  private val funs = Seq()
  private val subst = Seq(substPath, substConstraint, /*substConstraints,*/ substProp)
  private val gen = Seq(genPath, genConstraint, /*genConstraints,*/ genProp)
  private val baseProps = Seq(classProp, varProp, pathProp, inProgProp)
  private val dccProps = Seq(entailsProp, EntailsProp)
  private val structuralRules = Seq(cWeak/*, cPerm*/) // permutation not needed for sets
  private val dccRules = Seq(cIdent, cRefl, cClass, cCut, cSubst/*, cProg*/)

  def all: SMTLibScript = SMTLibScript(datatypes ++ funs ++ subst ++ gen ++ baseProps ++ dccProps ++ structuralRules ++ dccRules)

  def entails(premise: Seq[Term], conclusion: Term): Term =
    Apply("entails",
      Seq(
        premise.foldRight(IdentifierAs("emptyset", Constraints):Term)((x, xs) => Apply("insert", Seq(x, xs))),
        conclusion))

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

object NoiceTest extends App {
  val options = Seq(SetOption(ProduceProofs(true)), SetOption(ProduceUnsatCores(true)))

  val solver = new CVC4Solver(AxiomsCVC4.all, options,true)

  solver.flush()
  val x = AxiomsCVC4.path("x")
  val y = AxiomsCVC4.path("y")
  val z = AxiomsCVC4.path("z")

  val xy = AxiomsCVC4.pathEq(x, y)
  val yz = AxiomsCVC4.pathEq(y, z)
  val xz = AxiomsCVC4.pathEq(x, z)

  val knowledge = Seq(
    AxiomsCVC4.assertPath(x),
    AxiomsCVC4.assertPath(y),
    AxiomsCVC4.assertPath(z),
    AxiomsCVC4.assertVariable("x"),
    AxiomsCVC4.assertVariable("y"),
    AxiomsCVC4.assertVariable("z")
  )

  val assertion = Assert(Not(AxiomsCVC4.entails(Seq(xy, yz), xz)))


  solver.addCommands(knowledge :+ assertion)
  solver.addCommand(CheckSat)
  //solver.addCommand(GetUnsatCore)
  val (exit, out) = solver.execute()
}
