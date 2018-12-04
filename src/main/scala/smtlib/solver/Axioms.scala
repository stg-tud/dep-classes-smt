package smtlib.solver

import smtlib.SMTLibScript
import smtlib.syntax._
import smtlib.syntax.Implicit._

// TODO: Z3 version 4.8.3 - 64 bit produces errors for nil (in pattern matching). mismatching number of variables supplied to constructor
object Axioms {
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

  // C-Refl
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
                      Apply("conc", Seq("cs1", "cs2")),
                      "b"
                    ))))
  private val cCut = Assert(Annotate(cutTerm, Seq(KeyValueAttribute(Keyword("named"), "C-Cut"))))

  // C-Subst
  private val substTerm = Forall(
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

  private val entailsOrderingTerm = Forall(
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
  private val entailsOrdering = Assert(Annotate(entailsOrderingTerm, Seq(KeyValueAttribute(Keyword("named"), "ordering"))))

  private val datatypes = Seq(pathDatatype, constraintDatatype)
  private val funs = Seq(concat, elem)
  private val baseProps = Seq(classProp, varProp, inProgProp)
  private val subst = Seq(substPath, substConstraint, substConstraints, substProp)
  private val sequentCalculus = Seq(entailsProp, EntailsProp, cIdent, cRefl, cClass, cCut, cSubst, cProg)
  private val additionalProperties = Seq(/*entailsOrdering*/)

  def all: SMTLibScript = SMTLibScript(datatypes ++ funs ++ baseProps ++ subst ++ sequentCalculus ++ additionalProperties)

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

  def cls(s: String) = Apply("class", Seq(SMTLibString(s)))

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
  val y = Axioms.path("x")
  val z = Axioms.path("x")

  val xy = Axioms.pathEq(x, y)
  val yz = Axioms.pathEq(y, z)
  val xz = Axioms.pathEq(x, z)

  val assertion = Assert(Not(Axioms.entails(Seq(xy, yz), xz)))

  solver.addCommands(Seq(assertion, CheckSat, GetUnsatCore))
  val (exit, out) = solver.execute(30000)
}
