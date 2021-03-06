package smtlib.solver

import smtlib.SMTLibScript
import smtlib.syntax._
import smtlib.syntax.Implicit._

object OldAxioms {
  val printSucces = SetOption(PrintSuccess(true))

  val pathDatatype = DeclareDatatype("Path", ConstructorDatatype(Seq(
    ConstructorDec("var", Seq(SelectorDec("id", "String"))),
    ConstructorDec("pth", Seq(
      SelectorDec("obj", "Path"),
      SelectorDec("field", "String")
    ))
  )))

  /** String is a class name  */
  val classProp = DeclareFun("class", Seq("String"), Bool)

  /** String is a variable name  */
  val varProp = DeclareFun("variable", Seq("String"), Bool)

  val pathEqProp = DeclareFun("path-eq", Seq("Path", "Path"), Bool)
  val pathEqRefl = Assert(Forall(Seq(SortedVar("p", "Path")), Apply("path-eq", Seq("p", "p"))))
  val pathEqSym = Assert(Forall(Seq(SortedVar("p1", "Path"), SortedVar("p2", "Path")),
                          Implies(Apply("path-eq", Seq("p1", "p2")), Apply("path-eq", Seq("p2", "p1")))))
  val pathEqTrans = Assert(Forall(Seq(SortedVar("p1", "Path"), SortedVar("p2", "Path"), SortedVar("p3", "Path")),
                          Implies(And(Apply("path-eq", Seq("p1", "p2")), Apply("path-eq", Seq("p2", "p3"))), Apply("path-eq", Seq("p1", "p3")))))

  /**
    * Path equivalency axioms in order.
    * path-eq is equivalence relation: reflexive, symmetric, transitive
    * Requires Path datatype.
     */
  val pathEqAxioms = Seq(pathEqProp, pathEqRefl, pathEqSym, pathEqTrans)

  val instanceOfProp = DeclareFun("instance-of", Seq("Path", "String"), Bool)

  /**
    * instance of axioms in order.
    * Requires Path datatype.
    */
  val instanceOfAxioms = Seq(instanceOfProp)

  val instantiatedByProp = DeclareFun("instantiated-by", Seq("Path", "String"), Bool)

  val objIsInstance = Assert(Forall(Seq(SortedVar("p", "Path"), SortedVar("c", "String")),
                              Implies(
                                Apply("instantiated-by", Seq("p", "c")),
                                Apply("instance-of", Seq("p", "c")))))

  /**
    * instantiated by axioms in order.
    * Requires Path datatype.
    */
  val instantiatedByAxioms = Seq(instantiatedByProp, objIsInstance)

  // substitution
  // val substProp = DefineFun(
  //                    FunctionDef(
  //                      "subst",
  //                      Seq(SortedVar("c1", "Consraint"), // ???
  //                          SortedVar("x", "String"), // identifier
  //                          SortedVar("p", "Path"), // substitutee
  //                          SortedVar("c2", "Constraint")), // "result"
  //                      Bool,
  //                      ???))
  val substProp = DefineFun(
                    FunctionDef(
                      "subst",
                      Seq(SortedVar("p1", "Path"), // replace in this
                          SortedVar("x", "String"), // identifier
                          SortedVar("p2", "Path"), // replacement
                          SortedVar("p3", "Path")), // "result"
                      Bool,
                      Eq(Apply("subst-path", Seq("p1", "x", "p2")), "p3")
                    ))

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
    * path substitution axioms in order.
    * Requires Path datatype.
    */
  val substAxioms = Seq(substPath, substProp)

  // sequent calculus

  val bigAnd = DefineFunRec(
                FunctionDef("big-and",
                  Seq(
                    SortedVar("bs", Sorts("List", Seq(Bool)))
                  ),
                  Bool,
                  Match("bs", Seq(
                    MatchCase(Pattern("nil"), True()),
                    MatchCase(Pattern("insert", Seq("hd", "tl")), And("hd", Apply("big-and", Seq("tl"))))
                  ))))

  /**
    * dcc's sequent calculus judgement
    * Alternative to big-and: forall b: Bool. b \in l => b
    */
  val entails = DefineFun(
                  FunctionDef("entails",
                    Seq(
                      SortedVar("premise", Sorts("List", Seq(Bool))),
                      SortedVar("conclusion", Bool)
                    ),
                    Bool,
                    Implies(
                      Apply("big-and", Seq("premise")),
                      "conclusion")))

  // C-Ident TODO: class(c)?
  val identPath = Assert(Forall(Seq(SortedVar("p1", "Path"), SortedVar("p2", "Path")),
                          Apply("entails", Seq(
                            Apply("insert", Seq(Apply("path-eq", Seq("p1", "p2")), "nil")),
                            Apply("path-eq", Seq("p1", "p2"))))))
  val identInstOf = Assert(Forall(Seq(SortedVar("p", "Path"), SortedVar("c", "String")),
                            Apply("entails", Seq(
                              Apply("insert", Seq(Apply("instance-of", Seq("p", "c")), "nil")),
                              Apply("instance-of", Seq("p", "c"))))))
  val identInstBy = Assert(Forall(Seq(SortedVar("p", "Path"), SortedVar("c", "String")),
                            Apply("entails", Seq(
                              Apply("insert", Seq(Apply("instantiated-by", Seq("p", "c")), "nil")),
                              Apply("instantiated-by", Seq("p", "c"))))))
  val cIdent = Seq(identPath, identInstOf, identInstBy)

  // C-Refl
  val cRefl = Assert(Forall(Seq(SortedVar("p", "Path")),
                      Apply("entails", Seq("nil", Apply("path-eq", Seq("p", "p"))))))

  // C-Class
  val cClass = Assert(Forall(Seq(SortedVar("as", Sorts("List", Seq(Bool))), SortedVar("p", "Path"), SortedVar("x", "String")),
                        Implies(
                          Apply("entails", Seq(
                            "as",
                            Apply("instantiated-by", Seq("p", "c")))),
                          Apply("entails", Seq(
                            "as",
                            Apply("instance-of", Seq("p", "c")))))))

  val sequentCalculusRules = cIdent :+ cRefl :+ cClass

  def asSMTLib: SMTLibScript = SMTLibScript(
    Seq(printSucces, pathDatatype, classProp, varProp) ++
    pathEqAxioms ++
    instanceOfAxioms ++
    instantiatedByAxioms ++
    substAxioms ++
    Seq(bigAnd, entails)
  )

//  TODO: also old: hard to use as proposition
//  val pathEqDatatype = DeclareDatatype("PathEq", ConstructorDatatype(Seq(
//    ConstructorDec("PathEq", Seq(
//      SelectorDec("left", "Path"),
//      SelectorDec("right", "Path")
//    ))
//  )))
//
//  val instanceOfDatatype = DeclareDatatype("InstanceOf", ConstructorDatatype(Seq(
//    ConstructorDec("InstanceOf", Seq(
//      SelectorDec("path", "Path"),
//      SelectorDec("class", "String")
//    ))
//  )))
//
//  val instantiatedByDatatype = DeclareDatatype("InstantiatedBy", ConstructorDatatype(Seq(
//    ConstructorDec("InstantiatedBy", Seq(
//      SelectorDec("path", "Path"),
//      SelectorDec("class", "String")
//    ))
//  )))
//
//  // TODO: body must be boolean
//  val pathEqRefl = Assert(Forall(Seq(SortedVar("p", "Path")), Apply("PathEq", Seq("p", "p"))))
//
//  def asSMTLib: SMTLibScript = SMTLibScript(Seq(pathDatatype, pathEqDatatype, instanceOfDatatype, instantiatedByDatatype, pathEqRefl))

//  TODO: old: constraint as sort. remove?
//  val constraintDatatype = DeclareDatatype(SimpleSymbol("Constraint"), ConstructorDatatype(Seq(
//    ConstructorDec(SimpleSymbol("pathEq"), Seq(SelectorDec(SimpleSymbol("pLeft"), SimpleSymbol("Path")),
//                                               SelectorDec(SimpleSymbol("pRight"), SimpleSymbol("Path")))),
//    ConstructorDec(SimpleSymbol("instanceOf"), Seq(SelectorDec(SimpleSymbol("instance"), SimpleSymbol("Path")),
//                                                   SelectorDec(SimpleSymbol("class"), SimpleSymbol("String")))),
//    ConstructorDec(SimpleSymbol("instantiatedBy"), Seq(SelectorDec(SimpleSymbol("obj"), SimpleSymbol("Path")),
//                                                       SelectorDec(SimpleSymbol("classname"), SimpleSymbol("String"))))
//  )))
//
//  val isPathEqProp = DeclareFun(SimpleSymbol("isPathEq"), Seq(SimpleSymbol("Constraint")), Bool)
////  val isPathEqProp = DefineFun(FunctionDef(SimpleSymbol("isPathEq"), Seq(SortedVar(SimpleSymbol("c"), SimpleSymbol("Constraint"))), Bool,
////    Exists(Seq(SortedVar(SimpleSymbol("p1"), SimpleSymbol("Path")), SortedVar(SimpleSymbol("p2"), SimpleSymbol("Path"))),
////      Eq("c", Apply(SimpleSymbol("pathEq"), Seq(SimpleSymbol("p1"), SimpleSymbol("p2")))))))
//  val isInstanceOfProp = DeclareFun(SimpleSymbol("isInstanceOf"), Seq(SimpleSymbol("Constraint")), Bool)
//  val isInstantiatedByProp = DeclareFun(SimpleSymbol("isInstantiatedBy"), Seq(SimpleSymbol("Constraint")), Bool)
//
//  def asSMTLib: SMTLibScript = SMTLibScript(Seq(pathDatatype, constraintDatatype, isPathEqProp, isInstanceOfProp, isInstantiatedByProp))
}

object OldAxiomsTest extends App {
  val solver = new Z3Solver(OldAxioms.asSMTLib, debug=true)

  val p1 = DeclareConst("p1", "Path")
  val p2 = DeclareConst("p2", "Path")
  val p3 = DeclareConst("p3", "Path")

  val distinct = Seq(Assert(Distinct("p1", "p2")), Assert(Distinct("p2", "p3")), Assert(Distinct("p1", "p3")))

  val paths = Seq(
    Assert(Eq("p1", Apply("var", Seq(SMTLibString("x"))))),
    Assert(Eq("p2", Apply("var", Seq(SMTLibString("y"))))),
    Assert(Eq("p3", Apply("var", Seq(SMTLibString("z")))))
  )

  val p1EqP2 = Apply("path-eq", Seq("p1", "p2"))
  val p3EqP2 = Apply("path-eq", Seq("p3", "p2"))
  val p1EqP3 = Apply("path-eq", Seq("p1", "p3"))
  val goal = Assert(Implies(And(p1EqP2, p3EqP2), p1EqP3))

  val insts = Seq(Assert(Apply("instantiated-by", Seq("p2", SMTLibString("Bar")))),
                  Assert(Not(Apply("instance-of", Seq("p1", SMTLibString("Foo"))))),
                  Assert(Implies(Apply("instantiated-by", Seq("p2", SMTLibString("Bar"))),
                                 Apply("instance-of", Seq("p2", SMTLibString("Bar"))))))

  solver.addCommands(Seq(p1, p2, p3) ++ distinct ++ paths ++ insts ++ Seq(goal, CheckSat, GetModel))
  solver.execute()

  solver.flush()

  val p1xf1 = Assert(Eq("p1", Apply("pth", Seq(Apply("var", Seq(SMTLibString("x"))), SMTLibString("f1")))))
  val p2xf = Assert(Eq("p2", Apply("pth", Seq(Apply("var", Seq(SMTLibString("x"))), SMTLibString("f")))))
  val p3xff1 = Assert(Eq("p3", Apply("pth",
                                        Seq(
                                          Apply("pth", Seq(Apply("var", Seq(SMTLibString("x"))), SMTLibString("f"))),
                                          SMTLibString("f1")))))

  val subst = Assert(Apply("subst", Seq("p1", SMTLibString("x"), "p2", "p3")))

  solver.addCommands(Seq(p1, p2, p3, p1xf1, p2xf, p3xff1, subst, CheckSat, GetModel))
  solver.execute()


  solver.flush()

  val l1 = DeclareConst("l1", Sorts("List", Seq(Bool)))
  val l2 = DeclareConst("l2", Sorts("List", Seq(Bool)))

  val l1Cnt = Assert(Eq("l1", Apply("insert", Seq(True(), Apply("insert", Seq(True(), Apply("insert", Seq(True(), "nil"))))))))
  val l2Cnt = Assert(Eq("l2", Apply("insert", Seq(True(), Apply("insert", Seq(False(), Apply("insert", Seq(True(), "nil"))))))))

  val l1True = Assert(Apply("big-and", Seq("l1")))
  val l2False = Assert(Not(Apply("big-and", Seq("l2"))))

  solver.addCommands(Seq(l1, l2, l1Cnt, l2Cnt, l1True, l2False, CheckSat, GetModel))
  solver.execute()
//
//  solver.flush()
//
//  val vars = Seq(Assert(Apply("class", Seq(SMTLibString("x")))), Assert(Apply("class", Seq(SMTLibString("y")))), Assert(Apply("class", Seq(SMTLibString("z")))))
//  val exists = Assert(Forall(Seq(SortedVar("x", "String"), SortedVar("y", "String"), SortedVar("z", "String")),
//    Implies(And(Apply("class", Seq("x")), And(Apply("class", Seq("y")), Apply("class", Seq("z")))),
//      And(Apply("instance-of", Seq("p1", "x")),
//        And(Apply("instance-of", Seq("p1", "y")),
//          Apply("instance-of", Seq("p1", "z"))))
//    )))
//
//  solver.addCommands(Seq(p1, p2, p3))
//  solver.addCommands(distinct)
//  solver.addCommands(vars)
//  solver.addCommand(exists)
//  solver.addCommands(Seq(CheckSat, GetModel))
//  solver.execute()
}