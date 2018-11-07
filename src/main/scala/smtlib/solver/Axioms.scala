package smtlib.solver

import smtlib.SMTLibScript
import smtlib.syntax._
import smtlib.syntax.Implicit._

object Axioms {
  val pathDatatype = DeclareDatatype("Path", ConstructorDatatype(Seq(
    ConstructorDec("var", Seq(SelectorDec("id", "String"))),
    ConstructorDec("pth", Seq(
      SelectorDec("obj", "Path"),
      SelectorDec("field", "String")
    ))
  )))

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


  def asSMTLib: SMTLibScript = SMTLibScript(
    Seq(pathDatatype) ++
    pathEqAxioms ++
    instanceOfAxioms ++
    instantiatedByAxioms ++
    substAxioms
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

object AxiomsTest extends App {
  val solver = new Z3Solver(Axioms.asSMTLib)

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
}