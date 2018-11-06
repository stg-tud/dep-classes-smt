package smtlib.solver

import smtlib.SMTLibScript
import smtlib.syntax._
import smtlib.syntax.Implicit._

object Axioms {
  val pathDatatype = DeclareDatatype("Path", ConstructorDatatype(Seq(
    ConstructorDec("var", Seq(SelectorDec("id", "String"))),
    ConstructorDec("cons", Seq(
      SelectorDec("obj", "Path"),
      SelectorDec("field", "String")
    ))
  )))

  val pathEq = DeclareFun("pathEq", Seq("Path", "Path"), Bool)
  val instanceOf = DeclareFun("instanceOf", Seq("Path", "String"), Bool)
  val instantiatedBy = DeclareFun("instantiatedBy", Seq("Path", "String"), Bool)

  val pathEqRefl = Assert(Forall(Seq(SortedVar("p", "Path")), Apply("pathEq", Seq("p", "p"))))
  val pathEqSym = Assert(Forall(Seq(SortedVar("p1", "Path"), SortedVar("p2", "Path")),
                          Implies(Apply("pathEq", Seq("p1", "p2")), Apply("pathEq", Seq("p2", "p1")))))
  val pathEqTrans = Assert(Forall(Seq(SortedVar("p1", "Path"), SortedVar("p2", "Path"), SortedVar("p3", "Path")),
                          Implies(And(Apply("pathEq", Seq("p1", "p2")), Apply("pathEq", Seq("p2", "p3"))), Apply("pathEq", Seq("p1", "p3")))))

  def asSMTLib: SMTLibScript = SMTLibScript(Seq(pathDatatype, pathEq, instanceOf, instantiatedBy, pathEqRefl, pathEqSym, pathEqTrans))

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
  val solver = new Z3Solver

  solver.addScript(Axioms.asSMTLib)

  val p1 = DeclareConst("p1", "Path")
  val p2 = DeclareConst("p2", "Path")
  val p3 = DeclareConst("p3", "Path")

  val distinct = Seq(Assert(Distinct("p1", "p2")), Assert(Distinct("p2", "p3")), Assert(Distinct("p1", "p3")))

  val paths = Seq(
    Assert(Eq("p1", Apply("var", Seq(SMTLibString("x"))))),
    Assert(Eq("p2", Apply("var", Seq(SMTLibString("y"))))),
    Assert(Eq("p3", Apply("var", Seq(SMTLibString("z")))))
  )

  val p1EqP2 = Apply("pathEq", Seq("p1", "p2"))
  val p3EqP2 = Apply("pathEq", Seq("p3", "p2"))
  val p1EqP3 = Apply("pathEq", Seq("p1", "p3"))
  val goal = Assert(Implies(And(p1EqP2, p3EqP2), p1EqP3))

  solver.addCommands(Seq(p1, p2, p3) ++ distinct ++ paths ++ Seq(goal, CheckSat, GetModel))
  solver.execute()
}