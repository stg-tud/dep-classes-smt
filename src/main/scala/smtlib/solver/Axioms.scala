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

  val pathEqDatatype = DeclareDatatype("PathEq", ConstructorDatatype(Seq(
    ConstructorDec("PathEq", Seq(
      SelectorDec("left", "Path"),
      SelectorDec("right", "Path")
    ))
  )))

  val instanceOfDatatype = DeclareDatatype("InstanceOf", ConstructorDatatype(Seq(
    ConstructorDec("InstanceOf", Seq(
      SelectorDec("path", "Path"),
      SelectorDec("class", "String")
    ))
  )))

  val instantiatedByDatatype = DeclareDatatype("InstantiatedBy", ConstructorDatatype(Seq(
    ConstructorDec("InstantiatedBy", Seq(
      SelectorDec("path", "Path"),
      SelectorDec("class", "String")
    ))
  )))

  // TODO: body must be boolean
  val pathEqRefl = Assert(Forall(Seq(SortedVar("p", "Path")), Apply("PathEq", Seq("p", "p"))))

  def asSMTLib: SMTLibScript = SMTLibScript(Seq(pathDatatype, pathEqDatatype, instanceOfDatatype, instantiatedByDatatype, pathEqRefl))

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

  val paths = Seq(
    Assert(Eq("p1", Apply("var", Seq(SMTLibString("x"))))),
    Assert(Eq("p2", Apply("var", Seq(SMTLibString("y"))))),
    Assert(Eq("p3", Apply("var", Seq(SMTLibString("z")))))
  )

  val c1 = DeclareConst("c1", "PathEq")
  val c2 = DeclareConst("c2", "PathEq")

  val distinct = Seq(Assert(Distinct("p1", "p2")), Assert(Distinct("p2", "p3")), Assert(Distinct("p1", "p3")), Assert(Distinct("c1", "c2")))

  val cons = Seq(Assert(Eq(Apply("PathEq", Seq("p1", "p2")), "c1")), Assert(Eq(Apply("PathEq", Seq("p2", "p3")), "c2")))

  solver.addCommands(Seq(p1, p2, p3, c1, c2) ++ distinct ++ paths ++ cons ++ Seq(CheckSat, GetModel))
  solver.execute()
}