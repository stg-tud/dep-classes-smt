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

  val constraintDatatype = DeclareDatatype(SimpleSymbol("Constraint"), ConstructorDatatype(Seq(
    ConstructorDec(SimpleSymbol("pathEq"), Seq(SelectorDec(SimpleSymbol("pLeft"), SimpleSymbol("Path")),
                                               SelectorDec(SimpleSymbol("pRight"), SimpleSymbol("Path")))),
    ConstructorDec(SimpleSymbol("instanceOf"), Seq(SelectorDec(SimpleSymbol("instance"), SimpleSymbol("Path")),
                                                   SelectorDec(SimpleSymbol("class"), SimpleSymbol("String")))),
    ConstructorDec(SimpleSymbol("instantiatedBy"), Seq(SelectorDec(SimpleSymbol("obj"), SimpleSymbol("Path")),
                                                       SelectorDec(SimpleSymbol("classname"), SimpleSymbol("String"))))
  )))

  val isPathEqProp = DeclareFun(SimpleSymbol("isPathEq"), Seq(SimpleSymbol("Constraint")), Bool)
//  val isPathEqProp = DefineFun(FunctionDef(SimpleSymbol("isPathEq"), Seq(SortedVar(SimpleSymbol("c"), SimpleSymbol("Constraint"))), Bool,
//    Exists(Seq(SortedVar(SimpleSymbol("p1"), SimpleSymbol("Path")), SortedVar(SimpleSymbol("p2"), SimpleSymbol("Path"))),
//      Eq("c", Apply(SimpleSymbol("pathEq"), Seq(SimpleSymbol("p1"), SimpleSymbol("p2")))))))
  val isInstanceOfProp = DeclareFun(SimpleSymbol("isInstanceOf"), Seq(SimpleSymbol("Constraint")), Bool)
  val isInstantiatedByProp = DeclareFun(SimpleSymbol("isInstantiatedBy"), Seq(SimpleSymbol("Constraint")), Bool)

  def asSMTLib: SMTLibScript = SMTLibScript(Seq(pathDatatype, constraintDatatype, isPathEqProp, isInstanceOfProp, isInstantiatedByProp))
}

object AxiomsTest extends App {
  val solver = new Z3Solver

  solver.addScript(Axioms.asSMTLib)

  val p1 = DeclareConst("p1", "Path")
  val p2 = DeclareConst("p2", "Path")
  val p3 = DeclareConst("p3", "Path")

  val c1 = DeclareConst("c1", "Constraint")
  val c2 = DeclareConst("c2", "Constraint")

  val distinct = Seq(Assert(Distinct("p1", "p2")), Assert(Distinct("p2", "p3")), Assert(Distinct("p1", "p3")), Assert(Distinct("c1", "c2")))

  val consArePathEq = Seq(Assert(Apply("isPathEq", Seq("c1"))), Assert(Apply("isPathEq", Seq("c2"))))
  val cons = Seq(Assert(Eq(Apply("pathEq", Seq("p1", "p2")), "c1")), Assert(Eq(Apply("pathEq", Seq("p2", "p3")), "c2")))

  solver.addCommands(Seq(p1, p2, p3, c1, c2) ++ distinct ++ cons ++ consArePathEq ++ Seq(CheckSat, GetModel))
  solver.execute()
}