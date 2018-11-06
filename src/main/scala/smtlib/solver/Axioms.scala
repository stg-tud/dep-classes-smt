package smtlib.solver

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

  // TODO: isPathEq (...) proposition
}

object AxiomsTest extends App {
  val solver = new Z3Solver

  solver.addCommand(Axioms.pathDatatype)
  solver.addCommand(Axioms.constraintDatatype)

  val p1 = DeclareConst("p1", "Path")
  val p2 = DeclareConst("p2", "Path")
  val p3 = DeclareConst("p3", "Path")

  val c1 = DeclareConst("c1", "Constraint")
  val c2 = DeclareConst("c2", "Constraint")

  val distinct = Seq(Assert(Distinct("p1", "p2")), Assert(Distinct("c1", "c2")), Assert(Distinct("p2", "p3")))

  solver.addCommands(Seq(p1, p2, p3, c1, c2) ++ distinct ++ Seq(CheckSat, GetModel))
  solver.execute()
}