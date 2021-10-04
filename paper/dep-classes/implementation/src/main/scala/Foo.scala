import dcc.entailment.{EntailmentSort, PathDepthLimitEncoding, SemanticEntailment, SimplifiedSemanticEntailment}
import dcc.program.{BooleanExpressions, NaturalNumbers}
import dcc.program.NaturalNumbers
import dcc.syntax._
import dcc.syntax.Implicit._
import dcc.syntax.Program.Program
import dcc.types.{FaithfulAdaptionChecker, Type}
import smt.smtlib.SMTLibScript
import smt.smtlib.syntax.{Apply, DefineFun, FunctionDef, SimpleSymbol, SortedVar}
import smt.smtlib.theory.BoolPredefined.{And, Bool, Eq, False, Or, True}

object Foo extends App {
  // TODO: is there a way to force the program if the entailment to be the same as the program of the checker/interpreter?
  val sem3 = new SemanticEntailment(NaturalNumbers.program)
  val newChecker  = new FaithfulAdaptionChecker(NaturalNumbers.program, EntailmentSort.SimplifiedSemantic)

  val program: Program = List(
    ConstructorDeclaration("Zero", "x", Nil),
    ConstraintEntailment("x", List(InstanceOf("x", "Zero")), InstanceOf("x", "Nat")),
    ConstructorDeclaration("Succ", "x", List(InstanceOf(FieldPath("x", "p"), "Nat"))),
    ConstraintEntailment("x", List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Nat")), InstanceOf("x", "Nat")),
    AbstractMethodDeclaration("prev", "x", List(InstanceOf("x", "Nat")), Type("y", List(InstanceOf("y", "Nat")))),
    MethodImplementation("prev", "x", List(InstanceOf("x", "Zero")), Type("y", List(InstanceOf("y", "Nat"))),
      "x"),
    MethodImplementation("prev", "x", List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Nat")), Type("y", List(InstanceOf("y", "Nat"))),
      FieldAccess("x", "p"))
  )


//  println(newChecker.typeCheck)

//  import smt.smtlib.syntax.Implicit._
//  val Path = SimpleSymbol("Path")
//  val path_equivalence = "path-equivalence"
//
//  println(DefineFun(FunctionDef(
//    path_equivalence,
//    Seq(
//      SortedVar("path-p", Path),
//      SortedVar("path-q", Path)
//    ),
//    Bool,
//    Or(
//      Eq("path-p", "path-q"),
//      Or(
//        And(
//          Eq("path-p", Apply("var", Seq("X"))),
//          Eq("path-q", Apply("pth", Seq(Apply("var", Seq("Y")), "F")))
//        ),
//        Or(
//          And(
//            Eq("path-q", Apply("pth", Seq(Apply("var", Seq("Y")), "F"))),
//            Eq("path-p", Apply("var", Seq("X")))
//          ),
//          Or(
//            And(
//              Eq("path-p", Apply("var", Seq("X"))),
//              Eq("path-p", Apply("var", Seq("X")))
//            ),
//            And(
//              Eq("path-q", Apply("pth", Seq(Apply("var", Seq("Y")), "F"))),
//              Eq("path-q", Apply("pth", Seq(Apply("var", Seq("Y")), "F")))
//            )
//          )
//        )
//      )
//    )
//  )).format)

//  val boolChecker = new FaithfulAdaptionChecker(BooleanExpressions.program)
//  println(boolChecker.typeCheck)

  val sim = new SimplifiedSemanticEntailment(NaturalNumbers.program, debug = 3)
//  println(sim.entails(List(), PathEquivalence("q", "q")))
//  println(sim.entails(List(PathEquivalence("a", "b")), PathEquivalence("b", "a")))

  val limitEnc = new PathDepthLimitEncoding(NaturalNumbers.program, debug = 3)

  val vars = List("x", "y", "z")
  val fields = List("f", "g", "h")
  println(s"Variables: $vars")
  println(s"Fields: $fields\n")
  (0 to 2).foreach {
    i =>
      val paths = limitEnc.enumeratePaths(vars, fields, i)
      println(s"depth-limit: $i")
      paths.foreach(println)
      println
  }

  val parFun: PartialFunction[Int, Int] = {
    case i: Int if i != 0 => i+1
  }

  println((0 to 10).collect(parFun))

  val parFun2: PartialFunction[Object, Int] = {
    case _: String => 1
    case _: BigInt => 2
    case _: BigDecimal => 3
  }

  sealed trait T
  case class A(x: Int) extends T
  case class B(x: Int, y: Int) extends T
  case class C(x: Float, y: Double) extends T

  val parFun3: PartialFunction[T, Int] = {
    case A(i) => i
  }

  val tList = List(C(0.1f, 0.2), A(1), B(1, 2), A(2), B(2, 1), A(3), B(1, 1))

  println(tList.map(parFun3.isDefinedAt))
  println(tList.filter(parFun3.isDefinedAt).map(parFun3(_)))
  println(tList.collect(parFun3))

  println(limitEnc.constructProgRule.isDefinedAt(ConstructorDeclaration("Zero", "x", Nil), true))

  println(limitEnc.constructProgRule.isDefinedAt(
    ConstraintEntailment("x", List(InstanceOf("x", "Zero")), InstanceOf("x", "Nat")), true
  ))
  println(limitEnc.constructProgRule.isDefinedAt(
    ConstraintEntailment("x", List(InstanceOf("x", "Zero")), InstanceOf("x", "Nat")), false
  ))
  println(limitEnc.constructProgRule.isDefinedAt(
    ConstraintEntailment("x", List(InstanceOf("x", "Zero")), InstanceOf("y", "Nat")), true
  ))

//  List(ConstructorDeclaration("Zero", "x", Nil)).collect(limitEnc.constructProgRule(_, false))

  // TODO: fix ambiguous name errors (variables are also paths)
  // TODO: add substitution result paths into quantifier list (subst rules)
  // TODO: in prog rule generation, do not perform the substitution prior (p25.p)
  val res = limitEnc.entails(Nil, PathEquivalence("x", "x"))
  println(res)
}