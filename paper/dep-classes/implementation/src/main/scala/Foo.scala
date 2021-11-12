import dcc.entailment.{Algorithmic, EntailmentFactory, EntailmentSort, GroundPathDepthLimitEncoding, PathDepthLimitEncoding, SemanticEntailment, SimplifiedSemanticEntailment}
import dcc.program.{BooleanExpressions, NaturalNumbers}
import dcc.program.NaturalNumbers
import dcc.syntax._
import dcc.syntax.Implicit._
import dcc.syntax.Program.Program
import dcc.types.{FaithfulAdaptionChecker, Type}
import smt.smtlib.SMTLibScript
import smt.smtlib.syntax.{Apply, DefineFun, FunctionDef, SimpleSymbol, SortedVar, Term}
import smt.smtlib.theory.BoolPredefined.{And, Bool, Eq, False, Implies, Not, Or, True, Xor, Ite}

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

  val limitEnc = new PathDepthLimitEncoding(NaturalNumbers.program, debug = 3)

  // Path enumeration process
//  val vars = List("x", "y", "z")
//  val fields = List("f", "g", "h")
//  println(s"Variables: $vars")
//  println(s"Fields: $fields\n")
//  (0 to 2).foreach {
//    i =>
//      val paths = limitEnc.enumeratePaths(vars, fields, i)
//      println(s"depth-limit: $i")
//      paths.foreach(println)
//      println
//  }



  // Partial function oddities
//  val parFun: PartialFunction[Int, Int] = {
//    case i: Int if i != 0 => i+1
//  }
//
//  println((0 to 10).collect(parFun))
//
//  val parFun2: PartialFunction[Object, Int] = {
//    case _: String => 1
//    case _: BigInt => 2
//    case _: BigDecimal => 3
//  }
//
//  sealed trait T
//  case class A(x: Int) extends T
//  case class B(x: Int, y: Int) extends T
//  case class C(x: Float, y: Double) extends T
//
//  val parFun3: PartialFunction[T, Int] = {
//    case A(i) => i
//  }
//
//  val tList = List(C(0.1f, 0.2), A(1), B(1, 2), A(2), B(2, 1), A(3), B(1, 1))
//
//  println(tList.map(parFun3.isDefinedAt))
//  println(tList.filter(parFun3.isDefinedAt).map(parFun3(_)))
//  println(tList.collect(parFun3))
//
//  println(limitEnc.constructProgRules.isDefinedAt(ConstructorDeclaration("Zero", "x", Nil), Nil, 0))
//
//  println(limitEnc.constructProgRules.isDefinedAt(
//    ConstraintEntailment("x", List(InstanceOf("x", "Zero")), InstanceOf("x", "Nat")), Nil, 0
//  ))
//  println(limitEnc.constructProgRules.isDefinedAt(
//    ConstraintEntailment("x", List(InstanceOf("x", "Zero")), InstanceOf("x", "Nat")), Nil, 0
//  ))
//  println(limitEnc.constructProgRules.isDefinedAt(
//    ConstraintEntailment("x", List(InstanceOf("x", "Zero")), InstanceOf("y", "Nat")), Nil, 0
//  ))
//
//  List(ConstructorDeclaration("Zero", "x", Nil)).collect(limitEnc.constructProgRules(_, Nil, 0))




  // Quick (simple) plausibility tests for path depth limit encoding
  // unsat
  val res = limitEnc.entails(Nil, PathEquivalence("x", "x"))
  println(res)

  // unsat
  limitEnc.entails(List(PathEquivalence("y", "x")), PathEquivalence("x", "y"))

  // unsat
  limitEnc.entails(List(PathEquivalence("y", "x"), PathEquivalence("z", "x")), PathEquivalence("z", "y"))

  // sat
  limitEnc.entails(Nil, PathEquivalence("x", "y"))

  // unsat
  limitEnc.entails(List(InstanceOf("x", "Zero")), InstanceOf("x", "Nat"))

  // unsat
  limitEnc.entails(List(InstantiatedBy("x", "Zero")), InstanceOf("x", "Nat"))

  // sat
  limitEnc.entails(List(InstantiatedBy("x", "Zero")), InstanceOf("x", "Succ"))

  // unsat
  limitEnc.entails(List(InstantiatedBy("x", "Succ"), InstantiatedBy(FieldPath("x", "p"), "Zero")), InstanceOf("x", "Succ"))

  // sat
  limitEnc.entails(List(InstantiatedBy("x", "Succ"), InstantiatedBy(FieldPath("x", "p"), "Zero")), InstanceOf("x", "Zero"))
  // unsat
  limitEnc.entails(List(InstantiatedBy("x", "Succ"), InstantiatedBy(FieldPath("x", "p"), "Zero"), PathEquivalence("x", "y")), InstanceOf("y", "Nat"))


  // Prog rule generation
//  val progs1 = limitEnc.constructProgRules(ConstraintEntailment("x", List(InstanceOf("x", "Zero")), InstanceOf("x", "Nat")), List(Id(Symbol("x")), Id(Symbol("y"))), 1)
//  println(progs1.format)
//
//  val progs2 = limitEnc.constructProgRules(ConstraintEntailment("x", List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Nat")), InstanceOf("x", "Nat")), List(Id(Symbol("pth_x")), Id(Symbol("pth_y")), FieldPath(Id(Symbol("pth_x")), Id(Symbol("p")))), 1)
//  println(progs2.format)

//  val groundEnc = EntailmentFactory(EntailmentSort.GroundPathDepthLimit)(NaturalNumbers.program, 3)
  val groundEnc = new GroundPathDepthLimitEncoding(NaturalNumbers.program, 3)

  println("\n\n")
  println(groundEnc.entails(List(PathEquivalence("b", "a")), PathEquivalence("a", "b")))

//  val l = List(1, 2, 3)
//
//  val k: List[Int] = l flatMap (
//    a => l flatMap (
//      b => l map (
//        c => a+b+c
//      )
//    )
//  )
//
//  println(k)

  println("\n\n")
  println("limit encoding: · |- a=a")
  println(limitEnc.encode(Nil, PathEquivalence("a", "a")).pretty)
  println("\n---------------------------------------------------------------\n")
  println("ground encoding: · |- a=a")
  println(groundEnc.encode(Nil, PathEquivalence("a", "a")).pretty)

  println("\n\nalgorithmic system:")
  val algo = new Algorithmic(NaturalNumbers.program)

  println(s"check x::Zero |- x::Nat: ${algo.entails(
    List(InstanceOf("a", "Zero")),
    InstanceOf("a", "Nat")
  )}")

  println(True.format)
  println(True().format)
  println(SimpleSymbol("true").format)
  println(True.pretty)
  println(True().pretty)
  println(SimpleSymbol("true").pretty)

  val t: Term = Apply(SimpleSymbol("foo"), Seq(SimpleSymbol("true"), True, False))
  println(t.format)
  println(t.pretty)

  println(Not(True))
  println(Not(True).format)
  println(Not(True).pretty)

  println(And(
    Or(True, False, True),
    Implies(False, True),
    Xor(True, Eq(True, Not(True)))
  ).pretty)
  println(Ite(Not(False), False, True).pretty)
}