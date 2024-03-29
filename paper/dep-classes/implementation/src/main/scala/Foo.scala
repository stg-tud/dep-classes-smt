import collection.{Edge, InheritanceRelation}
import dcc.DCC
import dcc.DCC.{Heap, HeapObj}
import dcc.entailment.algorithmic.AlgorithmicSystem
import dcc.entailment.{EntailmentFactory, EntailmentSort, GroundPathDepthLimitEncoding, PathDepthLimitEncoding, SemanticEntailment, SimplifiedSemanticEntailment}
import dcc.interpreter.Interpreter
import dcc.program.{Arithmetic, BooleanExpressions, Lt, NaturalNumbers, NumericExpressions, WitnessMethodCallFails, WitnessMethodCallIncorrectProgram, WitnessMethodCallSucceeds, WitnessMethodCallTest, WitnessMethodRecursiveProperty, WitnessMethodSimplifiedRecursiveProperty, WitnessPathDependent}
import dcc.syntax._
import dcc.syntax.Implicit._
import dcc.syntax.Program.Program
import dcc.types.{InferenceChecker, SomeInferenceChecker, Type}
import com.github.gnush.smt.smtlib.SMTLibScript
import com.github.gnush.smt.smtlib.syntax.{Apply, DefineFun, FunctionDef, SimpleSymbol, SortedVar, Term}
import com.github.gnush.smt.smtlib.theory.BoolPredefined.{And, Bool, Eq, False, Implies, Ite, Not, Or, True, Xor}

object Foo extends App {
  val limitEnc = new PathDepthLimitEncoding(NaturalNumbers.program, debug = 3)
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

  // Prog rule generation
//  val progs1 = limitEnc.constructProgRules(ConstraintEntailment("x", List(InstanceOf("x", "Zero")), InstanceOf("x", "Nat")), List(Id(Symbol("x")), Id(Symbol("y"))), 1)
//  println(progs1.format)
//
//  val progs2 = limitEnc.constructProgRules(ConstraintEntailment("x", List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Nat")), InstanceOf("x", "Nat")), List(Id(Symbol("pth_x")), Id(Symbol("pth_y")), FieldPath(Id(Symbol("pth_x")), Id(Symbol("p")))), 1)
//  println(progs2.format)

//  val groundEnc = EntailmentFactory(EntailmentSort.GroundPathDepthLimit)(NaturalNumbers.program, 3)
  val groundEnc = new GroundPathDepthLimitEncoding(NaturalNumbers.program, 3)

  println("\n\nalgorithmic system:")
  val algo = new AlgorithmicSystem(NaturalNumbers.program)

  println("\n\n\n")

  println("\n\n\n") // ↓ these are cool examples to put into a test
  val chk = new InferenceChecker(NaturalNumbers.program, EntailmentSort.GroundPathDepthLimit, debug=2)
  val algoChk = new InferenceChecker(NaturalNumbers.program, EntailmentSort.AlgorithmicFix1)
  val xZeroContext = List(InstanceOf("x", "Zero"))
  val xOneContext = List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Zero"))
  val xTwoContext = List(InstanceOf("x", "Succ"), InstanceOf(FieldPath("x", "p"), "Succ"),
                         InstanceOf("y", "Succ"), InstanceOf(FieldPath("y", "p"),"Zero"),
                         PathEquivalence("y", FieldPath("x", "p")))

  // This is only useful with the extended Nat program
//  println(chk.typeOf(xZeroContext, ObjectConstruction("Succ", List(("p", "x"), ("flag", "x"))))) // This should work. It actually does, but does so by accident. (see previous error with constructors with too many arguments)
//  println(chk.typeOf(xOneContext, ObjectConstruction("Succ", List(("p", "x"), ("flag", "x"))))) // This should fail, as usefulProperty is required by the constructor to be a Zero. But it doesn't as of the previous error.

  val family: Program = List(
    ConstructorDeclaration("Node", "x", Nil),
    ConstructorDeclaration("Edge", "x", List(InstanceOf(FieldPath("x", "n1"), "Node"), InstanceOf(FieldPath("x", "n2"), "Node"))),
    ConstructorDeclaration("Graph", "x", List(InstanceOf(FieldPath("x", "node"), "Node"), InstanceOf(FieldPath("x", "edge"), "Edge")))
  )

  val family2: Program = List(
    ConstructorDeclaration("Node", "x", Nil),
    ConstructorDeclaration("Edge", "x", List(InstanceOf(FieldPath("x", "n1"), "Node"), InstanceOf(FieldPath("x", "n2"), "Node"))),
    ConstructorDeclaration("OnOffNode", "x", Nil),
    ConstructorDeclaration("OnOffEdge", "x", List(InstanceOf(FieldPath("x", "n1"), "Node"), InstanceOf(FieldPath("x", "n2"), "Node"), InstanceOf(FieldPath("x", "enabled"), "Boolean"))),
    ConstraintEntailment("x", List(InstanceOf("x", "OnOffNode"), InstanceOf(FieldPath("x", "n1"), "Node"), InstanceOf(FieldPath("x", "n2"), "Node")), InstanceOf("x", "Node")),
    ConstraintEntailment("x", List(InstanceOf("x", "OnOffEdge"), InstanceOf(FieldPath("x", "enabled"), "Boolean")), InstanceOf("x", "Edge")),
    MethodImplementation("touches", "x", List(), Type("y", Set(InstanceOf("y", "Boolean"))), "true")
  )

  println("\n\n\n approx:")
  chk.generateTypeApproximationSet(Type("x", Set(InstanceOf("x", "Nat"))))

//  println("\n\nAST:")
//  NumericExpressions.program.foreach(println)
//
//  val astChecker = new InferenceChecker(NumericExpressions.program, EntailmentSort.GroundPathDepthLimit)
//  val astInterp = new Interpreter(NumericExpressions.program, EntailmentSort.GroundPathDepthLimit)
//
//  val heap: Heap = Map(
//    Id(Symbol("zero")) -> (Id(Symbol("Zero")), List()),
//    Id(Symbol("one")) -> (Id(Symbol("Succ")), List((Id(Symbol("p")), Id(Symbol("zero"))))),
//    Id(Symbol("two")) -> (Id(Symbol("Succ")), List((Id(Symbol("p")), Id(Symbol("one"))))))

//  println(s"AST typechecks: ${astChecker.typeCheck}")
//  println(s"eval(new |2|): ${astInterp.execute(heap, MethodCall("eval", ObjectConstruction("Lit", List(("value", "two")))))}")

//  val arith = new InferenceChecker(Arithmetic.program, EntailmentSort.GroundPathDepthLimit, debug=2)
//  println(arith.typeCheck)

// TODO: typechecker
//   - construct inheritance graph from program (entailment declarations)
//   - use inheritance graph to minimize the needed class instance checks
  println("\nLt:")
  val lt = new InferenceChecker(Lt.program, EntailmentSort.GroundPathDepthLimit, debug = 2)
//  println(lt.typeCheck)
//  entailment: a.n1 :: Nat, a.n2 :: Zero |- a :: Lt
//  type check for new Lt(n1 ≡ a.n1, n2 ≡ a.n2, b ≡ new False()) failed with List(Class Lt: couldn't assign a type to field n2, because variable 'a' is not available in context a.n1 :: Nat, a.n2 :: Zero, Class Lt: couldn't assign a type to field n1, because variable 'a' is not available in context a.n1 :: Nat, a.n2 :: Zero)
//  false

  val ltGraph = lt.inheritanceGraph
  println("\nlt inheritance graph:\nnodes:")
  ltGraph.nodes.foreach(node => println(s"\t${node.cls}"))
  println("edges:")
  ltGraph.edges.foreach {
    case Edge(lhs, rhs, InheritanceRelation.Subtype) => println(s"\t${lhs.cls} <: ${rhs.cls}")
    case Edge(lhs, rhs, InheritanceRelation.Supertype) => println(s"\t${lhs.cls} :> ${rhs.cls}")
  }

  val newA = ObjectConstruction("A", Nil)
  val newB = ObjectConstruction("B", Nil)

  val witnessFail = new InferenceChecker(WitnessMethodCallFails.program, EntailmentSort.GroundPathDepthLimit)
  println("\nwitness method call fails:")
  WitnessMethodCallFails.program.foreach(println)
  println()
  println(s"program typechecks: ${witnessFail.typeCheck}")
  println(s"typecheck new Witness(choice=A): ${witnessFail.typeOf(Nil, ObjectConstruction("Witness", List(("choice", ObjectConstruction("A", Nil)))))}")
  println(s"typecheck property(new A): ${witnessFail.typeOf(Nil, MethodCall("property", ObjectConstruction("A", Nil)))}")
  println(s"typecheck m(property(new A)): ${witnessFail.typeOf(Nil, MethodCall("m", MethodCall("property", ObjectConstruction("A", Nil))))}")
  println(s"typecheck m(property(new B)): ${witnessFail.typeOf(Nil, MethodCall("m", MethodCall("property", ObjectConstruction("B", Nil))))}")
  println("\n")

  val witnessSuccess = new InferenceChecker(WitnessMethodCallSucceeds.program, EntailmentSort.GroundPathDepthLimit)
  println("\nwitness method call succeeds:")
  WitnessMethodCallSucceeds.program.foreach(println)
  println()
  println(s"program typechecks: ${witnessSuccess.typeCheck}")
  println(s"typecheck new Witness(choice=A): ${witnessSuccess.typeOf(Nil, ObjectConstruction("Witness", List(("choice", ObjectConstruction("A", Nil)))))}")
  println(s"typecheck property(new A): ${witnessSuccess.typeOf(Nil, MethodCall("property", ObjectConstruction("A", Nil)))}")
  println(s"typecheck m(property(new A)): ${witnessSuccess.typeOf(Nil, MethodCall("m", MethodCall("property", ObjectConstruction("A", Nil))))}")
  println(s"typecheck m(property(new B)): ${witnessSuccess.typeOf(Nil, MethodCall("m", MethodCall("property", ObjectConstruction("B", Nil))))}")
  println("\n")

  // TODO: smt encoding errors, why?
  val abstractWitness = new InferenceChecker(WitnessMethodCallTest.program, EntailmentSort.AlgorithmicFix1, debug=2)
  WitnessMethodCallTest.program.foreach(println)
  println()
  println("\nwitness method call test:")
  println(s"program typechecks: ${abstractWitness.typeCheck}")
  println(s"typecheck new WitnessT: ${abstractWitness.typeOf(Nil, ObjectConstruction("WitnessT", Nil))}")
  println(s"typecheck property(new A): ${abstractWitness.typeOf(Nil, MethodCall("property", ObjectConstruction("A", Nil)))}")
  println(s"typecheck m(property(new A)): ${abstractWitness.typeOf(Nil, MethodCall("m", MethodCall("property", ObjectConstruction("A", Nil))))}")
  println(s"typecheck m(property(new B)): ${abstractWitness.typeOf(Nil, MethodCall("m", MethodCall("property", ObjectConstruction("B", Nil))))}")
  println("\n")

  val incorrectWitness = new InferenceChecker(WitnessMethodCallIncorrectProgram.program, EntailmentSort.GroundPathDepthLimit)
  println("\nwitness method call incorrect program:")
  WitnessMethodCallIncorrectProgram.program.foreach(println)
  println()
  println(s"program typechecks: ${incorrectWitness.typeCheck}")
  println(s"typecheck new Witness(choice=A): ${incorrectWitness.typeOf(Nil, ObjectConstruction("Witness", List(("choice", ObjectConstruction("A", Nil)))))}")
  println(s"typecheck property(new A): ${incorrectWitness.typeOf(Nil, MethodCall("property", ObjectConstruction("A", Nil)))}")
  println(s"typecheck m(property(new A)): ${incorrectWitness.typeOf(Nil, MethodCall("m", MethodCall("property", ObjectConstruction("A", Nil))))}")
  println(s"typecheck m(property(new B)): ${incorrectWitness.typeOf(Nil, MethodCall("m", MethodCall("property", ObjectConstruction("B", Nil))))}")

  val newNil = ObjectConstruction("Nil", Nil)
  def newCons(hd: Expression, tl: Expression) = ObjectConstruction("Cons", List(("hd", hd), ("tl", tl)))

//  val recursiveProperty = new InferenceChecker(WitnessMethodRecursiveProperty.program, EntailmentSort.GroundPathDepthLimit)
//  println("\nwitness method recursive property:")
//  WitnessMethodRecursiveProperty.program.foreach(println)
//  println()
//  //println(s"program typechecks: ${recursiveProperty.typeCheck}")
//  println(s"typecheck new Witness(choice=A): ${recursiveProperty.typeOf(Nil, ObjectConstruction("Witness", List(("choice", newA))))}")
//  println(s"typecheck property(Nil): ${recursiveProperty.typeOf(Nil, MethodCall("property",newNil))}")
//  println(s"typecheck m(property(Nil)): ${recursiveProperty.typeOf(Nil, MethodCall("m", MethodCall("property", newNil)))}")
//  println(s"typecheck m(property(A::Nil)): ${recursiveProperty.typeOf(Nil, MethodCall("m", MethodCall("property", newCons(newA, newNil))))}")
//  println(s"typecheck m(property(B::Nil)): ${recursiveProperty.typeOf(Nil, MethodCall("m", MethodCall("property", newCons(newB, newNil))))}")
//  println(s"typecheck m(property(B::A::Nil)): ${recursiveProperty.typeOf(Nil, MethodCall("m", MethodCall("property", newCons(newB, newCons(newA, newNil)))))}")
//  println(s"typecheck m(property(A::B::Nil)): ${recursiveProperty.typeOf(Nil, MethodCall("m", MethodCall("property", newCons(newA, newCons(newB, newNil)))))}")
//  println("\n")

  def nilBox(data: Expression) = ObjectConstruction("Nil", List(("data", data)))
  def consBox(tl: Expression) = ObjectConstruction("Cons", List(("tl", tl)))

  val simplifiedRecursiveProperty = new InferenceChecker(WitnessMethodSimplifiedRecursiveProperty.program, EntailmentSort.AlgorithmicFix1)
  println("\nwitness method simplified recursive property:")
  WitnessMethodSimplifiedRecursiveProperty.program.foreach(println)
  println()
  //println(s"program typechecks: ${recursiveProperty.typeCheck}")
  println(s"typecheck new Witness(choice=A): ${simplifiedRecursiveProperty.typeOf(Nil, ObjectConstruction("Witness", List(("choice", newA))))}")
  println(s"typecheck property(Nil(A)): ${simplifiedRecursiveProperty.typeOf(Nil, MethodCall("property", nilBox(newA)))}")
  println(s"typecheck property(Cons(Nil(A))): ${simplifiedRecursiveProperty.typeOf(Nil, MethodCall("property", consBox(nilBox(newA))))}")
  println(s"typecheck m(property(Nil(A))): ${simplifiedRecursiveProperty.typeOf(Nil, MethodCall("m", MethodCall("property", nilBox(newA))))}")
  println(s"typecheck m(property(Nil(B))): ${simplifiedRecursiveProperty.typeOf(Nil, MethodCall("m", MethodCall("property", nilBox(newB))))}")
  println(s"typecheck m(property(Cons(Nil(A)))): ${simplifiedRecursiveProperty.typeOf(Nil, MethodCall("m", MethodCall("property", consBox(nilBox(newA)))))}")
  println(s"typecheck m(property(Cons(Nil(B)))): ${simplifiedRecursiveProperty.typeOf(Nil, MethodCall("m", MethodCall("property", consBox(nilBox(newB)))))}")
  println("\n")

  val pathDependentWitness = new InferenceChecker(WitnessPathDependent.program, EntailmentSort.GroundPathDepthLimit)
  println("\nwitness method path dependent type:")
  WitnessPathDependent.program.foreach(println)
  println()
//  println(s"program typechecks: ${pathDependentWitness.typeCheck}") // it does, takes some time
  println(s"typecheck new Witness(choice=A): ${pathDependentWitness.typeOf(Nil, ObjectConstruction("Witness", List(("choice", newA))))}")
  val pairMatch = ObjectConstruction("Pair", List(("witness", ObjectConstruction("Witness", List(("choice", newA)))), ("param", newA)))
  val pairMismatch = ObjectConstruction("Pair", List(("witness", ObjectConstruction("Witness", List(("choice", newA)))), ("param", newB)))
  println(s"typecheck new Pair(witness=new Witness(choice=A), param=A): ${pathDependentWitness.typeOf(Nil, pairMatch)}")
  println(s"typecheck new Pair(witness=new Witness(choice=A), param=B): ${pathDependentWitness.typeOf(Nil, pairMismatch)}")
  println(s"typecheck m(Witness(A), A): ${pathDependentWitness.typeOf(Nil, MethodCall("m", pairMatch))}")
  println(s"typecheck m(Witness(A), B): ${pathDependentWitness.typeOf(Nil, MethodCall("m", pairMismatch))}")
  println(s"typecheck x::A |- m(Witness(x), x): ${pathDependentWitness.typeOf(List(InstanceOf("foo", "A")), MethodCall("m", ObjectConstruction("Pair", List(("witness", ObjectConstruction("Witness", List(("choice", "foo")))), ("param", "foo")))))}")
}