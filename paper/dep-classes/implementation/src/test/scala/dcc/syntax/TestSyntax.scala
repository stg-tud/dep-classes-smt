package dcc.syntax

import Implicit.{StringToId, StringTupleToType}
import dcc.types.Type
import org.scalatest.funsuite.AnyFunSuite

class TestSyntax extends AnyFunSuite{
  test("Syntax Construction") {
    // Paths
    Id(Symbol("x"))
    FieldPath(Id(Symbol("y")), Id(Symbol("f")))
    FieldPath(FieldPath(Id(Symbol("y")), Id(Symbol("f"))), Id(Symbol("f")))

    // Expressions
    FieldAccess("x", "f") // x.f
    FieldAccess(FieldAccess("x", "f"), "g") //x.f.g
    MethodCall("m", "x") // m(x)
    MethodCall("m", FieldAccess("x", "f")) // m(x.f)
    ObjectConstruction("Class", List( // new Class
        ("arg1", "x")                                        // arg1 = x
      , ("arg2", FieldAccess(FieldAccess("x", "f"), "g"))    // arg2 = x.f.g
      , ("arg3", MethodCall("m", FieldAccess("x", "f")))))   // arg3 = m(x.f)

    // Constraints
    PathEquivalence("x", "y")
    PathEquivalence(FieldPath("x", "f"), "y")
    PathEquivalence(FieldPath(FieldPath("x", "f"), "g"), "y")
    val cs: Set[Constraint] = Set(
      PathEquivalence("x", "y"),
      PathEquivalence(FieldPath("x", "f"), "y"),
      PathEquivalence(FieldPath(FieldPath("x", "f"), "g"), "y"))

    // Types
    Type("x", cs)
    Type("y", Set(PathEquivalence("x", "y"), PathEquivalence(FieldPath("x", "f"), "y")))

    // Declarations
    ConstraintEntailment("x", List(PathEquivalence(FieldPath("x", "f"), "y"), PathEquivalence(FieldPath("y", "f"), "x")), PathEquivalence("y", "x"))
    AbstractMethodDeclaration("m", "x", cs.toList, ("x", cs))
    MethodImplementation("m", "x", cs.toList, Type("y", Set(PathEquivalence("x", "y"), PathEquivalence(FieldPath("x", "f"), "y"))), "x")
    ConstructorDeclaration("Class", "x", cs.toList)
  }

  // Paths
  val p: Path = Id(Symbol("x"))
  val q: Path = FieldPath(Id(Symbol("y")), Id(Symbol("f")))
  val pq: Path = FieldPath(q, Id(Symbol("f")))

  // Expressions
  val varExpr: Expression = "x" // x
  val field1: Expression = FieldAccess("x", "f") // x.f
  val field2: Expression = FieldAccess(FieldAccess("x", "f"), "g") //x.f.g
  val method1: Expression = MethodCall("m", "x") // m(x)
  val method2: Expression = MethodCall("m", FieldAccess("x", "f")) // m(x.f)
  val obj: Expression = ObjectConstruction("Class", List( // new Class
      ("arg1", varExpr)   // arg1 = x
    , ("arg2", field2)    // arg2 = x.f.g
    , ("arg3", method2))) // arg3 = m(x.f)

  // Constraints
  val c1: Constraint = PathEquivalence("x", "y")
  val c2: Constraint = PathEquivalence(FieldPath("x", "f"), "y")
  val c3: Constraint = PathEquivalence(FieldPath(FieldPath("x", "f"), "g"), "y")
  val cs: Set[Constraint] = Set(c1, c2, c3)

  // Types
  val t1: Type = ("x", cs)
  val t2: Type = ("y", Set(c1, c2))

  // Declarations
  val entail: Declaration = ConstraintEntailment("x", List(PathEquivalence(FieldPath("x", "f"), "y"), PathEquivalence(FieldPath("y", "f"), "x")), PathEquivalence("y", "x"))
  val abstractMethod: Declaration = AbstractMethodDeclaration("m", "x", cs.toList, ("x", cs))
  val methodImpl: Declaration = MethodImplementation("m", "x", cs.toList, t2, "x")
  val constructor: Declaration = ConstructorDeclaration("Class", "x", cs.toList)

  test ("Pretty Printing: Paths") {
    assert(p.toString == "x")
    assert(q.toString == "y.f")
    assert(pq.toString == "y.f.f")
  }

  test ("Pretty Printing: Expressions") {
    assert(obj.toString == "new Class(arg1 ≡ x, arg2 ≡ x.f.g, arg3 ≡ m(x.f))")
  }

  test ("Pretty Printing: Constraints") {
    assert(c1.toString == "x ≡ y")
    assert(c2.toString == "x.f ≡ y")
    assert(c3.toString == "x.f.g ≡ y")
  }

  test ("Pretty Printing: Types") {
    assert(t1.toString == "[x. x ≡ y, x.f ≡ y, x.f.g ≡ y]")
    assert(t2.toString == "[y. x ≡ y, x.f ≡ y]")
  }

  test ("Pretty Printing: Declarations") {
    assert(entail.toString == "∀x. x.f ≡ y, y.f ≡ x => y ≡ x")
    assert(abstractMethod.toString == "m(x. x ≡ y, x.f ≡ y, x.f.g ≡ y): [x. x ≡ y, x.f ≡ y, x.f.g ≡ y]")
    assert(methodImpl.toString == "m(x. x ≡ y, x.f ≡ y, x.f.g ≡ y): [y. x ≡ y, x.f ≡ y] := x")
    assert(constructor.toString == "Class(x. x ≡ y, x.f ≡ y, x.f.g ≡ y)")
  }
}