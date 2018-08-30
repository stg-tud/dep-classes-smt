package syntax

import Util._

class TestSyntax {
  // Paths
  val p: Path = Id('x)
  val q: Path = FieldPath(Id('y), Id('f))
  val pq: Path = FieldPath(q, Id('f))

  // Expressions
  val varExpr: Expression = 'x // x
  val field1: Expression = FieldAccess('x, 'f) // x.f
  val field2: Expression = FieldAccess(FieldAccess('x, 'f), 'g) //x.f.g
  val method1: Expression = MethodCall('m, 'x) // m(x)
  val method2: Expression = MethodCall('m, FieldAccess('x, 'f)) // m(x.f)
  val obj: Expression = ObjectConstruction(Id('Class), List( // new Class
    ('arg1, varExpr)                                     // arg1 = x
  , ('arg2, field2),                                     // arg2 = x.f.g
    ('arg3, method2)))                                   // arg3 = m(x.f)

  // Declarations
  val c1: Constraint = PathEquivalence('x, 'y)
  val c2: Constraint = PathEquivalence(FieldPath('x, 'f), 'y)
  val c3: Constraint = PathEquivalence(FieldPath(FieldPath('x, 'f), 'g), 'y)
  val cs: List[Constraint] = List(c1, c2, c3)

  val entail = ConstraintEntailment('x, List(PathEquivalence(FieldPath('x, 'f), 'y), PathEquivalence(FieldPath('y, 'f), 'x)), PathEquivalence('y, 'x))
  val abstractMethod = AbstractMethodDeclaration('m, 'x, cs, ('x, cs))
  val methodImpl = MethodImplementation('m, 'x, cs, ('x, cs), 'x)
  val constructor = ConstructorDeclaration('Class, 'x, cs)
}