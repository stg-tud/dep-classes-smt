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
}