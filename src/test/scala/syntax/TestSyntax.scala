package syntax

import jdk.nashorn.internal.codegen.CompilerConstants.FieldAccess

class TestSyntax {
  // Paths
  val p: Path = Id('x)
  val q: Path = FieldPath(Id('y), Id('f))
  val pq: Path = FieldPath(q, Id('f))

  // Expressions
  val varExpr: Expression = Id('x) // x
  val field1: Expression = FieldAccess(Id('x), Id('f)) // x.f
  val field2: Expression = FieldAccess(FieldAccess(Id('x), Id('f)), Id('g)) //x.f.g
  val method1: Expression = MethodCall(Id('m), Id('x)) // m(x)
  val method2: Expression = MethodCall(Id('m), FieldAccess(Id('x), Id('f))) // m(x.f)
  val obj: Expression = ObjectConstruction(Id('Class), List( // new Class
    (Id('arg1), varExpr)                                     // arg1 = x
  , (Id('arg2), field2),                                     // arg2 = x.f.g
    (Id('arg3), method2)))                                   // arg3 = m(x.f)
}