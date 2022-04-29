package dcc.program

import dcc.syntax.{AbstractMethodDeclaration, ConstraintEntailment, ConstructorDeclaration, InstanceOf, MethodImplementation, ObjectConstruction}
import dcc.syntax.Program.Program
import dcc.syntax.Implicit.StringToId
import dcc.types.Type

object BooleanExpressions {
  val program: Program = List(
    ConstructorDeclaration("True", "b", Nil),
    ConstructorDeclaration("False", "b", Nil),
    ConstraintEntailment("b", List(InstanceOf("b", "True")), InstanceOf("b", "Bool")),
    ConstraintEntailment("b", List(InstanceOf("b", "False")), InstanceOf("b", "Bool")),
    AbstractMethodDeclaration("not", "b", List(InstanceOf("b", "Bool")), Type("y", Set(InstanceOf("y", "Bool")))),
    MethodImplementation("not", "b", List(InstanceOf("b", "False")), Type("y", Set(InstanceOf("y", "Bool"))),
      ObjectConstruction("True", Nil)),
    MethodImplementation("not", "b", List(InstanceOf("b", "True")), Type("y", Set(InstanceOf("y", "Bool"))),
      ObjectConstruction("False", Nil))
  )
}
