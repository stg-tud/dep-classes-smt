package dcc.program

import dcc.syntax
import dcc.syntax.{AbstractMethodDeclaration, ConstraintEntailment, ConstructorDeclaration, FieldAccess, FieldPath, InstanceOf, MethodCall, MethodImplementation, ObjectConstruction}
import dcc.syntax.Program.Program
import dcc.syntax.Implicit.StringToId
import dcc.types.Type

object NumericExpressions {
  val program: Program =
    NaturalNumbers.program ++ List(
      ConstructorDeclaration("Lit", "x", List(InstanceOf(FieldPath("x", "value"), "Nat"))),
      ConstructorDeclaration("Plus", "x", List(InstanceOf(FieldPath("x", "l"), "Exp"), InstanceOf(FieldPath("x", "r"), "Exp"))),
      ConstraintEntailment("x", List(InstanceOf("x", "Lit"), InstanceOf(FieldPath("x", "value"), "Nat")), InstanceOf("x", "Exp")),
      ConstraintEntailment("x", List(InstanceOf("x", "Plus"), InstanceOf(FieldPath("x", "l"), "Exp"), InstanceOf(FieldPath("x", "r"), "Exp")), InstanceOf("x", "Exp")),
      AbstractMethodDeclaration("eval", "x", List(InstanceOf("x", "Exp")), Type("y", List(InstanceOf("y", "Exp")))),
      MethodImplementation("eval", "x",
        List(InstanceOf("x", "Lit"), InstanceOf(FieldPath("x", "value"), "Nat")), // Args
        Type("y", List(InstanceOf("y", "Exp"))), // Return type
        "x"), // Body
      MethodImplementation("eval", "x",
        List( // Args
          InstanceOf("x", "Plus"),
          InstanceOf(FieldPath("x", "l"), "Lit"),
          InstanceOf(FieldPath("x", "r"), "Lit"),
          InstanceOf(FieldPath(FieldPath("x", "l"), "value"), "Nat"),
          InstanceOf(FieldPath(FieldPath("x", "l"), "value"), "Zero")
        ),
        Type("y", List(InstanceOf("y", "Exp"))), // Return type
        FieldAccess("x", "l")), // Body
      MethodImplementation("eval", "x",
        List( // Args
          InstanceOf("x", "Plus"),
          InstanceOf(FieldPath("x", "l"), "Lit"),
          InstanceOf(FieldPath("x", "r"), "Lit"),
          InstanceOf(FieldPath(FieldPath("x", "l"), "value"), "Nat"),
          InstanceOf(FieldPath(FieldPath("x", "l"), "value"), "Succ"),
          InstanceOf(FieldPath(FieldPath(FieldPath("x", "l"), "value"), "p"), "Nat")
        ),
        Type("y", List(InstanceOf("y", "Exp"))), // Return type
        MethodCall("eval", // Body
          ObjectConstruction("Plus", List(
            ("l", ObjectConstruction("Lit", List(("value", ObjectConstruction("Succ", List(("p", FieldAccess(FieldAccess("x", "l"), "value")))))))),
            ("r", ObjectConstruction("Lit", List(("value", MethodCall("prev", FieldAccess(FieldAccess("x", "r"), "value"))))))
          )))
      ),
      MethodImplementation("eval", "x",
        List( // Args
          InstanceOf("x", "Plus"),
          InstanceOf(FieldPath("x", "l"), "Exp"),
          InstanceOf(FieldPath("x", "r"), "Plus"),
          InstanceOf(FieldPath(FieldPath("x", "r"), "l"), "Exp"),
          InstanceOf(FieldPath(FieldPath("x", "r"), "r"), "Exp")
        ),
        Type("y", List(InstanceOf("y", "Exp"))), // Return type
        MethodCall("eval",
          ObjectConstruction("Plus", List(
            ("l", FieldAccess("x", "l")),
            ("r", MethodCall("eval", FieldAccess("x", "r")))
          ))) ), // Body
      MethodImplementation("eval", "x",
        List( // Args
          InstanceOf("x", "Plus"),
          InstanceOf(FieldPath("x", "l"), "Plus"),
          InstanceOf(FieldPath("x", "r"), "Exp"),
          InstanceOf(FieldPath(FieldPath("x", "l"), "l"), "Exp"),
          InstanceOf(FieldPath(FieldPath("x", "l"), "r"), "Exp")
        ),
        Type("y", List(InstanceOf("y", "Exp"))), // Return type
        MethodCall("eval",
          ObjectConstruction("Plus", List(
            ("l", MethodCall("eval", FieldAccess("x", "l"))),
            ("r", FieldAccess("x", "r"))
          ))) ), // Body
    )
}
