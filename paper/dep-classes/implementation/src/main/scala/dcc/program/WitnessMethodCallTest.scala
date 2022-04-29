package dcc.program

import dcc.syntax.{AbstractMethodDeclaration, ConstraintEntailment, ConstructorDeclaration, FieldPath, InstanceOf, MethodImplementation, ObjectConstruction}
import dcc.syntax.Program.Program
import dcc.syntax.Implicit.StringToId
import dcc.types.Type

object WitnessMethodCallTest {
  val program: Program = List(
    ConstructorDeclaration("A", "x",  Nil),
    ConstructorDeclaration("B", "x",  Nil),
    ConstraintEntailment("x", List(InstanceOf("x", "A")), InstanceOf("x", "Param")),
    ConstraintEntailment("x", List(InstanceOf("x", "B")), InstanceOf("x", "Param")),
    ConstructorDeclaration("WitnessT", "x",  Nil),
    ConstructorDeclaration("WitnessF", "x",  Nil),
    ConstraintEntailment("x", List(InstanceOf("x", "WitnessT")), InstanceOf("x", "Witness")),
    ConstraintEntailment("x", List(InstanceOf("x", "WitnessF")), InstanceOf("x", "Witness")),
    ConstructorDeclaration("Result", "x", List()),
    // witness method
    AbstractMethodDeclaration("property", "x", List(InstanceOf("x", "Param")), Type("y", Set(InstanceOf("y", "Witness")))),
//    MethodImplementation("property", "x", List(InstanceOf("x", "A")), Type("y", Set(InstanceOf("y", "Witness"))),
//      ObjectConstruction("WitnessT", Nil)
//    ),
//    MethodImplementation("property", "x", List(InstanceOf("x", "B")), Type("y", Set(InstanceOf("y", "Witness"))),
//      ObjectConstruction("WitnessF", Nil)
//    ),
//    // method over witness argument
//    AbstractMethodDeclaration("m", "x", List(InstanceOf("x", "WitnessT")), Type("y", Set(InstanceOf("y", "Result")))),
//    MethodImplementation("m", "x", List(InstanceOf("x", "WitnessT")), Type("y", Set(InstanceOf("y", "Result"))),
//      ObjectConstruction("Result", List())
//    )
  )
}
