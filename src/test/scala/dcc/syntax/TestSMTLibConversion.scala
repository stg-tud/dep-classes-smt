package dcc.syntax

import dcc.syntax.Program.Program
import org.scalatest.FunSuite
import smtlib.syntax._

class TestSMTLibConversion extends FunSuite {
  test("Convert Id") {
    assert(SMTLibConverter.convertId(Id('x)) == SMTLibString("x"))
    assert(SMTLibConverter.convertId(Id('x0)) == SMTLibString("x0"))
    assert(SMTLibConverter.convertId(Id('fooBar42)) == SMTLibString("fooBar42"))
  }

  val p: Path = Id('x)
  val q: Path = FieldPath(Id('x), Id('f))
  val r: Path = FieldPath(FieldPath(Id('x), Id('f)), Id('f1))

  val pSMTLib = Apply(SimpleSymbol("var"), Seq(SMTLibString("x")))
  val qSMTLib = Apply(
                  SimpleSymbol("pth"),
                  Seq(
                    Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))),
                    SMTLibString("f")
                  ))
  val rSMTLib = Apply(
                  SimpleSymbol("pth"),
                  Seq(
                    Apply(SimpleSymbol("pth"), Seq(
                      Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))),
                      SMTLibString("f")
                    )),
                    SMTLibString("f1")
                  ))

  test("Convert Path") {
    assert(SMTLibConverter.convertPath(p) == pSMTLib)
    assert(SMTLibConverter.convertPath(q) == qSMTLib)
    assert(SMTLibConverter.convertPath(r) == rSMTLib)
  }

  val eq = PathEquivalence(p, q)
  val of = InstanceOf(p, Id('Cls))
  val by = InstantiatedBy(q, Id('Cls))

  val eqSMTLib = Apply(SimpleSymbol("path-eq"), Seq(pSMTLib, qSMTLib))
  val ofSMTLib = Apply(SimpleSymbol("instance-of"), Seq(pSMTLib, SMTLibString("Cls")))
  val bySMTLib = Apply(SimpleSymbol("instantiated-by"), Seq(qSMTLib, SMTLibString("Cls")))

  test("Convert Constraints") {
    assert(SMTLibConverter.convertConstraint(eq) == eqSMTLib)
    assert(SMTLibConverter.convertConstraint(of) == ofSMTLib)
    assert(SMTLibConverter.convertConstraint(by) == bySMTLib)
  }

  val naturalNumbers: Program = List(
    ConstructorDeclaration(Id('Zero), Id('x), Nil),
    ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Zero))), InstanceOf(Id('x), Id('Nat))),
    ConstructorDeclaration(Id('Succ), Id('x), List(InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat)))),
    ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Succ)), InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat))), InstanceOf(Id('x), Id('Nat))),
    AbstractMethodDeclaration(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Nat))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat))))),
    MethodImplementation(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Zero))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat)))),
      ObjectConstruction(Id('Zero), Nil)),
    MethodImplementation(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Succ)), InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat)))),
      FieldAccess(Id('x), Id('p)))
  )

  val zeroEntailment = Apply(
                        SimpleSymbol("in-program"),
                        Seq(
                          SMTLibString("x"),
                          Apply(SimpleSymbol("insert"), Seq(
                            Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))), SMTLibString("Zero"))),
                            SimpleSymbol("nil")
                          )),
                          Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))), SMTLibString("Nat")))
                        ))

  val succEntailment = Apply(
                        SimpleSymbol("in-program"),
                        Seq(
                          SMTLibString("x"),
                          Apply(SimpleSymbol("insert"), Seq(
                            Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))), SMTLibString("Succ"))),
                            Apply(SimpleSymbol("insert"), Seq(
                              Apply(SimpleSymbol("instance-of"), Seq(
                                Apply(SimpleSymbol("pth"), Seq(
                                  Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))),
                                  SMTLibString("p")
                                )),
                                SMTLibString("Nat"))),
                              SimpleSymbol("nil")
                            ))
                          )),
                          Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))), SMTLibString("Nat")))
                        ))

//  test("Convert Program Entailments") {
//    val entailments = SMTLibConverter.convertProgramEntailments(naturalNumbers)
//
//    assert(entailments.size == 2)
//    assert(entailments.head == zeroEntailment)
//    assert(entailments.last == succEntailment)
//  }

  test("Convert Entailment") {
    val ctx = List(by, eq)
    val entailment = Apply(SimpleSymbol("entails"), Seq(
      Apply(SimpleSymbol("insert"), Seq(
        bySMTLib,
        Apply(SimpleSymbol("insert"), Seq(
          eqSMTLib,
          SimpleSymbol("nil")
        ))
      )),
      ofSMTLib
    ))

    assert(SMTLibConverter.convertEntailment(ctx, of) == entailment)
  }

  test("Convert Variables") {
    val cs = List(of, by, eq, PathEquivalence(Id('y), Id('z)), PathEquivalence(FieldPath(Id('m), Id('h)), Id('y)))
    val vars = List(
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("m"))),
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("y"))),
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("z"))),
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("x")))
    )

    assert(SMTLibConverter.convertVariables(cs) == vars)
  }

  test("Convert Variables Paths Classes") {
    val x: Path = Id('x)
    val y: Path = Id('y)
    val z: Path = Id('z)
    val pth: Path = FieldPath(Id('x), Id('f))

    val xSMTLib = Apply(SimpleSymbol("var"), Seq(SMTLibString("x")))
    val ySMTLib = Apply(SimpleSymbol("var"), Seq(SMTLibString("y")))
    val zSMTLib = Apply(SimpleSymbol("var"), Seq(SMTLibString("z")))
    val pthSMTLib = Apply(SimpleSymbol("pth"), Seq(
      Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))),
      SMTLibString("f")
    ))

    val cs = List(
      PathEquivalence(z, pth),
      PathEquivalence(x, y),
      InstanceOf(x, Id('Cls1)),
      InstantiatedBy(y, Id('Cls1)),
      InstantiatedBy(z, Id('Cls2))
    )

    val (vars, paths, classes) = SMTLibConverter.convertVariablesPathsClasses(cs)

    val expectedVars = List(
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("y"))),
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("x"))),
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("z"))),
    )

    val expectedPaths = List(
      Apply(SimpleSymbol("path-exists"), Seq(ySMTLib)),
      Apply(SimpleSymbol("path-exists"), Seq(xSMTLib)),
      Apply(SimpleSymbol("path-exists"), Seq(pthSMTLib)),
      Apply(SimpleSymbol("path-exists"), Seq(zSMTLib)),
    )

    val expectedClasses = List(
      Apply(SimpleSymbol("class"), Seq(SMTLibString("Cls2"))),
      Apply(SimpleSymbol("class"), Seq(SMTLibString("Cls1"))),
    )

    assert(vars == expectedVars)
    assert(paths == expectedPaths)
    assert(classes == expectedClasses)
  }

  test("Make asserts"){
    val terms = List(
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("y"))),
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("x"))),
      Apply(SimpleSymbol("variable"), Seq(SMTLibString("z")))
    )

    val expectedAsserts = List(
      Assert(Apply(SimpleSymbol("variable"), Seq(SMTLibString("y")))),
      Assert(Apply(SimpleSymbol("variable"), Seq(SMTLibString("x")))),
      Assert(Apply(SimpleSymbol("variable"), Seq(SMTLibString("z"))))
    )

    assert(SMTLibConverter.makeAsserts(terms) == expectedAsserts)
  }

  test("make program entailment lookup function") {
    // TODO: multi hit in lookup
    val p: Program = List(
      ConstraintEntailment(Id('x), List(InstantiatedBy(Id('x), Id('Zero))), InstanceOf(Id('x), Id('Nat))),
      ConstructorDeclaration(Id('Zero), Id('x), Nil),
      ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Zero)), PathEquivalence(Id('x), FieldPath(Id('x), Id('f)))), InstanceOf(Id('x), Id('Nat))),
    )
    val vars: List[Id] = List(Id('x), Id('y), Id('z))

    val actual = SMTLibConverter.makeProgramEntailmentLookupFunction(p, vars)

    val c = SimpleSymbol("c")
    val x = Apply(SimpleSymbol("var"), Seq(SMTLibString("x")))
    val y = Apply(SimpleSymbol("var"), Seq(SMTLibString("y")))
    val z = Apply(SimpleSymbol("var"), Seq(SMTLibString("z")))

    def instOf(x: Term, cls: String) = Apply(SimpleSymbol("instance-of"), Seq(x, SMTLibString(cls)))
    def instBy(x: Term, cls: String) = Apply(SimpleSymbol("instantiated-by"), Seq(x, SMTLibString(cls)))
    def pEq(p: Term, q: Term) = Apply(SimpleSymbol("path-eq"), Seq(p, q))
    def insert(x: Term, xs: Term) = Apply(SimpleSymbol("insert"), Seq(x, xs))
    val nil = SimpleSymbol("nil")

    val expected = DefineFun(FunctionDef(SimpleSymbol("lookup-program-entailment"),
      Seq(SortedVar(c, SimpleSymbol("Constraint"))),
      Sorts(SimpleSymbol("List"), Seq(SimpleSymbol("Constraint"))),
      Ite(
        Eq(c, instOf(x, "Nat")),
        insert(instBy(x, "Zero"), nil),
        Ite(
          Eq(c, instOf(x, "Nat")),
          insert(
            instOf(x, "Zero"),
            insert(
              pEq(x, Apply(SimpleSymbol("pth"), Seq(x, SMTLibString("f")))),
              nil)),
          Ite(
            Eq(c, instOf(y, "Nat")),
            insert(instBy(y, "Zero"), nil),
            Ite(
              Eq(c, instOf(y, "Nat")),
              insert(
                instOf(y, "Zero"),
                insert(
                  pEq(y, Apply(SimpleSymbol("pth"), Seq(y, SMTLibString("f")))),
                  nil)),
              Ite(
                Eq(c, instOf(z, "Nat")),
                insert(instBy(z, "Zero"), nil),
                Ite(
                  Eq(c, instOf(z, "Nat")),
                  insert(
                    instOf(z, "Zero"),
                    insert(
                      pEq(z, Apply(SimpleSymbol("pth"), Seq(z, SMTLibString("f")))),
                      nil)),
                  nil
                )
              )
            )
          )
        )
      )
    ))

    assert(actual == expected)
  }

//  test("Instantiate program entailments") {
//    val p: Program = List(
//      ConstraintEntailment(Id('x), List(InstantiatedBy(Id('x), Id('Zero))), InstanceOf(Id('x), Id('Nat))),
//      ConstructorDeclaration(Id('Zero), Id('x), Nil),
//      ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Zero)), PathEquivalence(Id('x), FieldPath(Id('x), Id('f)))), InstanceOf(Id('x), Id('Nat))),
//    )
//    val vars: List[Id] = List(Id('x), Id('y), Id('z))
//
//    val actual = SMTLibConverter.instantiateProgramEntailments(p, vars)
//    val expected = List(
//      Apply(SimpleSymbol("in-program"),
//        Seq(SMTLibString("x"),
//          Apply(SimpleSymbol("insert"), Seq(
//            Apply(SimpleSymbol("instantiated-by"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))), SMTLibString("Zero"))),
//            SimpleSymbol("nil")
//          )),
//          Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))), SMTLibString("Nat")))
//        )),
//      Apply(SimpleSymbol("in-program"),
//        Seq(SMTLibString("x"),
//          Apply(SimpleSymbol("insert"), Seq(
//            Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))), SMTLibString("Zero"))),
//            Apply(SimpleSymbol("insert"), Seq(
//              Apply(SimpleSymbol("path-eq"), Seq(
//                Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))),
//                Apply(SimpleSymbol("pth"), Seq(
//                  Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))),
//                  SMTLibString("f")
//                ))
//              )),
//              SimpleSymbol("nil")
//            ))
//          )),
//          Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("x"))), SMTLibString("Nat")))
//      )),
//      Apply(SimpleSymbol("in-program"),
//        Seq(SMTLibString("y"),
//          Apply(SimpleSymbol("insert"), Seq(
//            Apply(SimpleSymbol("instantiated-by"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("y"))), SMTLibString("Zero"))),
//            SimpleSymbol("nil")
//          )),
//          Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("y"))), SMTLibString("Nat")))
//        )),
//      Apply(SimpleSymbol("in-program"),
//        Seq(SMTLibString("y"),
//          Apply(SimpleSymbol("insert"), Seq(
//            Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("y"))), SMTLibString("Zero"))),
//            Apply(SimpleSymbol("insert"), Seq(
//              Apply(SimpleSymbol("path-eq"), Seq(
//                Apply(SimpleSymbol("var"), Seq(SMTLibString("y"))),
//                Apply(SimpleSymbol("pth"), Seq(
//                  Apply(SimpleSymbol("var"), Seq(SMTLibString("y"))),
//                  SMTLibString("f")
//                ))
//              )),
//              SimpleSymbol("nil")
//            ))
//          )),
//          Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("y"))), SMTLibString("Nat")))
//      )),
//      ////
//      Apply(SimpleSymbol("in-program"),
//        Seq(SMTLibString("z"),
//          Apply(SimpleSymbol("insert"), Seq(
//            Apply(SimpleSymbol("instantiated-by"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("z"))), SMTLibString("Zero"))),
//            SimpleSymbol("nil")
//          )),
//          Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("z"))), SMTLibString("Nat")))
//        )),
//      Apply(SimpleSymbol("in-program"),
//        Seq(SMTLibString("z"),
//          Apply(SimpleSymbol("insert"), Seq(
//            Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("z"))), SMTLibString("Zero"))),
//            Apply(SimpleSymbol("insert"), Seq(
//              Apply(SimpleSymbol("path-eq"), Seq(
//                Apply(SimpleSymbol("var"), Seq(SMTLibString("z"))),
//                Apply(SimpleSymbol("pth"), Seq(
//                  Apply(SimpleSymbol("var"), Seq(SMTLibString("z"))),
//                  SMTLibString("f")
//                ))
//              )),
//              SimpleSymbol("nil")
//            ))
//          )),
//          Apply(SimpleSymbol("instance-of"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("z"))), SMTLibString("Nat")))
//      ))
//    )
//
//    assert(actual.size == expected.size)
//    assert(actual.forall(expected.contains(_)))
//  }
}
