package dcc.syntax

import dcc.syntax.Program.Program
import org.scalatest.{FunSuite, PrivateMethodTester}
import smtlib.SMTLibCommand
import smtlib.syntax._

class TestSMTLibConversion extends FunSuite with PrivateMethodTester {
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
    val p: Program = List(
      ConstraintEntailment(Id('x), List(InstantiatedBy(Id('x), Id('Zero))), InstanceOf(Id('x), Id('Nat))),
      ConstructorDeclaration(Id('Zero), Id('x), Nil),
      ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Zero)), PathEquivalence(Id('x), FieldPath(Id('x), Id('f)))), InstanceOf(Id('x), Id('Nat))),
    )
    val paths: List[Id] = List(Id('x), Id('y), Id('z))

    val actual = SMTLibConverter.makeProgramEntailmentLookupFunction(p, paths)

    val c = SimpleSymbol("c")
    val x = Apply(SimpleSymbol("var"), Seq(SMTLibString("x")))
    val y = Apply(SimpleSymbol("var"), Seq(SMTLibString("y")))
    val z = Apply(SimpleSymbol("var"), Seq(SMTLibString("z")))

    def instOf(x: Term, cls: String) = Apply(SimpleSymbol("instance-of"), Seq(x, SMTLibString(cls)))
    def instBy(x: Term, cls: String) = Apply(SimpleSymbol("instantiated-by"), Seq(x, SMTLibString(cls)))
    def pEq(p: Term, q: Term) = Apply(SimpleSymbol("path-eq"), Seq(p, q))
    def insert(x: Term, xs: Term) = Apply(SimpleSymbol("insert"), Seq(x, xs))
    def cons(x: Term, xs: Term) = Apply(SimpleSymbol("cons"), Seq(x, xs))
    val nil = SimpleSymbol("nil") //IdentifierAs(SimpleSymbol("nil"), Sorts(SimpleSymbol("List"), Seq(SimpleSymbol("Constraint"))))
    val nan = SimpleSymbol("nan")

    val expected = DefineFun(FunctionDef(SimpleSymbol("lookup-program-entailment"),
      Seq(SortedVar(c, SimpleSymbol("Constraint"))),
      SimpleSymbol("CsList"),
      Ite(
        Eq(c, instOf(x, "Nat")),
        cons(
          insert(
            instOf(x, "Zero"),
            insert(
              pEq(x, Apply(SimpleSymbol("pth"), Seq(x, SMTLibString("f")))),
              nil)),
          cons(
            insert(instBy(x, "Zero"), nil),
            nan
          )
        ),
        Ite(
          Eq(c, instOf(y, "Nat")),
          cons(
            insert(
              instOf(y, "Zero"),
              insert(
                pEq(y, Apply(SimpleSymbol("pth"), Seq(y, SMTLibString("f")))),
                nil)),
            cons(
              insert(instBy(y, "Zero"), nil),
              nan
            )
          ),
          Ite(
            Eq(c, instOf(z, "Nat")),
            cons(
              insert(
                instOf(z, "Zero"),
                insert(
                  pEq(z, Apply(SimpleSymbol("pth"), Seq(z, SMTLibString("f")))),
                  nil)),
              cons(
                insert(instBy(z, "Zero"), nil),
                nan
              )
            ),
            nan
          )
        )
      )
    ))

    assert(actual == expected)
  }

  test("instantiateSubstRule(x, p, q)") {
    val x = Id('x)
    val p = Id('p)
    val q = Id('q)

    val xTerm = SMTLibString("x")
    val pTerm = Apply(SimpleSymbol("var"), Seq(SMTLibString("p")))
    val qTerm = Apply(SimpleSymbol("var"), Seq(SMTLibString("q")))

    val expectedBody = Forall(
      Seq(
        SortedVar(SimpleSymbol("a2"), SimpleSymbol("Constraint")),
        SortedVar(SimpleSymbol("cs"), Sorts(SimpleSymbol("List"), Seq(SimpleSymbol("Constraint"))))
      ),
      Implies(
        Let(
          Seq(VarBinding(SimpleSymbol("a"), Apply(SimpleSymbol("generalize-constraint"), Seq(SimpleSymbol("a2"), pTerm, xTerm)))),
          Let(
            Seq(VarBinding(SimpleSymbol("a1"), Apply(SimpleSymbol("subst-constraint"), Seq(SimpleSymbol("a"), xTerm, qTerm)))),
            And(
              Apply(SimpleSymbol("entails"), Seq(SimpleSymbol("cs"), Apply(SimpleSymbol("path-eq"), Seq(pTerm, qTerm)))),
              Apply(SimpleSymbol("entails"), Seq(SimpleSymbol("cs"), SimpleSymbol("a1")))
            )
          )
        ),
        Apply(SimpleSymbol("entails"), Seq(SimpleSymbol("cs"), SimpleSymbol("a2")))
      )
    )

    val expectedRule = Assert(Annotate(expectedBody, Seq(KeyValueAttribute(Keyword("named"), SimpleSymbol(s"C-Subst-$x-$p-$q")))))

    val actualRule = SMTLibConverter invokePrivate PrivateMethod[SMTLibCommand]('instantiateSubstRule)(x, p, q)

    assert(actualRule == expectedRule)
  }

  test("instantiateSubstRule(x, x, q)") {
    val x = Id('x)
    val q = Id('q)

    val xTerm = SMTLibString("x")
    val pTerm = Apply(SimpleSymbol("var"), Seq(SMTLibString("x")))
    val qTerm = Apply(SimpleSymbol("var"), Seq(SMTLibString("q")))

    val expectedBody = Forall(
      Seq(
        SortedVar(SimpleSymbol("a2"), SimpleSymbol("Constraint")),
        SortedVar(SimpleSymbol("cs"), Sorts(SimpleSymbol("List"), Seq(SimpleSymbol("Constraint"))))
      ),
      Implies(
        Let(
          Seq(VarBinding(SimpleSymbol("a1"), Apply(SimpleSymbol("subst-constraint"), Seq(SimpleSymbol("a2"), xTerm, qTerm)))),
          And(
            Apply(SimpleSymbol("entails"), Seq(SimpleSymbol("cs"), Apply(SimpleSymbol("path-eq"), Seq(pTerm, qTerm)))),
            Apply(SimpleSymbol("entails"), Seq(SimpleSymbol("cs"), SimpleSymbol("a1")))
          )
        ),
        Apply(SimpleSymbol("entails"), Seq(SimpleSymbol("cs"), SimpleSymbol("a2")))
      )
    )

    val expectedRule = Assert(Annotate(expectedBody, Seq(KeyValueAttribute(Keyword("named"), SimpleSymbol(s"C-Subst-$x-$x-$q")))))

    val actualRule = SMTLibConverter invokePrivate PrivateMethod[SMTLibCommand]('instantiateSubstRule)(x, x, q)

    assert(actualRule == expectedRule)
  }

  test("instantiateSubstRule(x, p, x)") {
    val x = Id('x)
    val p = Id('p)

    val xTerm = SMTLibString("x")
    val pTerm = Apply(SimpleSymbol("var"), Seq(SMTLibString("p")))
    val qTerm = Apply(SimpleSymbol("var"), Seq(SMTLibString("x")))

    val expectedBody = Forall(
      Seq(
        SortedVar(SimpleSymbol("a2"), SimpleSymbol("Constraint")),
        SortedVar(SimpleSymbol("cs"), Sorts(SimpleSymbol("List"), Seq(SimpleSymbol("Constraint"))))
      ),
      Implies(
        Let(
          Seq(VarBinding(SimpleSymbol("a"), Apply(SimpleSymbol("generalize-constraint"), Seq(SimpleSymbol("a2"), pTerm, xTerm)))),
          And(
            Apply(SimpleSymbol("entails"), Seq(SimpleSymbol("cs"), Apply(SimpleSymbol("path-eq"), Seq(pTerm, qTerm)))),
            Apply(SimpleSymbol("entails"), Seq(SimpleSymbol("cs"), SimpleSymbol("a")))
          )
        ),
        Apply(SimpleSymbol("entails"), Seq(SimpleSymbol("cs"), SimpleSymbol("a2")))
      )
    )

    val expectedRule = Assert(Annotate(expectedBody, Seq(KeyValueAttribute(Keyword("named"), SimpleSymbol(s"C-Subst-$x-$p-$x")))))

    val actualRule = SMTLibConverter invokePrivate PrivateMethod[SMTLibCommand]('instantiateSubstRule)(x, p, x)

    assert(actualRule == expectedRule)
  }

  // obsolete since x, x, x entries are skipped in generation
  test("instantiateSubstRule(x, x, x)") {
    val x = Id('x)

    val xTerm = SMTLibString("x")
    val pTerm = Apply(SimpleSymbol("var"), Seq(SMTLibString("x")))
    val qTerm = Apply(SimpleSymbol("var"), Seq(SMTLibString("x")))

    val expectedBody = Forall(
      Seq(
        SortedVar(SimpleSymbol("a2"), SimpleSymbol("Constraint")),
        SortedVar(SimpleSymbol("cs"), Sorts(SimpleSymbol("List"), Seq(SimpleSymbol("Constraint"))))
      ),
      Implies(
        Apply(SimpleSymbol("entails"), Seq(SimpleSymbol("cs"), SimpleSymbol("a2"))),
        Apply(SimpleSymbol("entails"), Seq(SimpleSymbol("cs"), SimpleSymbol("a2")))
      )
    )

    val expectedRule = Assert(Annotate(expectedBody, Seq(KeyValueAttribute(Keyword("named"), SimpleSymbol(s"C-Subst-$x-$x-$x")))))

    val actualRule = SMTLibConverter invokePrivate PrivateMethod[SMTLibCommand]('instantiateSubstRule)(x, x, x)

    assert(actualRule == expectedRule)
  }

  test("instantiateSubstRule(x, x, q.f)") {
    val x = Id('x)
    val q = FieldPath(Id('q), Id('f))

    val xTerm = SMTLibString("x")
    val pTerm = Apply(SimpleSymbol("var"), Seq(SMTLibString("x")))
    val qTerm = Apply(SimpleSymbol("pth"), Seq(Apply(SimpleSymbol("var"), Seq(SMTLibString("q"))), SMTLibString("f")))

    val expectedBody = Forall(
      Seq(
        SortedVar(SimpleSymbol("a2"), SimpleSymbol("Constraint")),
        SortedVar(SimpleSymbol("cs"), Sorts(SimpleSymbol("List"), Seq(SimpleSymbol("Constraint"))))
      ),
      Implies(
        Let(
          Seq(VarBinding(SimpleSymbol("a1"), Apply(SimpleSymbol("subst-constraint"), Seq(SimpleSymbol("a2"), xTerm, qTerm)))),
          And(
            Apply(SimpleSymbol("entails"), Seq(SimpleSymbol("cs"), Apply(SimpleSymbol("path-eq"), Seq(pTerm, qTerm)))),
            Apply(SimpleSymbol("entails"), Seq(SimpleSymbol("cs"), SimpleSymbol("a1")))
          )
        ),
        Apply(SimpleSymbol("entails"), Seq(SimpleSymbol("cs"), SimpleSymbol("a2")))
      )
    )

    val expectedRule = Assert(Annotate(expectedBody, Seq(KeyValueAttribute(Keyword("named"), SimpleSymbol(s"C-Subst-$x-$x-$q")))))

    val actualRule = SMTLibConverter invokePrivate PrivateMethod[SMTLibCommand]('instantiateSubstRule)(x, x, q)

    assert(actualRule == expectedRule)
  }

  test("makePathPairs") {
    val x = Id('x)
    val y = Id('y)
    val z = Id('z)

    val expected = List(
      (x, x),
      (x, y),
      (x, z),
      (y, x),
      (y, y),
      (y, z),
      (z, x),
      (z, y),
      (z, z)
    )

    val actual = SMTLibConverter invokePrivate PrivateMethod[List[(Path, Path)]]('makePathPairs)(List(x, y, z))

    assert(actual.size == expected.size)
    assert(actual.forall(pair => expected.contains(pair)))
  }

  test("generateSubstRules") {
    val x = Id('x)
    val y = Id('y)

    import smtlib.syntax.Implicit.stringToSimpleSymbol

    def expectedRule(body: Term) = Forall(
      Seq(
        SortedVar(SimpleSymbol("a2"), SimpleSymbol("Constraint")),
        SortedVar(SimpleSymbol("cs"), Sorts(SimpleSymbol("List"), Seq(SimpleSymbol("Constraint"))))
      ),
      Implies(
        body,
        Apply("entails", Seq("cs", "a2"))
      )
    )

    val xVar = SMTLibString("x")
    val yVar = SMTLibString("y")
    val xPath = Apply("var", Seq(xVar))
    val yPath = Apply("var", Seq(yVar))

    val expected = Seq(
      Assert(Annotate(
        expectedRule(
          Let(
            Seq(VarBinding("a1", Apply("subst-constraint", Seq("a2", xVar, yPath)))),
            And(
              Apply("entails", Seq("cs", Apply("path-eq", Seq(xPath, yPath)))),
              Apply("entails", Seq("cs", "a1"))
            )
          )
        ),
        Seq(KeyValueAttribute(Keyword("named"), SimpleSymbol("C-Subst-x-x-y")))
      )),
      Assert(Annotate(
        expectedRule(
          Let(
            Seq(VarBinding("a", Apply("generalize-constraint", Seq("a2", yPath, xVar)))),
            And(
              Apply("entails", Seq("cs", Apply("path-eq", Seq(yPath, xPath)))),
              Apply("entails", Seq("cs", "a"))
            )
          )
        ),
        Seq(KeyValueAttribute(Keyword("named"), SimpleSymbol("C-Subst-x-y-x")))
      )),
      Assert(Annotate(
        expectedRule(
          Let(
            Seq(VarBinding("a", Apply("generalize-constraint", Seq("a2", yPath, xVar)))),
            Let(
              Seq(VarBinding("a1", Apply("subst-constraint", Seq("a", xVar, yPath)))),
              Apply("entails", Seq("cs", "a1"))
            )
          )
        ),
        Seq(KeyValueAttribute(Keyword("named"), SimpleSymbol("C-Subst-x-y-y")))
      )),
      Assert(Annotate(
        expectedRule(
          Let(
            Seq(VarBinding("a", Apply("generalize-constraint", Seq("a2", xPath, yVar)))),
            Let(
              Seq(VarBinding("a1", Apply("subst-constraint", Seq("a", yVar, xPath)))),
              Apply("entails", Seq("cs", "a1"))
            )
          )
        ),
        Seq(KeyValueAttribute(Keyword("named"), SimpleSymbol("C-Subst-y-x-x")))
      )),
      Assert(Annotate(
        expectedRule(
          Let(
            Seq(VarBinding("a", Apply("generalize-constraint", Seq("a2", xPath, yVar)))),
            And(
              Apply("entails", Seq("cs", Apply("path-eq", Seq(xPath, yPath)))),
              Apply("entails", Seq("cs", "a"))
            )
          )
        ),
        Seq(KeyValueAttribute(Keyword("named"), SimpleSymbol("C-Subst-y-x-y")))
      )),
      Assert(Annotate(
        expectedRule(
          Let(
            Seq(VarBinding("a1", Apply("subst-constraint", Seq("a2", yVar, xPath)))),
            And(
              Apply("entails", Seq("cs", Apply("path-eq", Seq(yPath, xPath)))),
              Apply("entails", Seq("cs", "a1"))
            )
          )
        ),
        Seq(KeyValueAttribute(Keyword("named"), SimpleSymbol("C-Subst-y-y-x")))
      ))
    )

    val actual = SMTLibConverter.generateSubstRules(List(x, y), List(x, y))

    assert(actual.size == expected.size)
    assert(actual.forall(rule => expected.contains(rule)))
  }
}
