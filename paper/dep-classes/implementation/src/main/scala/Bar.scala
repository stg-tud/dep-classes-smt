import smt.smtlib.syntax.Implicit.stringToSimpleSymbol
import smt.smtlib.syntax._
import smt.smtlib.theory.BoolPredefined.{And, Eq, Implies, Not, Or}
import smt.smtlib.{SMTLib, SMTLibCommand, SMTLibScript}
import smt.solver.Z3Solver

object Bar extends App {
  val smt = SMTLibScript(Seq(
    SMTLib.buildEnumerationType("Variable", List("x", "y")),
    DeclareFun("path-equivalence", Seq("Variable", "Variable"), "Bool"),
    DefineFun(FunctionDef(
      "substitute",
      Seq(
        SortedVar("p1", "Variable"),
        SortedVar("x1", "Variable"),
        SortedVar("p2", "Variable"),
        SortedVar("p3", "Variable")
      ),
      "Bool",
      Or(
        And(Eq("p1", "y"), Eq("x1", "y"), Eq("p2", "y"), Eq("p3", "y")),
        And(Eq("p1", "y"), Eq("x1", "y"), Eq("p2", "x"), Eq("p3", "x")),
        And(Eq("p1", "y"), Eq("x1", "x"), Eq("p2", "y"), Eq("p3", "y")),
        And(Eq("p1", "y"), Eq("x1", "x"), Eq("p2", "x"), Eq("p3", "y")),
        And(Eq("p1", "x"), Eq("x1", "y"), Eq("p2", "y"), Eq("p3", "x")),
        And(Eq("p1", "x"), Eq("x1", "y"), Eq("p2", "x"), Eq("p3", "x")),
        And(Eq("p1", "x"), Eq("x1", "x"), Eq("p2", "y"), Eq("p3", "y")),
        And(Eq("p1", "x"), Eq("x1", "x"), Eq("p2", "x"), Eq("p3", "x"))
      )
    )),
    Assert(Forall(
      Seq(
        SortedVar("p4", "Variable")
      ),
      Apply("path-equivalence", Seq("p4", "p4"))
    )),
    Assert(Forall(
      Seq(
        SortedVar("p6", "Variable"),
        SortedVar("p7", "Variable"),
        SortedVar("x2", "Variable"),
        SortedVar("p8", "Variable"),
        SortedVar("p9", "Variable"),
        SortedVar("p10", "Variable"),
        SortedVar("p11", "Variable"),
        SortedVar("p12", "Variable"),
        SortedVar("p13", "Variable")
      ),
      Implies(
        And(
          Apply("path-equivalence", Seq("p9", "p8")),
          Apply("substitute", Seq("p6", "x2", "p8", "p10")),
          Apply("substitute", Seq("p7", "x2", "p8", "p11")),
          Apply("path-equivalence", Seq("p10", "p11"))
        ),
        And(
          Apply("substitute", Seq("p6", "x2", "p9", "p12")),
          Apply("substitute", Seq("p7", "x2", "p9", "p13")),
          Apply("path-equivalence", Seq("p12", "p13"))
        )
      )
    )),
    Assert(Not(Apply("path-equivalence", Seq("x", "y")))),
  ))

  val solver = new Z3Solver(SMTLibScript.EMPTY, debug = true)

  solver.addScript(smt)
  solver.checkSat
  solver.flush()

  // ground this example and name the asserts to know which substitution to blame
  val vars = List("x", "y")

  def reflTemplate(p: String): SMTLibCommand = Assert(Annotate(Apply("path-equivalence", Seq(p, p)), Seq(KeyValueAttribute(Keyword("named"), s"C-Refl-$p"))))

  def substTemplate(p: String, q: String, x: String, r: String, s: String, res1: String, res2: String, res3: String, res4: String): SMTLibCommand = Assert(Annotate(
    Implies(
      And(
        Apply("path-equivalence", Seq(s, r)),
        Apply("substitute", Seq(p, x, r, res1)),
        Apply("substitute", Seq(q, x, r, res2)),
        Apply("path-equivalence", Seq(res1, res2))
      ),
      And(
        Apply("substitute", Seq(p, x, s, res3)),
        Apply("substitute", Seq(q, x, s, res4)),
        Apply("path-equivalence", Seq(res3, res4))
      )
    ),
    Seq(KeyValueAttribute(Keyword("named"), s"C-Subst-$p-$q-$x-$r-$s-$res1-$res2-$res3-$res4"))
  ))

  var substRules: List[SMTLibCommand] = Nil
  vars.foreach(p =>
    vars.foreach(q =>
      vars.foreach(x =>
        vars.foreach(r =>
          vars.foreach(s =>
            vars.foreach(res1 =>
              vars.foreach(res2 =>
                vars.foreach(res3 =>
                  vars.foreach(res4 =>
                    substRules = substTemplate(p, q, x, r, s, res1, res2, res3, res4) :: substRules
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  val groundSMT = SMTLibScript(Seq(
      SetOption(ProduceUnsatCores(true)),
      SMTLib.buildEnumerationType("Variable", List("x", "y")),
      DeclareFun("path-equivalence", Seq("Variable", "Variable"), "Bool"),
      DefineFun(FunctionDef(
        "substitute",
        Seq(
          SortedVar("p1", "Variable"),
          SortedVar("x1", "Variable"),
          SortedVar("p2", "Variable"),
          SortedVar("p3", "Variable")
        ),
        "Bool",
        Or(
          And(Eq("p1", "y"), Eq("x1", "y"), Eq("p2", "y"), Eq("p3", "y")),
          And(Eq("p1", "y"), Eq("x1", "y"), Eq("p2", "x"), Eq("p3", "x")),
          And(Eq("p1", "y"), Eq("x1", "x"), Eq("p2", "y"), Eq("p3", "y")),
          And(Eq("p1", "y"), Eq("x1", "x"), Eq("p2", "x"), Eq("p3", "y")),
          And(Eq("p1", "x"), Eq("x1", "y"), Eq("p2", "y"), Eq("p3", "x")),
          And(Eq("p1", "x"), Eq("x1", "y"), Eq("p2", "x"), Eq("p3", "x")),
          And(Eq("p1", "x"), Eq("x1", "x"), Eq("p2", "y"), Eq("p3", "y")),
          And(Eq("p1", "x"), Eq("x1", "x"), Eq("p2", "x"), Eq("p3", "x"))
        )
      )),
      Assert(Not(Apply("path-equivalence", Seq("x", "y")))),
    )) ++ SMTLibScript(vars.map(reflTemplate)) ++ SMTLibScript(substRules) ++ SMTLibScript(Seq(CheckSat, GetUnsatCore))

  solver.flush()
  solver.addScript(groundSMT)
  solver.execute
}
