package dcc.entailment
import dcc.syntax.{Constraint, Util}
import dcc.syntax.Program.Program
import smt.smtlib.SMTLibScript
import smt.smtlib.syntax.{SMTLibSymbol, SimpleSymbol, Unsat}
import smt.solver.Z3Solver

class PathDepthLimitEncoding(program: Program, debug: Int = 0) extends Entailment {
  override def entails(context: List[Constraint], constraint: Constraint): Boolean = {
    if (debug > 0)
      println(s"entailment: ${if (context.isEmpty) "·" else Util.commaSeparate(context)} |- $constraint")

    val smt = encode(context, constraint)
    val solver = new Z3Solver(smt, debug=if (debug>2) true else false)

    solver.checkSat match {
      case Left(Unsat) => true
      case Left(_) => false
      case Right(errors) =>
        System.err.println("SMT error:")
        errors foreach {error => System.err.println(error.format)}
        false
    }
  }

  override def entails(context: List[Constraint], constraints: List[Constraint]): Boolean = constraints.forall(entails(context, _))

  def encode(context: List[Constraint], conclusion: Constraint): SMTLibScript = {
    resetFreshNameCounter()

    //TODO
    SMTLibScript(Seq())
  }

  private var varCounter = 0
  private def freshVariable(): SMTLibSymbol = {
    varCounter = varCounter + 1
    SimpleSymbol(s"x$varCounter")
  }
  private var pathCounter = 0
  private def freshPath(): SMTLibSymbol = {
    pathCounter = pathCounter + 1
    SimpleSymbol(s"p$pathCounter")
  }
  private var classCounter = 0
  private def freshClassVar(): SMTLibSymbol = {
    classCounter = classCounter + 1
    SimpleSymbol(s"cls$classCounter")
  }

  private def resetFreshNameCounter(): Unit = {
    varCounter = 0
    pathCounter = 0
    classCounter = 0
  }
}
