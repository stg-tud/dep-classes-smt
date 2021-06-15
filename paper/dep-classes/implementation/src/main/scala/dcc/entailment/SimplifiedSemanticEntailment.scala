package dcc.entailment
import dcc.syntax.{Constraint, Util}
import smt.smtlib.SMTLibScript
import smt.smtlib.syntax.Unsat
import smt.solver.Z3Solver

class SimplifiedSemanticEntailment(debug: Int = 0) extends Entailment {
  override def entails(context: List[Constraint], constraint: Constraint): Boolean = {
    if (debug > 0)
      println(s"entailment: ${Util.commaSeparate(context)} |- $constraint")

    val smt = encoding(context, Some(constraint))
    val solver = new Z3Solver(smt, debug = if (debug>2) true else false)

    solver.checkSat match {
      case Left(Unsat)  => true
      case Left(_)      => false
      case Right(errors) =>
        System.err.println("Entailment check error:")
        errors foreach { error => System.err.println(error.format) }
        false
    }
  }

  override def entails(context: List[Constraint], constraints: List[Constraint]): Boolean = constraints.forall(entails(context, _))

  def encoding(context: List[Constraint], conclusion: Option[Constraint]): SMTLibScript = {
    val smt: SMTLibScript = SMTLibScript(Seq.empty)

    // TODO: all the stuff

    if (debug > 1) {
      println("First-Order Encoding:")
      println(smt.format)
    }

    smt
  }
}
