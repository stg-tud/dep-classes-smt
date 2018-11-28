package smtlib.solver

import smtlib.syntax.{CheckSatResponse, GetModelResponse, Sat, Unknown, Unsat}
import smtlib.{SMTLibCommand, SMTLibScript}

trait SMTSolver {
  /**
    * The Axioms that are true for all queries to the solver.
    */
  val axioms: SMTLibScript

  /**
    * Add a `SMTLibCommand` to be solved.
    * @param command A `SMTLibCommand` to be added.
    * @return True if the command has been added successfully .
    */
  def addCommand(command: SMTLibCommand): Boolean

  /**
    * Add a sequence of `SMTLibCommand`s to be solved.
    * @param commands A sequence of `SMTLibCommand`s to be added.
    * @return True if the commands have been added successfully.
    */
  def addCommands(commands: Seq[SMTLibCommand]): Boolean

  /**
    * Add a `SMTLibScript` to be solved.
    * @param script A `SMTLibScript` to be added.
    * @return True if the script has been added successfully.
    */
  def addScript(script: SMTLibScript): Boolean

  //def getResponse(command: SMTLibCommand): GeneralResponse

  /** Flush the currently held commands. Axioms are kept. */
  def flush(): Unit

  /**
    * Fabricates a sequence of `String`s callable via `ProcessBuilder`
    * in order to execute a SMTSolver.
    * Assuming the SMTSolver is in the operating systems path.
    * @param timeout The timeout per query in milliseconds.
    * @return A callable sequence to execute the SMTSolver.
    */
  def makeCall(timeout: Int): Seq[String]

  /**
    * Executes the SMTSolver with the currently held commands.
    * @param timeout The timeout for each query to the SMTSolver in milliseconds.
    * @return The return code of the SMTSolver or -1 in case of a timeout
    *         and the stdout of the solver.
    */
  def execute(timeout: Int = 1000): (Int, Seq[String])

  /**
    * Executes the SMTSolver with the currently held commands
    * and checks them for satisfiability.
    * @param timeout The timeout for each query to the SMTSolver in milliseconds.
    * @return `Sat` if the input is satisfiable
    *        `Unsat` if the infput is unsatisfiable
    *        `Unknown` if the solver can't decide.
    */
  def checksat(timeout: Int = 1000): CheckSatResponse

  /**
    * Executes the SMTSolver with the currently held commands
    * and checks them for satisfiability
    * and retrieves a model from the solver if possible.
    * @param timeout The timeout for each query to the SMTSolver in milliseconds.
    * @return `Sat` if the input is satisfiable
    *        `Unsat` if the infput is unsatisfiable
    *        `Unknown` if the solver can't decide.
    *        as well as a model in case of `Sat`
    */
  def getModel(timeout: Int = 1000): (CheckSatResponse, Option[GetModelResponse])

  protected def parseSatResponse(s: String): CheckSatResponse = {
    s match {
      case "sat"     => Sat
      case "unsat"   => Unsat
      case "unknown" => Unknown
      case _         => Unknown // if error
    }
  }
}