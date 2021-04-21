package smt.solver

import smt.smtlib.syntax.{CheckSatResponse, ErrorResponse, GetModelResponse, SMTLibString, Sat, Unknown, Unsat}
import smt.smtlib.{SMTLibCommand, SMTLibScript}

// TODO: add timeout flag as Option[Int] default None
//  add public setter/getter for timeout flag?
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
  def addCommands(commands: SMTLibCommand*): Boolean

  /**
    * Add a `SMTLibScript` to be solved.
    * @param script A `SMTLibScript` to be added.
    * @return True if the script has been added successfully.
    */
  def addScript(script: SMTLibScript): Boolean

  //def getResponse(command: SMTLibCommand): GeneralResponse

  /** Flush the currently held commands. Axioms are kept. */
  def flush(): Unit

//  /**
//    * Fabricates a sequence of `String`s callable via `ProcessBuilder`
//    * in order to execute a SMTSolver.
//    * Assuming the SMTSolver is in the operating systems path.
//    * @param timeout The timeout per query in milliseconds.
//    * @return A callable sequence to execute the SMTSolver.
//    */
//  def makeCall(timeout: Int): Seq[String]

  /**
    * Executes the SMTSolver with the currently held commands.
    * @param timeout The timeout for each query to the SMTSolver in milliseconds.
    * @return The return code of the SMTSolver or -1 in case of a timeout
    *         and the stdout of the solver.
    */
  def execute(timeout: Int = 2000): (Int, Seq[String])

  /**
    * Executes the SMTSolver with the currently held commands
    * and checks them for satisfiability.
    * @param timeout The timeout for each query to the SMTSolver in milliseconds.
    * @return `Sat` if the input is satisfiable
    *        `Unsat` if the input is unsatisfiable
    *        `Unknown` if the solver can't decide.
    */
  def checkSat(timeout: Int = 2000): Either[CheckSatResponse, Seq[ErrorResponse]]

  /**
    * Executes the SMTSolver with the currently held commands
    * and checks them for satisfiability
    * and retrieves a model from the solver if possible.
    * @param timeout The timeout for each query to the SMTSolver in milliseconds.
    * @return `Sat` if the input is satisfiable
    *        `Unsat` if the input is unsatisfiable
    *        `Unknown` if the solver can't decide.
    *        as well as a model in case of `Sat`
    */
  def getModel(timeout: Int = 1000): Either[(CheckSatResponse, Option[GetModelResponse]), Seq[ErrorResponse]]

  protected def parseSatResponse(s: String): Either[CheckSatResponse, ErrorResponse] = s match {
    case "sat"                       => Left(Sat)
    case "unsat"                     => Left(Unsat)
    case "unknown"                   => Left(Unknown)
    case _ if s.startsWith("(error") => Right(ErrorResponse(SMTLibString(s)))
    case _                           => Right(ErrorResponse(SMTLibString(s"Undetected response: $s"))) // some other response?
  }
}