package smtlib.solver

import java.io.PrintWriter

import smtlib.{SMTLibCommand, SMTLibScript}

import scala.concurrent._
import scala.sys.process._
import ExecutionContext.Implicits.global

class Z3Solver(val axioms: SMTLibScript) extends SMTSolver {
  // commands to send to the solver
  var commands: Seq[SMTLibCommand] = Seq()

  override def makeCall(timeout: Int = 1000): Seq[String] = {
//    var call = Seq("z3")
//
//    // use smtlib v2 as input language
//    call = call :+ "-smt2"
//
//    // (soft) timeout per query in ms
//    call = call :+ s"-t:${timeout.toString}"
//
//    // hard timeout in s
//    //call = call :+ s"-T:${((timeout/1000)*commands.size).toString}"
//
//    // interactive mode
//    call = call :+ "-in"
//
//    // input file
//    //call = call :+ ???
//    call

    Seq("z3", "-smt2", s"-t:${timeout.toString}", "-in")
  }

  override def execute(timeout: Int): Int = {
    val call = makeCall(timeout)
    val io = BasicIO.standard(in => {
      val writer = new PrintWriter(in)
      axioms.commands.foreach(command => writer.println(command.format()))
      commands.foreach(command => writer.println(command.format()))
      writer.close()
    })
    val p = call.run(io)

    val f = Future(blocking(p.exitValue()))
    try {
      Await.result(f, duration.Duration(timeout*commands.size+100, "ms"))
    } catch {
      case _: TimeoutException =>
        p.destroy()
        println("z3 timeout")
        -1
    }
  }

  override def addCommand(command: SMTLibCommand): Boolean = {
    commands = commands :+ command
    true
  }

  override def addCommands(commands: Seq[SMTLibCommand]): Boolean = {
    commands.foreach(command => this.commands = this.commands :+ command)
    true
  }

  override def addScript(script: SMTLibScript): Boolean = {
    script.commands.foreach(command => commands = commands :+ command)
    true
  }

  override def flush(): Unit = commands = Seq()
}