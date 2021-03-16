package smtlib.solver

import java.io.PrintWriter

import smtlib.syntax._
import smtlib.{SMTLibCommand, SMTLibScript}

import scala.concurrent._
import scala.sys.process._
import ExecutionContext.Implicits.global
import scala.io.Source

class CVC4Solver(val axioms: SMTLibScript, val options: Seq[SMTLibCommand] = Seq.empty, var debug: Boolean = false) extends SMTSolver {
  var commands: Seq[SMTLibCommand] = Seq.empty

  private def makeCall(timeout: Int = 1000): Seq[String] = { //Seq("cvc4", "-v", "-m", "--lang", "smt2.6.1")
    var call = Seq("cvc4")

    // use smtlib v2 as input language
    call = call ++ Seq("--lang", "smt2.6.1")

    // verbose output
//    call = call :+ "-vv" // TODO: make verbosity parameterized or set it via smtlib option "(set-option :verbosity i)" where 0 <= i <= 2

    // turn on model generation
    call = call :+ "-m"

    // enable incremental solving
//    call = call :+ "-i" // not supported with produce proofs

    // enable time limiting per query (give milliseconds)
    call = call :+ s"--tlimit-per=$timeout"

    // enable time limiting (give milliseconds)
    call = call :+ s"--tlimit=${timeout*commands.size+100}"


    call
  }

  override def execute(timeout: Int): (Int, Seq[String]) = {
    val call = makeCall(timeout)
    var output: Seq[String] = Seq.empty

    val io = new ProcessIO(
      in => {
        val writer = new PrintWriter(in)

        options.foreach(command => {
          val format = command.format()
          if (debug) println(s"< $format")
          writer.println(format)
        })

        axioms.commands.foreach(command => {
          val format = command.format()
          if (debug) println(s"< $format")
          writer.println(format)
        })

        commands.foreach(command => {
          val format = command.format()
          if (debug) println(s"< $format")
          writer.println(format)
        })
        writer.close()
      },
      out => {
        val src = Source.fromInputStream(out)
        for (line <- src.getLines()) {
          if (debug) println(s"> $line")
          output = output :+ line
        }
        src.close()
      },
      BasicIO.toStdErr)
    val p = call.run(io)

    val f = Future(blocking(p.exitValue()))
    try {
      (Await.result(f, duration.Duration(timeout*commands.size+100, "ms")),
       output)
    } catch {
      case _: TimeoutException =>
        p.destroy()
        println("z3 timeout")
        (-1, output)
    }
  }

  override def checksat(timeout: Int): CheckSatResponse = {
    val pre = commands

    addCommand(CheckSat)

    val (status, output) = execute(timeout)

    commands = pre

    if (status == 0 && output.nonEmpty) {
      parseSatResponse(output.last)
    } else {
      // In case of timeout
      Unknown
    }
  }

  override def getModel(timeout: Int): (CheckSatResponse, scala.Option[GetModelResponse]) = {
    val pre = commands

    addCommand(CheckSat)
    addCommand(GetModel)

    val (status, output) = execute(timeout)

    commands = pre

    if (status == 0 && output.nonEmpty) {
      val sat = parseSatResponse(output.head)
      val model = None // TODO: Some(modelparsing)

      (sat, model)
    } else {
      // In case of timeout
      (Unknown, None)
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
