package smtlib.solver

import java.io.PrintWriter

import smtlib.syntax._
import smtlib.{SMTLibCommand, SMTLibScript}

import scala.concurrent._
import scala.sys.process._
import ExecutionContext.Implicits.global
import scala.io.Source

class Z3Solver(val axioms: SMTLibScript, val options: Seq[SMTLibCommand] = Seq.empty, var debug: Boolean = false) extends SMTSolver {
  // commands to send to the solver
  var commands: Seq[SMTLibCommand] = Seq.empty

  override def makeCall(timeout: Int = 1000): Seq[String] = Seq("z3", "-smt2", s"-t:${timeout.toString}", "-in")
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
//  }

  override def execute(timeout: Int): (Int, Seq[String]) = {
    val call = makeCall(timeout)
    var output: Seq[String] = Seq.empty
//    val io = BasicIO.standard(in => {
//      val writer = new PrintWriter(in)
//      axioms.commands.foreach(command => {
//        val format = command.format()
//        if (debug) println(s"< $format")
//        writer.println(format)
//      })
//      commands.foreach(command => {
//        val format = command.format()
//        if (debug) println(s"< $format")
//        writer.println(format)
//      })
//      writer.close()
//    })
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

//        val format = axioms.format()
//        if (debug) println(s"< $format")
//        writer.println(format)

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
    addCommand(CheckSat)

    val (status, output) = execute(timeout)

    if (status == 0 && output.nonEmpty) {
      parseSatResponse(output.last)
    }

    // In case of timeout
    Unknown
  }

  override def getModel(timeout: Int): (CheckSatResponse, scala.Option[GetModelResponse]) = {
    addCommand(CheckSat)
    addCommand(GetModel)

    val (status, output) = execute(timeout)

    if (status == 0 && output.nonEmpty) {
      val sat = parseSatResponse(output.head)
      val model = None // TODO: Some(modelparsing)

      (sat, model)
    }

    // In case of timeout
    (Unknown, None)
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