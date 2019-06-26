package smtlib.solver

import java.io.PrintWriter

import smtlib.syntax.{CheckSatResponse, GetModelResponse, Unknown}
import smtlib.{SMTLibCommand, SMTLibScript}

import scala.concurrent._
import scala.io.Source
import scala.sys.process._
import ExecutionContext.Implicits.global

class Vampire(val axioms: SMTLibScript, val options: Seq[SMTLibCommand] = Seq.empty, var debug: Boolean = false) extends SMTSolver {
  var commands: Seq[SMTLibCommand] = Seq.empty

  // TODO: see sylvias type-pragmatics for reference
  private def makeCall(): Seq[String] = Seq("vampire", "--input_syntax", "smtlib2")

  override def execute(timeout: Int): (Int, Seq[String]) = {
    val call = makeCall()
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

  // TODO
  override def checksat(timeout: Int): CheckSatResponse = {
    Unknown
  }

  // TODO
  override def getModel(timeout: Int): (CheckSatResponse, Option[GetModelResponse]) = (Unknown, None)

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