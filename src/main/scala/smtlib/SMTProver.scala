package smtlib

import java.io.{File, PrintWriter}

import smtlib.syntax._
import smtlib.syntax.Implicit._

object SMTProver extends App{
  val proverCommand = findeBinary()
  val start = System.nanoTime
  val tempInFile = File.createTempFile("smtfile", "in")
  val tempOutFile = File.createTempFile("smtfile", "out")

  val const1 = DeclareConst("a", Sort("Int"))
  val const2 = DeclareConst("b", Sort("Int"))
  val const3 = DeclareConst("c", Sort("Int"))
  val ab = Apply("+", Seq("a", "b"))
  val aa = Apply("+", Seq("a", "a"))
  val bb = Apply("+", Seq("b", "b"))
  val abIsC = Eq(ab, "c")
  val aaIsC = Eq(aa, "c")
  val bbIsC = Eq(bb, "c")
  val assert1 = Assert(abIsC)
  val assert2 = Assert(aaIsC)
  val assert3 = Assert(bbIsC)
  val distinct = Assert(Distinct("a", "b"))
  val checksat = CheckSat
  val script = SMTLibScript(Seq(const1, const2, const3, distinct, assert1, assert2, assert3, checksat))

  script.commands.foreach(c => println(c.format))

  new PrintWriter(tempInFile) {
    script.commands.foreach(c => write(c.format() ++ "\n"))
    close
  }

  import scala.sys.process._

  val call = makeCall(tempInFile, 1000)
  val p = call.run()

  import scala.concurrent._
  import ExecutionContext.Implicits.global
  val f = Future(blocking(p.exitValue()))

  try {
    val code = Await.result(f, duration.Duration(40, "sec"))
    val end = System.nanoTime
  } catch {
    case _: TimeoutException =>
      p.destroy()
      println("timout")
  }



  private def makeCall(file: File, timeout: Int) = {
    var call = Seq(proverCommand.getAbsolutePath)

    if (timeout > 0) {
      call = call :+ s"-t:${timeout.toString}"
      call = call :+"-T:30"
    }

    call = call :+ "-smt2"

    call = call :+ file.getAbsolutePath
    call
  }

  private def findeBinary(): File = {
    for (p <- System.getenv(("PATH")).split(File.pathSeparator);
         f = new File(p, "z3") if f.exists && f.canExecute)
      return f
    null
  }
}
