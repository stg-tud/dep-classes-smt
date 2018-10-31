package smtlib.solver

import java.io.File

import smtlib.{SMTLibCommand, SMTLibScript, syntax}
import smtlib.syntax._
import smtlib.syntax.Implicit._

import scala.sys.process._

object ProcessIoTest extends App {
  val const1 = DeclareConst("a", "Int")
  val const2 = DeclareConst("b", "Int")
  val const3 = DeclareConst("c", "Int")
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

  val z3Solver: Z3Solver = new Z3Solver
  z3Solver.addScript(script)
  z3Solver.execute()

  z3Solver.flush()

  z3Solver.addCommand(const1)
  z3Solver.addCommand(const2)
  z3Solver.addCommand(const3)
  z3Solver.addCommands(Seq(assert1, assert2, assert3))
  z3Solver.addCommand(Assert(Not(Eq("a", Numeral(0)))))
  z3Solver.addCommand(checksat)
  z3Solver.addCommand(GetModel)
  z3Solver.execute()
}

object DCCVariableEncodingTest extends App {
  val solver: SMTSolver = new Z3Solver

//  (declare-datatypes (T)
//    ( (Lst
//        nil
//        (cons
//          (hd T)
//          (tl Lst)
//        )
//      )
//    )
//  )


// (declare-datatypes (T) (( Lst   nil  (cons (hd T) (tl Lst)) )))
// (declare-datatypes (T) (( Lst   nil  (cons (hd T) (tl Lst)) )))
// (declare-datatypes (T) ((      (nil) (cons (hd T) (tl Lst)))))
//(declare-const l1 (Lst Int))
//(declare-const l2 (Lst Bool))
//  val listType: SMTLibCommand = DeclareDatatypes(Seq("T"),
//                                                 Seq(ConstructorDatatype(Seq(
//                                                   ConstructorDec("Lst", Seq()),
//                                                   ConstructorDec("nil", Seq()),
//                                                   ConstructorDec("cons", Seq(
//                                                     SelectorDec("hd", "T"),
//                                                     SelectorDec("tl", "Lst")
//                                                   ))
//                                                 ))))

// (declare-datatypes (T) ((    (nil ) (cons (hd T) (tl Lst)))))
//  val listType: SMTLibCommand = DeclareDatatypes(Seq("T"),
//    Seq(ConstructorDatatype(Seq(
//      ConstructorDec("nil", Seq()),
//      ConstructorDec("cons", Seq(
//        SelectorDec("hd", "T"),
//        SelectorDec("tl", "Lst")
//      ))
//    ))))

// (declare-datatypes (T1 T2) ((Pair (mk-pair (first T1) (second T2)))))
// (declare-datatypes (T1 T2) ((     (mk-pair (first T1) (second T2)))))
//  val recordType = DeclareDatatypes(Seq("T1", "T2"),
//    Seq(ConstructorDatatype(Seq(
//      ConstructorDec("mk-pair", Seq(
//        SelectorDec("first", "T1"),
//        SelectorDec("second", "T2")
//      ))
//    )
//    )))

//  (declare-datatypes ( (List 1) ) ((par (T) ( (nil) (cons (hd T) (tl (List T)) )))))
//  (declare-datatypes ( (List 1) ) ((par (T) ( (nil) (cons (hd T) (tl (List T)) )))))
  val listType = DeclareDatatypes(
                  Seq(SortDec("Lst", 1)),
                  Seq(ParDatatype(
                    Seq("T"), Seq(
                      ConstructorDec("nil", Seq()),
                      ConstructorDec("cons", Seq(
                        SelectorDec("hd", "T"),
                        SelectorDec("tl", Sorts("Lst", Seq("T")))
                      ))
                    ))))

  println(listType.format())

  val l1 = DeclareConst("l1", Sorts("Lst", Seq("Int")))
  val l2 = DeclareConst("l1", Sorts("Lst", Seq(Bool)))

  val l1NotEmtpy = Assert(Not(Eq(IdentifierAs("l1", Sorts("Lst", Seq("Int"))), IdentifierAs("nil", Sorts("Lst", Seq("Int"))))))

  solver.addCommand(listType)
  solver.addCommand(l1)
  solver.addCommand(l2)
  solver.addCommand(l1NotEmtpy)
  solver.addCommand(CheckSat)
  solver.addCommand(GetModel)
  solver.execute()
}


import scala.sys.process.ProcessIO
import java.io.InputStream
import java.io.OutputStream
import java.util.Scanner
import scala.sys.process.Process

object TestProcessIO extends App {
  def feedInput(in: OutputStream): Unit = {
    for (ln <- io.Source.stdin.getLines) {
      print("> ")
      in.write((ln + "\n").toCharArray().map(_.toByte))
      in.flush // very important
    }
  }
  def processOutput(pref: String)(os: InputStream): Unit = {
    val sc = new Scanner(os)
    while (true) {
      println(pref + sc.nextLine)
    }
  }
  val pb = Process("/bin/bash")
  val pio = new ProcessIO(feedInput,
    processOutput("OUT:"), processOutput("ERR:"))
  pb.run(pio) // don't wait


}


import scala.io.Source

object SimpleProcessExample extends App {

  def out = (output: java.io.OutputStream) => {
    output.flush()
    output.close()
  }

  def in = (input: java.io.InputStream) => {
    println("Stdout: " + Source.fromInputStream(input).mkString)
    input.close()
  }

  // limit scope of any temporary variables
  locally {
    val calcCommand = "bc"
    // strings are implicitly converted to ProcessBuilder
    // via scala.sys.process.ProcessImplicits.stringToProcess(_)
    val calcProc = calcCommand.run(new ProcessIO(
      // Handle subprocess's stdin
      // (which we write via an OutputStream)
      in => {
        val writer = new java.io.PrintWriter(in)
        writer.println("1 + 2")
        writer.println("3 + 4")
        writer.close()
      },
      // Handle subprocess's stdout
      // (which we read via an InputStream)
      out => {
        val src = scala.io.Source.fromInputStream(out)
        for (line <- src.getLines()) {
          println("Answer: " + line)
        }
        src.close()
      },
      // We don't want to use stderr, so just close it.
      _.close()
    ))

    // Using ProcessBuilder.run() will automatically launch
    // a new thread for the input/output routines passed to ProcessIO.
    // We just need to wait for it to finish.

    val code = calcProc.exitValue()

    println(s"Subprocess exited with code $code.")

  }
}

object demo extends App {
  import scala.sys.process._

  def getIO = {
    // create piped streams that can attach to process streams:
    val procInput = new java.io.PipedOutputStream()
    val procOutput = new java.io.PipedInputStream()
    val io = new ProcessIO(
      // attach to the process's internal input stream
      { in =>
        val istream = new java.io.PipedInputStream(procInput)
        val buf = Array.fill(100)(0.toByte)
        var br = 0
        while (br >= 0) {
          br = istream.read(buf)
          if (br > 0) { in.write(buf, 0, br) }
        }
        in.close()
      },
      // attach to the process's internal output stream
      { out =>
        val ostream = new java.io.PipedOutputStream(procOutput)
        val buf = Array.fill(100)(0.toByte)
        var br = 0
        while (br >= 0) {
          br = out.read(buf)
          if (br > 0) { ostream.write(buf, 0, br) }
        }
        out.close()
      },
      // ignore stderr
      { err => () }
    )
    // run the command with the IO object:
    val cmd = List("awk", "{ print $1 + $2 }")
    val proc = cmd.run(io)

    // wrap the raw streams in formatted IO objects:
    val procO = new java.io.BufferedReader(new java.io.InputStreamReader(procOutput))
    val procI = new java.io.PrintWriter(procInput, true)
    (procI, procO)
  }

  val (in, out) = getIO
  in.println("1 2")
  println(out.readLine())

  in.println("2 3")
  println(out.readLine())

  in.close()
  out.close()
}