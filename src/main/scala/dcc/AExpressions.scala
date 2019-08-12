package dcc

import dcc.syntax.Util._
import dcc.syntax._
import dcc.syntax.Program.Program
import smtlib.solver.{Axioms, Z3Solver}
import smtlib.syntax.{Assert, CheckSat, Not}

object AExpressions extends App {
  val naturalNumbers: Program = List(
    ConstructorDeclaration(Id('Zero), Id('x), Nil),
    ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Zero))), InstanceOf(Id('x), Id('Nat))),
    ConstructorDeclaration(Id('Succ), Id('x), List(InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat)))),
    ConstraintEntailment(Id('x), List(InstanceOf(Id('x), Id('Succ)), InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat))), InstanceOf(Id('x), Id('Nat))),
    AbstractMethodDeclaration(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Nat))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat))))),
    MethodImplementation(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Zero))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat)))),
      ObjectConstruction(Id('Zero), Nil)),
    MethodImplementation(Id('prev), Id('x), List(InstanceOf(Id('x), Id('Succ)), InstanceOf(FieldPath(Id('x), Id('p)), Id('Nat))), Type(Id('y), List(InstanceOf(Id('y), Id('Nat)))),
      FieldAccess(Id('x), Id('p)))
  )

  val aexp: Program = List(
    ConstructorDeclaration('Lit, 'x, List(InstanceOf(FieldPath('x, 'value), 'Nat))),
    ConstructorDeclaration('Plus, 'x, List(InstanceOf(FieldPath('x, 'l), 'Exp), InstanceOf(FieldPath('x, 'r), 'Exp))),
    ConstraintEntailment('x, List(InstanceOf('x, 'Lit), InstanceOf(FieldPath('x, 'value), 'Nat)), InstanceOf('x, 'Exp)),
    ConstraintEntailment('x, List(InstanceOf('x, 'Neg), InstanceOf(FieldPath('x, 'l), 'Exp), InstanceOf(FieldPath('x, 'r), 'Exp)), InstanceOf('x, 'Exp)),
    AbstractMethodDeclaration('eval, 'x, List(InstanceOf('x, 'Exp)), Type('y, List(InstanceOf('y, 'Exp)))),
    MethodImplementation('eval, 'x, List(InstanceOf('x, 'Lit), InstanceOf(FieldPath('x, 'value), 'Nat)),
      Type('y, List(InstanceOf('y, 'Exp))), 'x),
    MethodImplementation('eval,
      'x, List(InstanceOf('x, 'Plus),
               InstanceOf(FieldPath('x, 'l), 'Lit),
               InstanceOf(FieldPath('x, 'r), 'Lit),
               InstanceOf(FieldPath(FieldPath('x, 'l), 'value), 'Nat),
               InstanceOf(FieldPath(FieldPath('x, 'r), 'value), 'Zero)),
      Type('y, List(InstanceOf('y, 'Exp))), FieldAccess('x, 'l)), // TODO: remove method call, x.l is direct result
    MethodImplementation('eval,
      'x, List(InstanceOf('x, 'Plus),
        InstanceOf(FieldPath('x, 'l), 'Lit),
        InstanceOf(FieldPath('x, 'r), 'Lit),
        InstanceOf(FieldPath(FieldPath('x, 'l), 'value), 'Nat),
        InstanceOf(FieldPath(FieldPath('x, 'r), 'value), 'Succ),
        InstanceOf(FieldPath(FieldPath(FieldPath('x, 'r), 'value), 'p), 'Nat)),
      Type('y, List(InstanceOf('y, 'Exp))),
      MethodCall('eval,
        ObjectConstruction('Plus, List(
          ('l, ObjectConstruction('Lit, List(
            ('value, ObjectConstruction('Succ, List(('p, FieldAccess(FieldAccess('x, 'l), 'value)))) )))),
          ('r, ObjectConstruction('Lit, List(
            ('value, MethodCall('prev, FieldAccess(FieldAccess('x, 'r), 'value)))
          )))))
      )),
    MethodImplementation('eval, 'x,
      List(
        InstanceOf('x, 'Plus),
        InstanceOf(FieldPath('x, 'l), 'Exp),
        InstanceOf(FieldPath('x, 'r), 'Plus),
          InstanceOf(FieldPath(FieldPath('x, 'r), 'l), 'Exp),
          InstanceOf(FieldPath(FieldPath('x, 'r), 'r), 'Exp)
      ),
      Type('y, List(InstanceOf('y, 'Exp))),
      MethodCall('eval,
        ObjectConstruction('Plus, List(
          ('l, FieldAccess('x, 'l)),
          ('r, MethodCall('eval, FieldAccess('x, 'r)))
        ))
      )),
    MethodImplementation('eval, 'x,
      List(
        InstanceOf('x, 'Plus),
        InstanceOf(FieldPath('x, 'l), 'Plus),
        InstanceOf(FieldPath(FieldPath('x, 'l), 'l), 'Exp),
        InstanceOf(FieldPath(FieldPath('x, 'l), 'r), 'Exp),
        InstanceOf(FieldPath('x, 'r), 'Exp)
      ),
      Type('y, List(InstanceOf('y, 'Exp))),
      MethodCall('eval,
        ObjectConstruction('Plus, List(
          ('l, MethodCall('eval, FieldAccess('x, 'l))),
          ('r, FieldAccess('x, 'r))
        ))
      ))
  )

  val program = naturalNumbers ++ aexp

  program.foreach(println)

  val dcc = new DCC(program)

  val zero = ObjectConstruction('Zero, List())
  val one = ObjectConstruction('Succ, List(('p, zero)))
  val two = ObjectConstruction('Succ, List(('p, one)))

  def lit(nat: Expression) = ObjectConstruction('Lit, List(('value, nat)))
  def plus(l: Expression, r: Expression) = ObjectConstruction('Plus, List(('l, l), ('r, r)))

  val (h0, x0) = dcc.interp(Map.empty, one)
  println("-------------------------------------------")
  val (h1, x1) = dcc.interp(h0, ObjectConstruction('Succ, List(('p, x0))), preOptimize = true)


  println("-------------------------------------------")
  println("Heap")
  h1.foreach(x => println(s"\t$x"))
  println(x1)

//  println("-------------------------------------------")
//  val entailment = SMTLibConverter.convertEntailment(List(InstanceOf('x2, 'Succ), InstanceOf(FieldPath('x2, 'p), 'Nat)), InstanceOf('x2, 'Nat))
//  val solver = new Z3Solver(Axioms.allDirectClosure)
//  solver.addCommands(SMTLibConverter.generateSubstRules(List('x2), List('x2, FieldPath('x2, 'p))))
//  solver.addCommand(SMTLibConverter.makeProgramEntailmentLookupFunction(program, List('x2, FieldPath('x2, 'p))))
//  solver.addCommand(Axioms.cProg)
//  solver.addCommand(Axioms.assertVariable("x2"))
//  solver.addCommand(Axioms.assertPath(SMTLibConverter.convertPath('x2)))
//  solver.addCommand(Axioms.assertPath(SMTLibConverter.convertPath(FieldPath('x2, 'p))))
//  solver.addCommand(Axioms.assertClass("Nat"))
//  solver.addCommand(Axioms.assertClass("Succ"))
//  solver.addCommand(Axioms.assertClass("Zero"))
//  solver.addCommand(Assert(Not(entailment)))
//  solver.addCommand(CheckSat)
//  val (_, out) = solver.execute()
//  out.foreach(println)
}


object Footest extends App{
  sealed trait Exp
  case class Lit(value: Int) extends Exp
  case class Plus(l: Exp, r: Exp) extends Exp

  def eval(e: Exp): Exp = e match {
    case Lit(i) => Lit(i)
    case Plus(Lit(i), Lit(0)) => Lit(i)
    case Plus(Lit(i), Lit(j)) => eval(Plus(Lit(i+1), Lit(j-1)))
    case Plus(sub, Plus(l, r)) => eval(Plus(sub, eval(Plus(l, r))))
    case Plus(Plus(l, r), sub) => eval(Plus(eval(Plus(l, r)), sub))
  }

  val one = Lit(1)
  val five = Lit(5)
  val plus = Plus(one, five)
  val plusFive = Plus(plus, five)

  println(eval(Plus(plus, plusFive)))
}