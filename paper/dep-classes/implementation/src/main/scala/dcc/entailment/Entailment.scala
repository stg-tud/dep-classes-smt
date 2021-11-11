package dcc.entailment

import dcc.entailment.EntailmentSort.EntailmentSort
import dcc.syntax.Constraint

trait Entailment {
  def entails(context: List[Constraint], constraint: Constraint): Boolean
  def entails(context: List[Constraint], constraints: List[Constraint]): Boolean

  def typ: EntailmentSort
}