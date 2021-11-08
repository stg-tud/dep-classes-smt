package dcc.entailment
import dcc.syntax.{Constraint, InstanceOf, InstantiatedBy, PathEquivalence}

class Algorithmic extends Entailment {
  override def entails(context: List[Constraint], constraints: List[Constraint]): Boolean = constraints.forall(entails(context, _))

  override def entails(context: List[Constraint], constraint: Constraint): Boolean = search(context, constraint)

  def search(context: List[Constraint], conclusion: Constraint): Boolean = conclusion match { // Check for closure
    // CA-Refl
    case PathEquivalence(p, q) if p==q => true
    // CA-Ident
    case _ if context.contains(conclusion) => true
    // No closure. Search!
    case _ =>
      // CA-Class
//      conclusion match {
//        case InstanceOf(p, cls) =>
//          val sub = search(context, InstantiatedBy(p, cls))
//        if (sub)
//          return true
//        case _ =>
//      }
      if (conclusion.isInstanceOf[InstanceOf]) {
        val InstanceOf(p, cls) = conclusion

        if(search(context, InstantiatedBy(p, cls))) // TODO: if needed or directly return recursive call? test if more rules are impl
          return true
      }

      // CA-Prog
      // TODO

      // CA-Subst1
      // TODO

      // CA-Subst2
      // TODO

      // CA-Subst3
      // TODO

      // CA-Subst4
      // TODO

      // No solution
      false
  }
}
