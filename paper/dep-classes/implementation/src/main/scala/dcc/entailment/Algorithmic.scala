package dcc.entailment
import dcc.syntax.{Constraint, FieldPath, InstanceOf, InstantiatedBy, PathEquivalence}

class Algorithmic extends Entailment {
  override def entails(context: List[Constraint], constraints: List[Constraint]): Boolean = constraints.forall(entails(context, _))

  override def entails(context: List[Constraint], constraint: Constraint): Boolean = search(context, constraint)

  // TODO: keep track of already "visited" conclusions? o/w it might happen to test for the same one again leading to non-terminating behaviour
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
      if (conclusion.isInstanceOf[InstantiatedBy]) {
        val InstantiatedBy(p1, cls) = conclusion

        for (p <- context.flatMap(_.containedPaths)) {
          if (search(context, PathEquivalence(p, p1)) &&
            search(context, InstantiatedBy(p, cls))
          ) {
            return true
          }
        }
      }

      // CA-Subst2
      if (conclusion.isInstanceOf[InstanceOf]) {
        val InstanceOf(p1, cls) = conclusion

        for (p <- context.flatMap(_.containedPaths)) {
          if (search(context, PathEquivalence(p, p1)) &&
            search(context, InstanceOf(p, cls))
          ) {
            return true
          }
        }
      }

      // CA-Subst3
      if (conclusion.isInstanceOf[PathEquivalence]) {
        val PathEquivalence(p1, p2) = conclusion

        for (p <- context.flatMap(_.containedPaths)) {
          if (search(context, PathEquivalence(p1, p)) &&
            search(context, PathEquivalence(p, p2))
          ) {
            return true
          }
        }
      }

      // CA-Subst4
      if (conclusion.isInstanceOf[PathEquivalence]) {
        val PathEquivalence(pf1, pf2) = conclusion

        if (pf1.isInstanceOf[FieldPath] && pf2.isInstanceOf[FieldPath]) {
          val FieldPath(p1, f) = pf1
          val FieldPath(p2, g) = pf2

          if (f==g &&
            search(context, PathEquivalence(p1, p2))
          ) {
            return true
          }
        }
      }

      // No solution
      false
  }
}
