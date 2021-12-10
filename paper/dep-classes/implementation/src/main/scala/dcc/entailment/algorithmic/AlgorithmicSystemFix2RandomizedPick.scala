package dcc.entailment.algorithmic

import dcc.entailment.EntailmentSort.EntailmentSort
import dcc.entailment.{Entailment, EntailmentSort}
import dcc.syntax.{Constraint, ConstraintEntailment, FieldPath, InstanceOf, InstantiatedBy, PathEquivalence}
import dcc.syntax.Program.{GetMatchingConstraintEntailments, Program}

import scala.util.Random

// TODO: add debug output?
class AlgorithmicSystemFix2RandomizedPick(program: Program, debug: Int = 0) extends Entailment {
  override def typ: EntailmentSort = EntailmentSort.AlgorithmicFix2RandomizedPick

  override def entails(context: List[Constraint], constraints: List[Constraint]): Boolean = constraints.forall(entails(context, _))

  override def entails(context: List[Constraint], constraint: Constraint): Boolean = search(context, constraint, Nil, Nil, Nil, Nil, Nil, Nil)

  // TODO: keep track of already "visited" conclusions? o/w it might happen to test for the same one again leading to non-terminating behaviour
  //  do this per rule, as they are not mutually exclusive
  //  ↑ also memoize if the corresponding branch could be closed successfully?
  // TODO: consolidate cases (instance of checks)
  def search(context: List[Constraint], conclusion: Constraint,
             visitedClass: List[Constraint],
             visitedProg: List[Constraint],
             visitedSubst1: List[Constraint],
             visitedSubst2: List[Constraint],
             visitedSubst3: List[Constraint],
             visitedSubst4: List[Constraint]): Boolean =
    conclusion match { // Check for closure
      // CA-Refl
      case PathEquivalence(p, q) if p==q => true
      // CA-Ident
      case _ if context.contains(conclusion) => true
      // No closure. Search!
      case _ =>
        // CA-Class
        if (!visitedClass.contains(conclusion) && conclusion.isInstanceOf[InstanceOf]) {
          val InstanceOf(p, cls) = conclusion

          if(search(context, InstantiatedBy(p, cls), conclusion::visitedClass, visitedProg, visitedSubst1, visitedSubst2, visitedSubst3, visitedSubst4))
            return true
        }

        // CA-Prog
        if (!visitedProg.contains(conclusion) && conclusion.isInstanceOf[InstanceOf]) {
          val InstanceOf(p, cls) = conclusion

          if (context.flatMap(_.containedPaths).contains(p)) {
            // find matching constraint entailment declarations
            for (ConstraintEntailment(x, as, _) <- GetMatchingConstraintEntailments(program, cls)) {
//            for (elem@ConstraintEntailment(x, as, _) <- GetMatchingConstraintEntailments(program, cls)) {
//                            println(s"want to show '$conclusion'")
//                            println(s"found matching declaration '$elem'")
              if (as.exists(c => search(context, dcc.Util.substitute(x, p, c), visitedClass, conclusion::visitedProg, visitedSubst1, visitedSubst2, visitedSubst3, visitedSubst4))) {
                return true
              }
            }
          }
        }

        // CA-Subst1
        if (!visitedSubst1.contains(conclusion) && conclusion.isInstanceOf[InstantiatedBy]) {
          val InstantiatedBy(p1, cls) = conclusion

          for (p <- Random.shuffle(context.flatMap(_.containedPaths))) {
            if (search(context, PathEquivalence(p, p1), visitedClass, visitedProg, conclusion::visitedSubst1, visitedSubst2, visitedSubst3, visitedSubst4) && // TODO: add to visited in this as well?
              search(context, InstantiatedBy(p, cls), visitedClass, visitedProg, conclusion::visitedSubst1, visitedSubst2, visitedSubst3, visitedSubst4)
            ) {
              return true
            }
          }
        }

        // CA-Subst2
        if (!visitedSubst2.contains(conclusion) && conclusion.isInstanceOf[InstanceOf]) {
          val InstanceOf(p1, cls) = conclusion

          for (p <- Random.shuffle(context.flatMap(_.containedPaths))) {
            if (search(context, PathEquivalence(p, p1), visitedClass, visitedProg, visitedSubst1, conclusion::visitedSubst2, visitedSubst3, visitedSubst4) &&
              search(context, InstanceOf(p, cls), visitedClass, visitedProg, visitedSubst1, conclusion::visitedSubst2, visitedSubst3, visitedSubst4)
            ) {
              return true
            }
          }
        }

        // CA-Subst3
        if (!visitedSubst3.contains(conclusion) && conclusion.isInstanceOf[PathEquivalence]) {
          val PathEquivalence(p1, p2) = conclusion

          for (p <- Random.shuffle(context.flatMap(_.containedPaths))) {
            if (search(context, PathEquivalence(p, p1), visitedClass, visitedProg, visitedSubst1, visitedSubst2, conclusion::visitedSubst3, visitedSubst4) &&
              search(context, PathEquivalence(p, p2), visitedClass, visitedProg, visitedSubst1, visitedSubst2, conclusion::visitedSubst3, visitedSubst4)
            ) {
              return true
            }
          }
        }

        // CA-Subst4
        if (!visitedSubst4.contains(conclusion) && conclusion.isInstanceOf[PathEquivalence]) {
          val PathEquivalence(pf1, pf2) = conclusion

          if (pf1.isInstanceOf[FieldPath] && pf2.isInstanceOf[FieldPath]) {
            val FieldPath(p1, f) = pf1
            val FieldPath(p2, g) = pf2

            if (f==g &&
              search(context, PathEquivalence(p1, p2), visitedClass, visitedProg, visitedSubst1, visitedSubst2, visitedSubst3, conclusion::visitedSubst4)
            ) {
              return true
            }
          }
        }

        // No solution
        false
    }
}
