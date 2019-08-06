package hosc

import HLanguage._
import TermAlgebra._

object HE {
  def he(term1: Expression, term2: Expression): Boolean = he(term1, term2, Nil)

  def heByCoupling(term1: Expression, term2: Expression): Boolean = heByCoupling(term1, term2, Nil)

  private def he(term1: Expression, term2: Expression, binders: List[(Variable, Variable)]): Boolean =
    heByDiving(term1, term2, binders) || heByCoupling(term1, term2, binders)

  def heByDiving(term1: Expression, term2: Expression, binders: List[(Variable, Variable)]): Boolean = {
    val term1Vars = getFreeVars(term1)
    for ((b1, b2) <- binders) if ((b1 != null) && (term1Vars contains b1)) return false

    term2 match {
      case Constructor(_, args) =>
        args exists (he(term1, _, binders))

      case LambdaAbstraction(v, t) =>
        he(term1, t, (null, v) :: binders)

      // subtle point
      case Application(head@Application(_, _), arg) =>
        heByDiving(term1, head, binders) || he(term1, arg, binders)
      case Application(head, arg) =>
        he(term1, head, binders) || he(term1, arg, binders)

      case CaseExpression(sel, bs) =>
        he(term1, sel, binders) || (bs exists { b => he(term1, b.term, (b.pattern.args map { (null, _) }) ::: binders) })

      case _ => false
    }
  }

  def heByCoupling(term1: Expression, term2: Expression, binders: List[(Variable, Variable)]): Boolean =
    (term1, term2) match {
      case (v1: Variable, v2: Variable) => (v1.global == true && v2.global == true && v1.name == v2.name) ||
        (v1.global == false && v2.global == false) &&
        ((binders exists { case (b1, b2) => b1 == v1 && b2 == v2 }) || (binders forall { case (b1, b2) => b1 != v1 && b2 != v2 }))

      case (Constructor(name1, args1), Constructor(name2, args2)) if name1 == name2 =>
        (args1 zip args2) forall (args => he(args._1, args._2, binders))

      case (LambdaAbstraction(v1, t1), LambdaAbstraction(v2, t2)) =>
        he(t1, t2, (v1, v2) :: binders)

      // subtle point
      case (a1: Application, a2: Application) =>
        heByCoupling(a1.head, a2.head, binders) && he(a1.arg, a2.arg, binders)

      case (CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) => {
        val bs1_ = bs1 sortWith compareB
        val bs2_ = bs2 sortWith compareB
        val samePatterns = (bs1_ map (_.pattern.name)) == (bs2_ map (_.pattern.name))
        samePatterns && he(sel1, sel2, binders) &&
          ((bs1_ zip bs2_) forall (bs => bs._1.pattern.name == bs._2.pattern.name &&
            he(bs._1.term, bs._2.term, (bs._1.pattern.args zip bs._2.pattern.args) ::: binders)))
      }

      case _ => false
    }
}
