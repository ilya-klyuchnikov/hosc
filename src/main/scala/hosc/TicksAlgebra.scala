package hosc
import hosc.HLanguage._

object TicksAlgebra {
  // tests whether e1 is an improvement of e2
  // e1 <= e2 in terms of ticks
  def isImprovement(e1: Expression, e2: Expression): Boolean = {
    if (e1.ticks > e2.ticks) {
      return false
    }
    (e1, e2) match {
      case (Variable(_), Variable(_)) => true
      case (Constructor(_, args1), Constructor(_, args2)) =>
        (args1 zip args2) forall {case (a1, a2) => isImprovement(a1, a2)}
      case (LambdaAbstraction(_, b1), LambdaAbstraction(_, b2)) =>
        isImprovement(b1, b2)
      case (Application(h1, a1), Application(h2, a2)) =>
        isImprovement(a1, a2) && isImprovement(h1, h2)
      case (LetRecExpression((_, b1), e1), LetRecExpression((_, b2), e2)) =>
        isImprovement(e1, e2) && isImprovement(b1, b2)
      case (CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) => {
        val bs1_ = bs1 sortWith TermAlgebra.compareB
        val bs2_ = bs2 sortWith TermAlgebra.compareB
        isImprovement(sel1, sel2) &&
          ((bs1_ zip bs2_) forall (bs => bs._1.pattern.name == bs._2.pattern.name && isImprovement(bs._1.term, bs._2.term)))
      }
      case _ => false
    }
  }
}
