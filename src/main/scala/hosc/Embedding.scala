package hosc

import HLanguage._
import TermAlgebra._

trait Embedding {
  def he(e1: Expression, e2: Expression): Boolean
  def heByCoupling(e1: Expression, e2: Expression): Boolean
  def heByDiving(e1: Expression, e2: Expression): Boolean
}

object EmbeddingNaiveBinary extends Embedding {
  def he(e1: Expression, e2: Expression): Boolean =
    heByCoupling(e1, e2) || heByDiving(e1, e2)

  def heByDiving(term1: Expression, term2: Expression): Boolean = term2 match {
    case Constructor(_, args) => args exists (he(term1, _))
    case LambdaAbstraction(v, t) => he(term1, t)
    case Application(e1, e2) => he(term1, e1) || he(term1, e2)
    case CaseExpression(sel, bs) => he(term1, sel) || (bs exists { b => he(term1, b.term) })
    case _ => false
  }

  def heByCoupling(term1: Expression, term2: Expression): Boolean = (term1, term2) match {

    case (v1: Variable, v2: Variable) =>
      (v1.global == false && v2.global == false) || (v1.global == true && v2.global == true && v1.name == v2.name)

    case (Constructor(name1, args1), Constructor(name2, args2)) =>
      name1 == name2 && ((args1 zip args2) forall { case (a1, a2) => he(a1, a2) })

    case (LambdaAbstraction(v1, t1), LambdaAbstraction(v2, t2)) =>
      he(t1, t2)

    case (Application(head1, arg1), Application(head2, arg2)) =>
      he(head1, head2) && he(arg1, arg2)

    case (CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) => {
      val bs1_ = bs1 sort compareB
      val bs2_ = bs2 sort compareB
      val samePatterns = (bs1_ map (_.pattern.name)) == (bs2_ map (_.pattern.name))
      samePatterns && he(sel1, sel2) &&
        ((bs1_ zip bs2_) forall (bs => bs._1.pattern.name == bs._2.pattern.name && he(bs._1.term, bs._2.term)))
    }

    case _ => false

  }
}