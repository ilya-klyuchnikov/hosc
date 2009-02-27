package hosc.sc0

import HLanguage._
import TermAlgebra0._

object HE0 {
  def he(term1: Expression, term2: Expression): Boolean = he(term1, term2, Nil)
  
  def heByCoupling(term1: Expression, term2: Expression): Boolean = heByCoupling(term1, term2, Nil)
  
  def strictHe(term1: Expression, term2: Expression) = heByDiving(term1, term2, Nil)
  
  private def he(term1: Expression, term2: Expression, binders: List[Tuple2[Variable, Variable]]): Boolean = 
    heByVar(term1, term2, binders) || heByDiving(term1, term2, binders) || heByCoupling(term1, term2, binders)
  
  private def heByVar(term1: Expression, term2: Expression, binders: List[Tuple2[Variable, Variable]]): Boolean = 
    (term1, term2) match {
    case (v1: Variable, v2: Variable) => (v1.global == true && v2.global == true && v1.name == v2.name) ||
      (v1.global == false && v2.global == false) && 
        ((binders exists {p => p._1 == v1 && p._2 == v2}) || (binders forall {p => p._1 != v1 && p._2 != v2}))
    case _ => false
  }
  
  private def heByDiving(term1: Expression, term2: Expression, binders: List[Tuple2[Variable, Variable]]): Boolean = { 
    val term1Vars = getFreeVars(term1)
    for (p <- binders) if (term1Vars contains p._1) return false
    term2 match {
      case Constructor(_, args) => args exists (he(term1, _, binders))
      case LambdaAbstraction(v, t) => he(term1, t, (null, v)::binders)
      case a: Application => lineApp(a) exists (he(term1, _, binders))
      case CaseExpression(sel, bs) => he(term1, sel, binders) || (bs exists {b => he(term1, b.term, (b.pattern.args map {(null,_)}) ::: binders)})
      case _ => false
    }
  }
  
  private def heByCoupling(term1: Expression, term2: Expression, binders: List[Tuple2[Variable, Variable]]): Boolean = 
    (term1, term2) match {
    case (Constructor(name1, args1), Constructor(name2, args2)) if name1 == name2 => 
      (args1 zip args2) forall (args => he(args._1, args._2, binders))
    case (LambdaAbstraction(v1, t1), LambdaAbstraction(v2, t2)) => he(t1, t2, (v1, v2)::binders)
    case (a1: Application, a2: Application) => {
      val (line1, line2) = (lineApp(a1), lineApp(a2)) 
      line1.length == line2.length && ((line1 zip line2) forall (args => he(args._1, args._2, binders)))
    }
    case (CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) => {
      val bs1_ = bs1 sort compareB
      val bs2_ = bs2 sort compareB
      he(sel1, sel2, binders) && 
        ((bs1_ zip bs2_) forall (bs => bs._1.pattern.name == bs._2.pattern.name && 
          he(bs._1.term, bs._2.term, (bs._1.pattern.args zip bs._2.pattern.args) ::: binders)))
    }
    case _ => false
  }
}
