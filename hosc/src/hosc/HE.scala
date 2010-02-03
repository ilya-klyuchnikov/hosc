package hosc

import HLanguage._
import TermAlgebra._

object HE {
  def he(term1: Expression, term2: Expression): Boolean = he(term1, term2, Nil)
  
  def heByCoupling(term1: Expression, term2: Expression): Boolean = heByCoupling(term1, term2, Nil)
  
  def strictHe(term1: Expression, term2: Expression) = heByDiving(term1, term2, Nil)
  
  private def he(term1: Expression, term2: Expression, binders: List[(Variable, Variable)]): Boolean = 
    heByDiving(term1, term2, binders) || heByCoupling(term1, term2, binders)
  
  private def heByDiving(term1: Expression, term2: Expression, binders: List[(Variable, Variable)]): Boolean = { 
    val term1Vars = getFreeVars(term1)
    for ((b1, b2) <- binders) if ((b1 != null) && (term1Vars contains b1)) return false
    term2 match {
      case Constructor(_, args) => args exists (he(term1, _, binders))
      case LambdaAbstraction(v, t) => he(term1, t, (null, v)::binders)
      case a: Application => lineApp(a) exists (he(term1, _, binders))
      case CaseExpression(sel, bs) => 
        he(term1, sel, binders) || (bs exists {b => he(term1, b.term, (b.pattern.args map {(null,_)}) ::: binders)})
      case Choice(e1, e2) => he(term1, e1, binders) || he(term1, e2, binders)
      case _ => false
    }
  }
  
  private def heByCoupling(term1: Expression, term2: Expression, binders: List[(Variable, Variable)]): Boolean = 
    (term1, term2) match {
    case (v1: Variable, v2: Variable) => (v1.global == true && v2.global == true && v1.name == v2.name) ||
      (v1.global == false && v2.global == false) && 
        ((binders exists {case (b1, b2) => b1 == v1 && b2 == v2}) || (binders forall {case (b1, b2) => b1 != v1 && b2 != v2}))
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
      val samePatterns = (bs1_ map (_.pattern.name)) == (bs2_ map (_.pattern.name))
      samePatterns && he(sel1, sel2, binders) && 
        ((bs1_ zip bs2_) forall (bs => bs._1.pattern.name == bs._2.pattern.name && 
          he(bs._1.term, bs._2.term, (bs._1.pattern.args zip bs._2.pattern.args) ::: binders)))
    }
    case (Choice(e1a, e2a), Choice(e1b, e2b)) => he(e1a, e1b, binders) && he(e2a, e2b, binders)
    case _ => false
  }
}
