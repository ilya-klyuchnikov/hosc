package hosc

import HLanguage._
import TermAlgebra._

// fast or classic homeomorhic embedding
// it process all non-global variables in a uniform way
// so bound and free variables are considered to be the same!
object HE0 {
  
  def strictHe(term1: Expression, term2: Expression) = heByDiving(term1, term2)
  
  private def he(term1: Expression, term2: Expression): Boolean = 
    heByVar(term1, term2) || heByDiving(term1, term2) || heByCoupling(term1, term2)
  
  private def heByVar(term1: Expression, term2: Expression): Boolean = (term1, term2) match {
    case (v1: Variable, v2: Variable) => 
      (v1.global == false && v2.global == false) || (v1.global == true && v2.global == true && v1.name == v2.name)
    case _ => false
  }
  
  private def heByDiving(term1: Expression, term2: Expression): Boolean = term2 match {
    case Constructor(_, args) => args exists (he(term1, _))
    case LambdaAbstraction(v, t) => he(term1, t)
    case a: Application => lineApp(a) exists (he(term1, _))
    case CaseExpression(sel, bs) => he(term1, sel) || (bs exists {b => he(term1, b.term)})
    case Choice(e1, e2) => he(term1, e1) || he(term1, e2)
    case _ => false
  }
  
  def heByCoupling(term1: Expression, term2: Expression): Boolean = (term1, term2) match {
    case (Constructor(name1, args1), Constructor(name2, args2)) => 
      name1 == name2 && ((args1 zip args2) forall {case (a1, a2) => he(a1, a2)})
    case (LambdaAbstraction(v1, t1), LambdaAbstraction(v2, t2)) => he(t1, t2)
    case (a1: Application, a2: Application) => {
      val (line1, line2) = (lineApp(a1), lineApp(a2)) 
      line1.length == line2.length && ((line1 zip line2) forall (args => he(args._1, args._2)))
    }
    case (CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) => {
      val bs1_ = bs1 sort compareB
      val bs2_ = bs2 sort compareB
      val samePatterns = (bs1_ map (_.pattern.name)) == (bs2_ map (_.pattern.name))
      samePatterns && he(sel1, sel2) && 
        ((bs1_ zip bs2_) forall (bs => bs._1.pattern.name == bs._2.pattern.name && he(bs._1.term, bs._2.term)))
    }
    case (Choice(e1a, e2a), Choice(e1b, e2b)) => he(e1a, e1b) && he(e2a, e2b)
    case _ => false
  }
}