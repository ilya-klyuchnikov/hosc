package hosc;

import HLanguage1._
import TermAlgebra1._

object HE1 {
  def he(term1: Term1, term2: Term1): Boolean = he(term1, term2, Nil, Map())
  def heByCoupling(term1: Term1, term2: Term1): Boolean = heByCoupling(term1, term2, Nil, Map())
  
  private def he(term1: Term1, term2: Term1, binders: List[Tuple2[Variable1, Variable1]], 
      letrecs: Map[Variable1, Variable1]): Boolean = 
    heByVar(term1, term2, binders, letrecs) || 
      heByDiving(term1, term2, binders, letrecs) || 
        heByCoupling(term1, term2, binders, letrecs)
  
  private def heByVar(term1: Term1, term2: Term1, 
      binders: List[Tuple2[Variable1, Variable1]], letrecs: Map[Variable1, Variable1]): Boolean = 
        if (term1.label != term2.label) false else
    (term1, term2) match {
    case (v1: Variable1, v2: Variable1) => 
      (v1.call == true && v2.call == true && v1.name == v2.name) ||
      (letrecs.contains(v1) && letrecs(v1)== v2) ||
      (v1.call == false && v2.call == false) && 
        ((binders exists {p => p._1 == v1 && p._2 == v2}) || (binders forall {p => p._1 != v1 && p._2 != v2}))
    case _ => false
  }
  
  private def heByDiving(term1: Term1, term2: Term1, 
      binders: List[Tuple2[Variable1, Variable1]], letrecs: Map[Variable1, Variable1]): Boolean = term1 match {
    case v: Variable1 if binders exists {v1 => v1._1 == v} => false
    case _ => term2 match {
      case Constructor1(_, args) => args exists (he(term1, _, binders, letrecs))
      case LambdaAbstraction1(v, t) => he(term1, t, (null, v)::binders, letrecs)
      case a: Application1 => lineApp(a) exists (he(term1, _, binders, letrecs))
      case CaseExpression1(sel, bs) => he(term1, sel, binders, letrecs) || 
        (bs exists {b => he(term1, b.term, binders, letrecs)})
      case LetRecExpression1(_, e) => he(term1, e, binders, letrecs)
      case _ => false
    }
  }  
  
  private def heByCoupling(term1: Term1, term2: Term1, 
      binders: List[Tuple2[Variable1, Variable1]], letrecs: Map[Variable1, Variable1]): Boolean = 
    if (term1.label != term2.label) false else (term1, term2) match {
    case (Constructor1(name1, args1), Constructor1(name2, args2)) if name1 == name2 => 
      (args1 zip args2) forall (args => he(args._1, args._2, binders, letrecs))
    case (LambdaAbstraction1(v1, t1), LambdaAbstraction1(v2, t2)) => he(t1, t2, (v1, v2)::binders, letrecs)
    case (a1: Application1, a2: Application1) => {
      val (line1, line2) = (lineApp(a1), lineApp(a2)) 
      line1.length == line2.length && ((line1 zip line2) forall (args => he(args._1, args._2, binders, letrecs)))
    }
    case (CaseExpression1(sel1, bs1), CaseExpression1(sel2, bs2)) => {
      val bs1_ = bs1 sort compareB
      val bs2_ = bs2 sort compareB
      he(sel1, sel2, binders, letrecs) && 
        ((bs1_ zip bs2_) forall (bs => bs._1.pattern.name == bs._2.pattern.name && 
          he(bs._1.term, bs._2.term, (bs._1.pattern.args zip bs._2.pattern.args) ::: binders, letrecs)))
    }
    case (LetRecExpression1((f1, a1), e1), LetRecExpression1((f2, a2), e2)) =>
      he(e1, e2, binders, letrecs + (f1 -> f2)) && heByCoupling(a1, a2, binders, letrecs + (f1 -> f2))
    case _ => false
  }
  
  private def lineApp(term: Term1): List[Term1] = term match {
    case Application1(h, a) => lineApp(h) ::: (a:: Nil)
    case t => t :: Nil
  }
}
