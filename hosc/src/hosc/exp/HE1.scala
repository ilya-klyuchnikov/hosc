package hosc.exp

import hosc.HLanguage._
import hosc.TermAlgebra._

object HE1 {
  def he(term1: Expression, term2: Expression): Boolean = he(term1, term2, Nil, Map())
  def heByCoupling(term1: Expression, term2: Expression): Boolean = heByCoupling(term1, term2, Nil, Map())
  
  private def he(term1: Expression, term2: Expression, binders: List[Tuple2[Variable, Variable]], 
      letrecs: Map[Variable, Variable]): Boolean = 
    heByVar(term1, term2, binders, letrecs) || 
      heByDiving(term1, term2, binders, letrecs) || 
        heByCoupling(term1, term2, binders, letrecs)
  
  private def heByVar(term1: Expression, term2: Expression, 
      binders: List[Tuple2[Variable, Variable]], letrecs: Map[Variable, Variable]): Boolean = 
    (term1, term2) match {
    case (v1: Variable, v2: Variable) => true 
      //(letrecs.contains(v1) && letrecs(v1) == v2) ||
      /*(!letrecs.keys.contains(v1) && !letrecs.values.contains(v2)) && 
        ((binders exists {case (b1, b2) => b1 == v1 && b2 == v2}) || (binders forall {case (b1, b2) => b1 != v1 && b2 != v2}))*/
    case _ => false
  }
  
  private def heByDiving(term1: Expression, term2: Expression, 
      binders: List[Tuple2[Variable, Variable]], letrecs: Map[Variable, Variable]): Boolean = {
    //val term1Vars = Set(getVarsOrdered(term1):_*) // ??? maybe free vars?
    //for ((b1, b2) <- binders) if ((b1 != null) && (term1Vars contains b1)) return false
    term2 match {
      case Constructor(_, args) => args exists (he(term1, _, binders, letrecs))
      case LambdaAbstraction(v, t) => he(term1, t, (null, v)::binders, letrecs)
      case Application(h, a) => he(term1, h, binders, letrecs) || he(term1, a, binders, letrecs) 
      case CaseExpression(sel, bs) => he(term1, sel, binders, letrecs) || 
        (bs exists {b => he(term1, b.term, (b.pattern.args map {(null, _)}) ::: binders, letrecs)})
      case LetRecExpression((v, t), e) => he(term1, e, binders, letrecs) || he(term1, t, binders, letrecs) // seems to be correct (?? binders??)
      case _ => false
    }
  }  
  
  private def heByCoupling(term1: Expression, term2: Expression, 
      binders: List[Tuple2[Variable, Variable]], letrecs: Map[Variable, Variable]): Boolean = 
    (term1, term2) match {
    case (Constructor(name1, args1), Constructor(name2, args2)) if name1 == name2 => 
      (args1 zip args2) forall (args => he(args._1, args._2, binders, letrecs))
    case (LambdaAbstraction(v1, t1), LambdaAbstraction(v2, t2)) => he(t1, t2, (v1, v2)::binders, letrecs)
    case (Application(h1, a1), Application(h2, a2)) => {
      he(h1, h2, binders, letrecs) && he(a1, a2, binders, letrecs) 
    }
    case (CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) => {
      val bs1_ = bs1 sort compareB
      val bs2_ = bs2 sort compareB
      val samePatterns = (bs1_ map (_.pattern.name)) == (bs2_ map (_.pattern.name))
      samePatterns && he(sel1, sel2, binders, letrecs) && 
        ((bs1_ zip bs2_) forall (bs => bs._1.pattern.name == bs._2.pattern.name && 
          he(bs._1.term, bs._2.term, (bs._1.pattern.args zip bs._2.pattern.args) ::: binders, letrecs)))
    }
    case (LetRecExpression((f1, a1), e1), LetRecExpression((f2, a2), e2)) =>
      he(e1, e2, binders, letrecs + (f1 -> f2)) && he(a1, a2, binders, letrecs + (f1 -> f2)) // maybe || ?
    case _ => false
  }

}
