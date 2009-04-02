package hosc

import HLanguage._

object Eq {
  def equivalent(term1: Expression, term2: Expression): Boolean = {
    val map12 = scala.collection.mutable.Map[Variable, Variable]()
    val map21 = scala.collection.mutable.Map[Variable, Variable]()
    def eq1(t1: Expression, t2: Expression): Boolean = (t1, t2) match {
      case (v1: Variable, v2: Variable) => (map12.get(v1), map21.get(v2)) match {
        case (Some(v3), Some(v4)) => v2 == v3 && v1 == v4
        case (None, None) => map12(v1) = v2; map21(v2) = v1; true
        case _ => false
      }
      case (Constructor(name1, args1), Constructor(name2, args2)) =>
        name1 == name2 && List.forall2(args1, args2)(eq1)
      case (Application(h1, a1), Application(h2, a2)) => eq1(h1, h2) && eq1(a1, a2)
      case (LambdaAbstraction(b1, v1), LambdaAbstraction(b2, v2)) => eq1(b1, b2) && eq1(v1, v2)
      case (CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) => {
        val bs1s = bs1 sort compareB1
        val bs2s = bs2 sort compareB1
        bs1s(0).pattern.name == bs2s(0).pattern.name && 
          eq1(sel1, sel2) &&
            List.forall2(bs1s, bs2s){
              (b1, b2) => List.forall2(b1.pattern.args, b2.pattern.args)(eq1) && eq1(b1.term, b2.term)
            }
      }
      case (LetRecExpression((f1, a1), e1), LetRecExpression((f2, a2), e2)) => {
        map12(f1) = f2; map21(f2) = f1;
        eq1(a1, a2) && eq1(e1, e2)
      }
      case _ => false
    }    
    eq1(term1, term2)
  }
  def compareB1(b1: Branch, b2: Branch) = b1.pattern.name.compareTo(b2.pattern.name) < 0
}
