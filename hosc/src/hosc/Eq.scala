package hosc

import HLanguage._

object Eq {
  // tests term equivalence up to alpha renaming
  def equivalent(term1: Expression, term2: Expression): Boolean = {
    val map12 = scala.collection.mutable.Map[Variable, Variable]()
    val map21 = scala.collection.mutable.Map[Variable, Variable]()
    def eq1(t1: Expression, t2: Expression, bv1: Set[Variable], bv2: Set[Variable]): Boolean = (t1, t2) match {
      case (v1: Variable, v2: Variable) if bv1(v1) == false && bv2(v2) == false =>
        v1 == v2
      case (v1: Variable, v2: Variable) if bv1(v1) == true && bv2(v2) == true => 
        (map12.get(v1), map21.get(v2)) match {
          case (Some(v3), Some(v4)) => v2 == v3 && v1 == v4
          case (None, None) => map12(v1) = v2; map21(v2) = v1; true
          case _ => false
        }
      case (Constructor(name1, args1), Constructor(name2, args2)) =>
        name1 == name2 && List.forall2(args1, args2)(eq1(_, _, bv1, bv2))
      case (Application(h1, a1), Application(h2, a2)) => 
        eq1(h1, h2, bv1, bv2) && eq1(a1, a2, bv1, bv2)
      case (LambdaAbstraction(v1, b1), LambdaAbstraction(v2, b2)) => 
        eq1(v1, v2, bv1 + v1, bv2 + v2) && eq1(b1, b2, bv1 + v1, bv2 + v2)
      case (CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) => {
        val bs1s = bs1 sort compareB1
        val bs2s = bs2 sort compareB1
        bs1s(0).pattern.name == bs2s(0).pattern.name && 
          eq1(sel1, sel2, bv1, bv2) &&
            List.forall2(bs1s, bs2s){
              (b1, b2) => List.forall2(b1.pattern.args, b2.pattern.args)((x, y) => eq1(x, y, bv1 + x, bv2 + y)) && 
                eq1(b1.term, b2.term, bv1 ++ b1.pattern.args, bv2 ++ b2.pattern.args)
            }
      }
      case (LetRecExpression((f1, a1), e1), LetRecExpression((f2, a2), e2)) => {
        map12(f1) = f2; map21(f2) = f1;
        eq1(a1, a2, bv1 + f1, bv2 + f2) && eq1(e1, e2, bv1 + f1, bv2 + f2)
      }
      case _ => false
    }    
    eq1(term1, term2, Set(), Set())
  }
  def compareB1(b1: Branch, b2: Branch) = b1.pattern.name.compareTo(b2.pattern.name) < 0
}
