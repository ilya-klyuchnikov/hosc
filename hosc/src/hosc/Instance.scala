package hosc

import HLanguage._

object Instance {
  // we assume for now that all binders are different!
  def findSubst(expr1: Expression, expr2: Expression): Map[Variable, Expression] = {
    val map = scala.collection.mutable.Map[Variable, Expression]()
    def walk(e1: Expression, e2: Expression, binders: List[(Variable, Variable)]): Boolean = (e1, e2) match {
      case (v1: Variable, _) => { 
        // 1. e1 is bound => e1 == e2
        val bs = binders find {case (b1, b2) => b1 == v1}
        bs match {
          case None =>
          case Some((b1, b2)) => return b2 == e2
        }
        // 2. e2 has bound variable => false
        val fv2 = Set(TermAlgebra.getFreeVars(e2):_*)
        binders forall {case (_, v2) => !(fv2(v2))} match {
          case false => return false
          case true =>
        }
        if (v1.global && v1 != e2) {
          false
        } else if (map.getOrElse(v1, e2) == e2) {
          map += (v1 -> e2)
          true
        } else {
          false
        }
      }
      case (Constructor(n1, args1), Constructor(n2, args2)) if n1 == n2 =>
        List.forall2(args1, args2)(walk(_, _,binders))
      case (Application(h1, a1), Application(h2, a2)) =>
        walk(h1, h2, binders) && walk(a1, a2, binders)
      case (LambdaAbstraction(v1, b1), LambdaAbstraction(v2, b2)) =>
        walk(b1, b2, (v1, v2) :: binders)
      case (CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) if bs1.size == bs2.size =>
        val bs1s = bs1 sort TermAlgebra.compareB
        val bs2s = bs2 sort TermAlgebra.compareB
        walk(sel1, sel2, binders) &&
          List.forall2(bs1s, bs2s){(b1, b2) =>
            b1.pattern.name == b2.pattern.name && walk(b1.term, b2.term, (b1.pattern.args zip b2.pattern.args) ::: binders)
          }
      case (LetRecExpression((f, e11), e21), LetRecExpression((g, e12), e22)) =>
        walk(e11, e12, (f, g) :: binders) && walk(e21, e22, (f, g) :: binders)
      case _ => false
    }
    if (walk(expr1, expr2, Nil)) {
      Map(map.toList:_*).filter{case (k, v) => k != v} 
    } else {
      null
    }
  }
  
  def instanceOf(e1: Expression, e2: Expression) = findSubst(e1, e2) != null
}
