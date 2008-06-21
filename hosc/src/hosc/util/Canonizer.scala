package hosc.util;

import hosc.TermAlgebra1.compareB1
import hosc.HLanguage1._
import hosc.HLanguage._

object Canonizer {
  def canonize1(tt: Term1):Term1 = tt match {
    case v: Variable1 => v
    case c@Constructor1(name, args) => {
      val c1 = Constructor1(name, args map canonize1)
      c1.label = c.label
      c1
    }
    case la@LambdaAbstraction1(x, term) => {
      val la1 = LambdaAbstraction1(x, canonize1(term))
      la1.label = la.label
      la1
    }
    case a@Application1(head, arg) => {
      val a1 = Application1(canonize1(head), canonize1(arg))
      a1.label = a.label
      a1
    }
    case ce@CaseExpression1(sel, bs) => {
      val sortedBranches = bs sort compareB1
      val canonizedBranches = sortedBranches map {b => Branch1(b.pattern, canonize1(b.term))}
      val canonizedSelector = canonize1(sel)
      val ce1 = CaseExpression1(canonizedSelector, canonizedBranches)
      ce1.label = ce.label
      ce1
    }    
    case lr@LetRecExpression1(b, expr) => {
      val lr1 = LetRecExpression1(b, canonize1(expr))
      lr1.label = lr.label
      lr1
    }
  }
}
