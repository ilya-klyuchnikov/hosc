package hosc;

import HLanguage._
import HLanguage1._

object HLUtils {
  // converts hlanguage to hlanguage1
  def hlToHl1(term: Term): Term1 = term match {
    case Variable(n) => Variable1(n)
    case Constructor(n, args) => Constructor1(n, args map hlToHl1)
    case LambdaAbstraction(v, e) => LambdaAbstraction1(Variable1(v.name), hlToHl1(e))
    case Application(h, a) => Application1(hlToHl1(h), hlToHl1(a))
    case CaseExpression(sel, bs) => 
      CaseExpression1(hlToHl1(sel), bs map {b => Branch1(hlToHl1(b.pattern), hlToHl1(b.term))})
  }
  
  def hlToHl1(p: Pattern): Pattern1 = {
    Pattern1(p.name, p.args map {v => Variable1(v.name)})
  }
}
