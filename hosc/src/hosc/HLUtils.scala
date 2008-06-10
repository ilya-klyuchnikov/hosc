package hosc;

import HLanguage._
import HLanguage1._
import scala.collection.mutable.ListBuffer

object HLUtils {
  // converts hlanguage to hlanguage1
  def hlToHl1(term: Term): Term1 = term match {
    case v @ Variable(n) => val v1 = Variable1(n); v1.call = v.global; v1
    case Constructor(n, args) => Constructor1(n, args map hlToHl1)
    case LambdaAbstraction(v, e) => LambdaAbstraction1(Variable1(v.name), hlToHl1(e))
    case Application(h, a) => Application1(hlToHl1(h), hlToHl1(a))
    case CaseExpression(sel, bs) => 
      CaseExpression1(hlToHl1(sel), 
          bs map {b => Branch1(Pattern1(b.pattern.name, b.pattern.args map {v => Variable1(v.name)}), hlToHl1(b.term))})
  }  
  
  def hlToHl1(p: Pattern): Pattern1 = {
    Pattern1(p.name, p.args map {v => Variable1(v.name)})
  }
  
  def hl1ToHl(e1: Term1, p: ListBuffer[Function]): Term = e1 match {
    case Variable1(n) => Variable(n)
    case Constructor1(n, args) => Constructor(n, args map {e => hl1ToHl(e, p)})
    case LambdaAbstraction1(v, e) => LambdaAbstraction(Variable(v.name), hl1ToHl(e, p))
    case Application1(h, a) => Application(hl1ToHl(h, p), hl1ToHl(a, p))
    case CaseExpression1(sel, bs) => 
      CaseExpression(hl1ToHl(sel, p), 
          bs map {b => Branch(Pattern(b.pattern.name, b.pattern.args map {v => Variable(v.name)}), hl1ToHl(b.term, p))})
    case lr: LetRecExpression1 => letrecToHl(lr, p)
  }
  
  def letrecToHl(letrec: LetRecExpression1, p: ListBuffer[Function]): Term = { 
    Function(letrec.binding._1.name, hl1ToHl(letrec.binding._2, p).asInstanceOf[LambdaAbstraction]) +: p
    hl1ToHl(letrec.expr, p)
  }
  
  def hl1ToHl(p1: Program1): Program = {
    val functions = new ListBuffer[Function]()
    val goal = hl1ToHl(p1.expr, functions)
    Program(p1.ts, goal, functions.toList)
  }
}
