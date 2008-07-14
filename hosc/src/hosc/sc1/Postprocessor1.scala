package hosc.sc1

import HLanguage1._

// update letrec flag for variables
object Postprocessor1 {
  
  def postprocess(program: Program1) = {
    process(program.expr, Set[Variable1]())
  }
  
  def process(t: Term1, letrecs: Set[Variable1]): Unit = t match {
    case v: Variable1 => v.call = (letrecs contains v)
    case Constructor1(_, args) => for (a <- args) process(a, letrecs)
    case LambdaAbstraction1(v, t) => process(t, letrecs)
    case Application1(h, a) => process(h, letrecs); process(a, letrecs)
    case CaseExpression1(s, bs) => process(s, letrecs); for (b <- bs) process(b.term, letrecs)
    case LetRecExpression1((v, t1), t2) => {
      v.call = true
      val letrecs1 = letrecs + v
      process(t1, letrecs1)
      process(t2, letrecs1)
    }
  }
  
}
