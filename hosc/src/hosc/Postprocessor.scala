package hosc;

import HLanguage._

// qualifies global variables
object Postprocessor {
  
  def postprocess(program: Program) = {
    for (f <- program.fs) process(f.lam, Set.empty[Variable])
  }
  
  def process(t: Term, locals: Set[Variable]): Unit = t match {
    case v: Variable => v.global = !(locals contains v)
    case Constructor(_, args) => for (a <- args) process(a, locals)
    case LambdaAbstraction(v, t) => process(t, locals + v)
    case Application(h, a) => process(h, locals); process(a, locals)
    case CaseExpression(s, bs) => process(s, locals); for (b <- bs) process(b.term, locals ++ b.pattern.args)
  }
  
}
