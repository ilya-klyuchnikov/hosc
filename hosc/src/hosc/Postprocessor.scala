package hosc;

import HLanguage._

// qualifies global variables
object Postprocessor {
  
  def postprocess(program: Program) = {
    val globals = Set[Variable]() ++ (program.fs map (f => Variable(f.name)))
    for (f <- program.fs) process(f.lam,  globals)
  }
  
  def process(t: Term, globals: Set[Variable]): Unit = t match {
    case v: Variable => v.global = (globals contains v)
    case Constructor(_, args) => for (a <- args) process(a, globals)
    case LambdaAbstraction(v, t) => process(t, globals)
    case Application(h, a) => process(h, globals); process(a, globals)
    case CaseExpression(s, bs) => process(s, globals); for (b <- bs) process(b.term, globals)
  }
  
}
