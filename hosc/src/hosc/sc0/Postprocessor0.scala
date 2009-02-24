package hosc.sc0

import HLanguage._

// qualifies global variables
object Postprocessor0 {
  
  def postprocess(program: Program) = {
    val globals = Set[Variable]() ++ (program.fs map (f => Variable(f.name)))
    for (f <- program.fs) process(f.lam,  globals)
    process(program.goal, globals)
  }
  
  def process(t: Expression, globals: Set[Variable]): Unit = t match {
    case v: Variable => v.global = (globals contains v)
    case Constructor(_, args) => for (a <- args) process(a, globals)
    case LambdaAbstraction(v, t) => process(t, globals)
    case Application(h, a) => process(h, globals); process(a, globals)
    case CaseExpression(s, bs) => process(s, globals); for (b <- bs) process(b.term, globals)
    case LetRecExpression((v, e), e0) => {process(e, globals); process(e0, globals)}                                                   
  }
  
}
