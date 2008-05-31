package hosc;

import HLanguage1._

object TermAlgebra1 {
  
  def applySubstitution1(term: Term1, s: Map[Variable1, Term1]): Term1 = term match {
    case v: Variable1 => s.get(v) match {case Some(t) => t; case None => v}
    case Constructor1(n, args) => Constructor1(n, args map {applySubstitution1(_, s)})
    case LambdaAbstraction1(v, t) => {
      val v_ = applySubstitution1(v, s)
      if (!v_.isInstanceOf[Variable1]) {
        println(v)
      }
      LambdaAbstraction1(v_.asInstanceOf[Variable1], applySubstitution1(t, s))    
    }      
    case Application1(h, a) => Application1(applySubstitution1(h, s), applySubstitution1(a, s))
    case CaseExpression1(sel, bs) => 
      CaseExpression1(applySubstitution1(sel, s), 
          bs map {b => Branch1(Pattern1(b.pattern.name, 
              b.pattern.args map {applySubstitution1(_, s).asInstanceOf[Variable1]}), 
              applySubstitution1(b.term, s))});
    case LetRecExpression1(b, e) => 
      LetRecExpression1((b._1, applySubstitution1(b._2, s)), applySubstitution1(e, s));
  }
  
  def getAllVars1(expr: Term1): Set[Variable1] = expr match {
    case v: Variable1 => Set(v)
    case Constructor1(_, args) => (Set[Variable1]() /: args) {(vs, term) => vs ++ getAllVars1(term)}
    case LambdaAbstraction1(x, term) => getAllVars1(term) + x
    case Application1(head, arg) => getAllVars1(head) ++ getAllVars1(arg)
    case CaseExpression1(sel, bs) => 
      getAllVars1(sel) ++ (Set[Variable1]() /: bs) {(vs, b) => vs ++ getAllVars1(b.term) ++ b.pattern.args}
    case LetRecExpression1(b, expr) =>
      getAllVars1(expr) ++ getAllVars1(b._2) + b._1
  }
  
  def constructLambda1(vs: List[Variable1], e: Term1): Term1 = {
    def constructLambda_(vs_ : List[Variable1]) : Term1 = vs_ match {
      case Nil => e;
      case v :: vv => LambdaAbstraction1(v, constructLambda_(vv))
    }
    constructLambda_(vs)
  }
  
  def constructApplication1(head: Term1, args: List[Term1]): Term1 = {
    var res = head
    var list = args
    while (!list.isEmpty) {
      res = Application1(res, list.head)
      list = list.tail
    }
    res
  }

}
