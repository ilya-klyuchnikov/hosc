package hosc;

import HLanguage1._

object TermAlgebra1 {
  var i = 0
  def newVar1() = {
    i += 1
    Variable1("$" + i) 
  }
  type Substitution = Tuple2[Variable1, Term1]
  type DoubleSubstitution = Tuple3[Variable1, Term1, Term1]
  case class Generalization(term: Term1, sub1: List[Substitution], sub2: List[Substitution])
  case class Generalization2(term: Term1, dSub: List[DoubleSubstitution])

  sealed abstract class TermDecomposition1
  
  sealed abstract class Observable1(val term: Term1) extends TermDecomposition1
  case class ObservableVar(v: Variable1) extends Observable1(v)
  case class ObservableVarApp(v: Variable1, app: Application1) extends Observable1(app)
  case class ObservableCon1(c: Constructor1) extends Observable1(c)
  case class ObservableLam1(l: LambdaAbstraction1) extends Observable1(l)
  
  sealed abstract class Redex1(val term : Term1)
  case class RedexLetRec1(letrec: LetRecExpression1) extends Redex1(letrec)
  case class RedexLamApp1(lam: LambdaAbstraction1, app: Application1) extends Redex1(app)
  case class RedexCaseVarApp1(a: Application1, ce: CaseExpression1) extends Redex1(ce)
  case class RedexCaseVar1(v: Variable1, ce: CaseExpression1) extends Redex1(ce)  
  case class RedexCaseCon1(c: Constructor1, ce: CaseExpression1) extends Redex1(ce)
  
  sealed abstract class Context1(val redex: Redex1) extends TermDecomposition1 {
    def replaceHole(t: Term1): Term1
  }  
  case class ContextHole1(override val redex: Redex1) extends Context1(redex) {
    def replaceHole(t: Term1) = t
  }
  // app = con e 
  case class ContextApp1(head: Context1, app: Application1) extends Context1(head.redex) {
    def replaceHole(t: Term1) = Application1(head.replaceHole(t), app.arg)
  }
  // ce = case selector of ....
  case class ContextCase1(selector: Context1, ce: CaseExpression1) extends Context1(selector.redex) {
    def replaceHole(t: Term1) = CaseExpression1(selector.replaceHole(t), ce.branches)
  } 
  
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
