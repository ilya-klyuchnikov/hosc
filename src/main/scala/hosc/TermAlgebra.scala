package hosc

import HLanguage._

object TermAlgebra {
  var i = 0
  def newVar(): Variable = {
    i += 1
    Variable("$" + i)
  }

  def applySubstitution(term: Expression, s: Map[Variable, Expression]): Expression = term match {
    case v: Variable => s.get(v) match {case Some(t) => t; case None => v}
    case Constructor(n, args) => Constructor(n, args map {applySubstitution(_, s)})
    case LambdaAbstraction(v, t) =>
      LambdaAbstraction(v, applySubstitution(t, s - v))
    case Application(h, a) => Application(applySubstitution(h, s), applySubstitution(a, s))
    case CaseExpression(sel, bs) =>
      CaseExpression(applySubstitution(sel, s),
          bs map {b => Branch(b.pattern, applySubstitution(b.term, s -- b.pattern.args))})
    case let: LetExpression => throw new IllegalArgumentException("unexpected expr: " + let)
    case letrec: LetRecExpression => throw new IllegalArgumentException("unexpected expr: " + letrec)
  }

  def getFreeVars(t: Expression): List[Variable] = t match {
    case v: Variable => if (v.global) List() else List(v)
    case Constructor(_, args) => (List[Variable]() /: args) {(vs, exp) =>  vs ++ (getFreeVars(exp).filterNot(vs.contains))}
    case LambdaAbstraction(x, term) => getFreeVars(term).filterNot(_ == x)
    case Application(head, arg) => {
      val headVars = getFreeVars(head)
      headVars ++ (getFreeVars(arg).filterNot(headVars.contains))
    }
    case CaseExpression(sel, bs) =>
       (getFreeVars(sel) /: bs) {(vs, b) => vs ++ (getFreeVars(b.term).filterNot(b.pattern.args.contains).filterNot(vs.contains))}
    case LetRecExpression((f, e), e0) => {
      val eVars = getFreeVars(e)
      (eVars ++ (getFreeVars(e0).filterNot(eVars.contains))).filterNot(_ == f)
    }
    case let: LetExpression => throw new IllegalArgumentException("unexpected expr: " + let)
  }

  def compareB(b1: Branch, b2: Branch) = b1.pattern.name.compareTo(b2.pattern.name) < 0

  def getCoreLocalHead(app: Application): Expression = app.head match {
    case a: Application => getCoreLocalHead(a)
    case h => h
  }

  def constructApplication(head: Expression, args: List[Expression]): Expression = {
    var res = head
    var list = args
    while (!list.isEmpty) {
      res = Application(res, list.head)
      list = list.tail
    }
    res
  }

  def constructLambda(vs: List[Variable], e: Expression): Expression = {
    def constructLambda_(vs_ : List[Variable]) : Expression = vs_ match {
      case Nil => e;
      case v :: vv => LambdaAbstraction(v, constructLambda_(vv))
    }
    constructLambda_(vs)
  }

  def freshBinders(term: Expression): Expression = term match {
    case v: Variable => v
    case LetExpression(bs, expr) => LetExpression(bs map {case (k, v) => (k, freshBinders(v))}, freshBinders(expr))
    case Constructor(name, args) => Constructor(name, args map (freshBinders(_)))
    case Application(e1, e2) => Application(freshBinders(e1), freshBinders(e2))
    case LambdaAbstraction(v, t) => {
      val freshV = newVar()
      LambdaAbstraction(freshV, applySubstitution(freshBinders(t), Map(v -> freshV)))
    }
    case CaseExpression(sel, bs) => CaseExpression(freshBinders(sel), bs map {freshBinders(_)})
    case letrec: LetRecExpression => throw new IllegalArgumentException("unexpected expr: " + letrec)
  }

  def freshBinders(b: Branch): Branch = {
    val args = b.pattern.args
    val newVars = args map {x => newVar()}
    Branch(Pattern(b.pattern.name, newVars),
        applySubstitution(freshBinders(b.term), Map[Variable, Expression]() ++ (args zip newVars)))
  }

  def extractAppArgs(term: Expression): List[Expression] = term match {
    case Application(h, a) => extractAppArgs(h) ::: List(a)
    case _ => Nil
  }

  def lineApp(term: Expression): List[Expression] = term match {
    case Application(h, a) => lineApp(h) ::: (a:: Nil)
    case t => t :: Nil
  }

  def getAllVars(expr: Expression): Set[Variable] = expr match {
    case v: Variable => Set(v)
    case Constructor(_, args) => (Set[Variable]() /: args) {_ ++ getAllVars(_)}
    case LambdaAbstraction(x, term) => getAllVars(term) + x
    case Application(head, arg) => getAllVars(head) ++ getAllVars(arg)
    case CaseExpression(sel, bs) =>
       (getAllVars(sel) /: bs) {(vs, b) => vs ++ getAllVars(b.term) ++ b.pattern.args}
    case LetExpression(bs, expr) =>
       (getAllVars(expr) /: bs) {(vs, b) => vs ++ getAllVars(b._2) + b._1}
    case LetRecExpression(bs, expr) =>
       (getAllVars(expr) /: (bs :: Nil)) {(vs, b) => vs ++ getAllVars(b._2) + b._1}
  }

}
