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

}
