package hosc

object TermAlgebra {
  import HLanguage._

  def getFreeVars(t: Expression): List[Variable] = t match {
    case v: Variable =>
      if (v.global) List() else List(v)
    case Constructor(_, args) =>
      args.foldLeft(List[Variable]())((vs, exp) =>  vs ++ getFreeVars(exp).filterNot(vs.contains))
    case LambdaAbstraction(x, term) =>
      getFreeVars(term).filterNot(_ == x)
    case Application(head, arg) =>
      val headVars = getFreeVars(head)
      headVars ++ getFreeVars(arg).filterNot(headVars.contains)
    case CaseExpression(sel, bs) =>
      bs.foldLeft(getFreeVars(sel))((vs, b) => vs ++ getFreeVars(b.term).filterNot(b.pattern.args.contains).filterNot(vs.contains))
    case LetRecExpression((f, e), e0) =>
      val eVars = getFreeVars(e)
      (eVars ++ getFreeVars(e0).filterNot(eVars.contains)).filterNot(_ == f)
    case let: LetExpression =>
      throw new IllegalArgumentException("unexpected expr: " + let)
  }
}
