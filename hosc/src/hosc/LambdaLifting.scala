package hosc
import HLanguage._
import sc0.TermAlgebra0

object LambdaLifting {
  val none: Option[LetRecExpression] = None
  // eliminates free variables from letrecs
  def lift (e: Expression) : Expression = {
    e
  }
  
  def findLetRec(e: Expression) : Option[LetRecExpression] = e match {
    case Variable(_) => None
    case Constructor(_, args) => args.foldLeft(none){(r, expr) => r.orElse(findLetRec(expr))}
    case LambdaAbstraction(_, expr) => findLetRec(expr)
    case Application(head, arg) => findLetRec(head).orElse(findLetRec(arg))
    case CaseExpression(sel, branches) => branches.foldLeft(findLetRec(sel)){(r, branch) => r.orElse(findLetRec(branch.term))}
    case letrec@LetRecExpression((f, e1), e2) => if (TermAlgebra0.getFreeVars(e1).isEmpty) findLetRec(e1).orElse(findLetRec(e2)) else Some(letrec)
  }
}