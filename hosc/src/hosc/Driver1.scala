package hosc;

import HLanguage1._
import TermAlgebra1._

object Driver1 {
  def unfoldLetrec(letrec: LetRecExpression1) = 
    inline(letrec.expr / Map(letrec.binding), letrec)
    
  private def inline(term: Term1, letrec: LetRecExpression1): Term1 = {
    val (oldF, body) = letrec.binding
    val letrecApp = letrec.expr
    def traverse(t: Term1): Term1 = t match {
      case v : Variable1 => v
      case Constructor1(name, args) => Constructor1(name, args map traverse)
      case CaseExpression1(sel, bs) => CaseExpression1(traverse(sel), bs map {b => Branch1(b.pattern, traverse(b.term)) })
      case LetRecExpression1((f, fdef), expr) => LetRecExpression1((f, traverse(fdef)), traverse(expr))
      case LambdaAbstraction1(v, expr) => LambdaAbstraction1(v, traverse(expr))
      case app @ Application1(head, arg) => {        
        val coreF = getCoreAppVar(app)
        if (coreF == oldF) {
          val newF = newVar1()
          newF.call = true
          val fargs = extractAppArgs(app)
          LetRecExpression1((newF, freshAllBinders(body)/Map(oldF -> newF)), 
              freshAllBinders(constructApplication1(newF, fargs))) 
        } else {
          Application1(traverse(head), traverse(arg))
        }
      }
    }    
    traverse(term)
  }
  
}
