package hosc;

import HLanguage1._
import sc1.TermAlgebra1._

class Transformer1(val program: Program1) {
  val emptyMap = Map[Variable1, Term1]()
  
  def driveExp(expr: Expression1): List[Pair[Term1, Map[Variable1, Term1]]] = expr match {
    case LetExpression1(bs, t) => (t, Map[Variable1, Term1]()) :: Nil 
    case t: Term1 => decompose1(t) match {
      case ObservableVar1(_) => Nil
      case ObservableCon1(c) => c.args map {a => (a, emptyMap)}
      case ObservableVarApp1(_, app) => extractAppArgs1(app) map {a => (a, emptyMap)}
      case ObservableLam1(l) => (l.t, emptyMap) :: Nil
      case context: Context1 => context.redex match {
        case RedexLamApp1(lam, app) => 
          (context.replaceHole(lam.t/Map(lam.v -> app.arg)), emptyMap) :: Nil
        case RedexCaseCon1(c, ce) => {
          val b = ce.branches.find(_.pattern.name == c.name).get
          val sub = Map((b.pattern.args zip c.args):_*)
          (context.replaceHole(b.term/sub), emptyMap) :: Nil          
        }
        case RedexCaseVar1(v, CaseExpression1(sel, bs)) =>
          (sel, emptyMap) :: (bs map 
            {b => (freshBinders(replaceTerm1(context.replaceHole(b.term), v, Constructor1(b.pattern.name, b.pattern.args))), Map(v-> Constructor1(b.pattern.name, b.pattern.args)))})
        case RedexCaseVarApp1(a, CaseExpression1(sel, bs)) =>
          (sel, emptyMap) :: (bs map 
            {b => (freshBinders(replaceTerm1(context.replaceHole(b.term), a, Constructor1(b.pattern.name, b.pattern.args))), emptyMap)})
        //should not happen cause we always preserve letrecs during unfolding
        case RedexCall1(f) =>
            throw new IllegalArgumentException(t.toString())
        case RedexLetRec1(letrec) => {
          (context.replaceHole(letrec.expr), emptyMap) :: Nil}
      }
    }    
  }
  def unfoldLetrec() = null
}
