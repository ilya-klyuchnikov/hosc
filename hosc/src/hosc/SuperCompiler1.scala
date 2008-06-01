package hosc;

//import HLanguage._
import HLanguage1._
import TermAlgebra1._
import ProcessTree1._

abstract class SuperCompiler1(val program: Program1) {
  val emptyMap = Map[Variable1, Term1]()
  
  def driveExp(expr: Expression1): List[Pair[Term1, Map[Variable1, Term1]]] = expr match {
    case LetExpression1(bs, t) => (t, Map[Variable1, Term1]()) :: (bs map {b => (b._2, emptyMap)}) 
    case t: Term1 => decompose1(t) match {
      case ObservableVar1(_) => Nil
      case ObservableCon1(c) => c.args map {a => (a, emptyMap)}
      case ObservableVarApp1(_, app) => extractAppArgs1(app) map {a => (a, emptyMap)}
      case ObservableLam1(l) => (l.t, emptyMap) :: Nil
      case context: Context1 => context.redex match {
        case RedexLamApp1(lam, app) => 
          (context.replaceHole(applySubstitution1(lam.t, Map(lam.v -> app.arg))), emptyMap) :: Nil
        case RedexCaseCon1(c, ce) => {
          val b = ce.branches.find(_.pattern.name == c.name).get
          val sub = Map[Variable1, Term1]() ++ (b.pattern.args zip c.args)
          (context.replaceHole(applySubstitution1(b.term, sub)), emptyMap) :: Nil          
        }
        case RedexCaseVar1(v, CaseExpression1(sel, bs)) =>
          (sel, emptyMap) :: (bs map 
            {b => (replaceTerm1(context.replaceHole(b.term), v, Constructor1(b.pattern.name, b.pattern.args)), emptyMap)})  
        case RedexCaseVarApp1(a, CaseExpression1(sel, bs)) =>
          (sel, emptyMap) :: (bs map 
            {b => (replaceTerm1(context.replaceHole(b.term), a, Constructor1(b.pattern.name, b.pattern.args)), emptyMap)})
        case RedexLetRec1(letrec) => (context.replaceHole(unfold(letrec)), emptyMap) :: Nil
      }
    }
  }
  
  def unfold(letrec: LetRecExpression1): Term1
}
