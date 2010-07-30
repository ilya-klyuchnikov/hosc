package hosc

import HLanguage._
import HE._
import MSG._
import TermAlgebra._
import ProcessTree._
import LangUtils._

// base code of supercompiler that doesn't depend on
// WHISTLE and GENERALIZATION algorithms
trait ASupercompiler {
  def program: Program
  
  // None means dead branch (missing pattern)
  def driveExp(expr: Expression): Option[List[Expression]] = expr match {
    case LetExpression(bs, t) => Some(t :: (bs map {_._2}))
    case t => decompose(t) match {
      case ObservableVar(_) => Some(Nil)
      case ObservableCon(c) => Some(c.args)
      case ObservableVarApp(_, app) => Some(extractAppArgs(app))
      case ObservableLam(l) => Some(l.t :: Nil)
      case context@Context(redex) => redex match {
        case RedexCall(v) => {
          val lam = program.getFunction(v.name).get.body
          //Some(betaReduce(context.replaceHole(freshBinders(lam))) :: Nil)
          Some(context.replaceHole(freshBinders(lam)) :: Nil)
        }
        case RedexLamApp(lam, app) => 
          Some(betaReduce(expr) :: Nil)
        case RedexCaseCon(c, ce) => {
          ce.branches.find(_.pattern.name == c.name) match {
            case Some(b) => {
              val sub = Map(b.pattern.args zip c.args:_*)
              Some(freshBinders(context.replaceHole(applySubstitution(b.term, sub))) :: Nil)
            }
            case None => None
          }
        }
        case RedexCaseVar(_, CaseExpression(sel, bs)) =>
          Some(freshBinders(sel) :: 
            (bs map {b => freshBinders(replaceTerm(context.replaceHole(b.term), sel, Constructor(b.pattern.name, b.pattern.args)))}))
      }
    }
  }
  
  def betaReduce(expr: Expression): Expression = decompose (expr) match {
    case context@Context(RedexLamApp(lam, app)) =>
     //betaReduce(context.replaceHole(applySubstitution(lam.t, Map(lam.v -> app.arg))))
     freshBinders(context.replaceHole(applySubstitution(lam.t, Map(lam.v -> app.arg))))
    case _ => freshBinders(expr)
  }
}
