package hosc;

import HLanguage._
import TermAlgebra._
import ProcessTree._

class SuperCompiler(program: Program){
  val emptyMap = Map[Variable, Term]()
  
  def driveExp(expr: BaseExpression): List[Pair[Term, Map[Variable, Term]]] = expr match {
    case LetExpression(bs, t) => {
      (t.asInstanceOf[Term], Map[Variable, Term]()) :: 
        (bs map {b => (b._2.asInstanceOf[Term], emptyMap)}) 
    }
    case t: Term => decompose(t) match {
      case ObservableVar(_) => Nil
      case ObservableCon(c) => c.args map {a => (a, emptyMap)}
      case ObservableVarApp(_, app) => extractAppArgs(app) map {a => (a, emptyMap)}
      case ObservableLam(l) => (l.t, emptyMap) :: Nil
      case context: Context => context.redex match {
        case RedexCall(v) => {
          val lam = program.getFunction(v.name).get.lam
          (context.replaceHole(freshBinders(lam)), emptyMap) :: Nil 
        }
        case RedexLamApp(lam, app) => 
          (context.replaceHole(applySubstitution(lam.t, Map(lam.v -> app.arg))), emptyMap) :: Nil
        case RedexCaseCon(c, ce) => {
          val b = ce.branches.find(_.pattern.name == c.name).get
          val sub = Map[Variable, Term]() ++ (b.pattern.args zip c.args)
          (context.replaceHole(applySubstitution(b.term, sub)), emptyMap) :: Nil          
        }
        case RedexCaseVar(v, CaseExpression(sel, bs)) =>
          (sel, emptyMap) :: (bs map 
            {b => (context.replaceHole(replaceTerm(b.term, v, Constructor(b.pattern.name, b.pattern.args))), emptyMap)})
        case RedexCaseVarApp(a, CaseExpression(sel, bs)) =>
          (sel, emptyMap) :: (bs map 
            {b => (context.replaceHole(replaceTerm(b.term, a, Constructor(b.pattern.name, b.pattern.args))), emptyMap)})
      }
    }
  }  
  
  def buildProcessTree(e: BaseExpression): ProcessTree = {
    val p = ProcessTree(e)
    while (!p.isClosed) {
      val beta = p.leafs.find(!_.isProcessed).get
      val bExpr = beta.expr
      beta.expr match {
        case bTerm: Term => 
          if (!isConF(bTerm)) {
            drive(p, beta) 
          } else beta.ancestors.find {n1: Node => n1.expr match {case c: Term => he(c, bTerm); case _ => false}} match {
            case None => drive(p, beta)
            case Some(alpha) => {
              val aTerm = alpha.expr.asInstanceOf[Term]
              val msg_ = msg(aTerm, bTerm)
              if (isConV(msg_.term)) 
                makeAbstraction(p, beta, alpha)
              else 
                makeAbstraction(p, alpha, beta)
            }
          }        
        case _ => drive(p, beta)
      }
      
    }    
    p
  }  
  
  def drive(t: ProcessTree, n: Node): Unit = {
    t.addChildren(n, driveExp(n.expr))
  }
  
  def makeAbstraction(t: ProcessTree, alpha: Node, beta: Node): Unit = {
    val g = msg(alpha.expr.asInstanceOf[Term], beta.expr.asInstanceOf[Term])
    if (g.sub1.isEmpty){
      t.replace(alpha, g.term)
    } else {
      t.replace(alpha, LetExpression(g.sub1, g.term))
    }    
  }
  
}