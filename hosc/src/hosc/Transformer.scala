package hosc;

import HLanguage._
import HLanguage1._
import TermAlgebra1._
import HLUtils._
import ProcessTree1._
import MSG1._
import HE1._

class Transformer(val tree: ProcessTree1, val program: Program) {
  val emptyMap = Map[Variable1, Term1]()
  var defs = Map((program.fs map {f => val v = Variable1(f.name); v.call = true; (v, hlToHl1(f.lam))}):_*)
  
  def driveExp(expr: Expression1): List[Pair[Term1, Map[Variable1, Term1]]] = expr match {
    case LetExpression1(bs, t) => (t, Map[Variable1, Term1]()) :: Nil 
    case t: Term1 => t.label match { 
      case null => decompose1(t) match {
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
          case RedexCall1(f) =>
            val lam = defs(f)
            val lam1 = freshBinders(lam)
            (context.replaceHole(lam1), emptyMap) :: Nil
          case RedexLetRec1(letrec) => {
            defs += letrec.binding
            (context.replaceHole(letrec.expr), emptyMap) :: Nil}
        }
      }
      case _ => {
        val t1 = freshBinders(t); t1.label = null  
        (t1, emptyMap) :: Nil    
      }
    }    
  }
  
  
  def transform(): Boolean = {
    var transformed = false
    while (!tree.isClosed) {
      val beta = tree.leafs.find(!_.isProcessed).get
      val bExpr = beta.expr
      beta.expr match {
        case bTerm: Term1 if canFoldOrGenarilize(bTerm) => 
          beta.ancestors.find {n1: Node1 => n1.expr match {case a: Term1 => instanceOf(a, bTerm); case _ => false}} match {
            case Some(alpha) => {beta.repeatedOf = alpha;transformed = true;}
            case None => {
              beta.ancestors.find 
                {n1: Node1 => n1.expr match {case a: Term1 => heByCoupling(a, bTerm); case _ => false}} 
                  match {
                    case Some(alpha) => {
                      val aTerm = alpha.expr.asInstanceOf[Term1] 
                      msg(aTerm, bTerm).term match {
                        case Variable1(_) => {drive(beta); transformed = true}
                        case _ => {makeAbstraction(alpha, beta); transformed = true}
                      }
                    }
                    case None => {drive(beta); transformed = true}
                  }
            }
          }
        case _ => {drive(beta); transformed = true}
      }
      
    }
    transformed
  }
  
  def drive(n: Node1): Unit = {
    tree.addChildren(n, driveExp(n.expr))
  }
  
  def makeAbstraction(alpha: Node1, beta: Node1): Unit = {
    val g = msg(alpha.expr.asInstanceOf[Term1], beta.expr.asInstanceOf[Term1])
    var t = g.term
    var subs = g.sub1
    var resSub = List[Substitution]()
    var set = Set[Variable1]()
    for (sub <- subs) {
      sub._2 match {
        case v: Variable1 if (!set.contains(v)) => set += v; t = t\\Map(sub._1 -> v);
        case _ => resSub = sub :: resSub
      }
    }
    println("GENERALIZING")
    println(alpha.expr)
    println(beta.expr)
    val let = LetExpression1(resSub, t) 
    println(g)
    println(let)
    tree.replace(alpha, let)
  }
}
