package hosc;

import HLanguage._
import HLanguage1._
import TermAlgebra1._
import HLUtils._
import ProcessTree1._
import MSG1._
import HE1._
import util.Canonizer.{canonize1 => can}

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
        case bTerm: Term1 if canBeEnchanced(bTerm) => 
          beta.ancestors find instanceTest(bTerm) match {
            case Some(alpha) => {beta.repeatedOf = alpha; transformed = true;}
            case None => {
              beta.ancestors find heByCouplingTest(bTerm) match {
                case Some(alpha) => makeAbstraction(alpha, beta); transformed = true
                case None => drive(beta); transformed = true
              }
            }
          }
        case _ => {drive(beta); transformed = true}
      }
      
    }
    transformed
  }
  
  private def canBeEnchanced(t: Term1) = 
    if (t.label==Loop()) {
      true 
    } else if (t.label != Repeat()) decompose1(t) match {
      case c: Context1 => c.redex match { 
        case r: RedexCall1 => true
        case r: RedexCaseVar1 => true
        case r: RedexCaseVarApp1 => true
        case r: RedexLetRec1 => true
        case _ => false
      }
      case _ => false
    } else {
      false
    }
  
  private def instanceTest(bTerm: Term1)(aNode: Node1): Boolean = aNode.expr match {
    case aTerm: Term1 => /*!letrecDirectChild(aNode) &&*/ sameRedex(aTerm, bTerm) && instanceOf(aTerm, bTerm);
    case _ => false
  }
  
  private def heByCouplingTest(bTerm: Term1)(aNode: Node1): Boolean = aNode.expr match {
    case aTerm: Term1 => !letrecDirectChild(aNode) && sameRedex(aTerm, bTerm) && heByCoupling(aTerm, bTerm);
    case _ => false
  } 
  
  private def letrecDirectChild(n: Node1): Boolean = {
    for (a <- n.ancestors) {
      if (a.children.size > 1) {
        return false
      } else {
        a.expr match {
          case possibleLetrec: Term1 => decompose1(possibleLetrec) match {
            case possibleLetRecContext: Context1 => possibleLetRecContext.redex match {
              case RedexLetRec1(_) => if (a.ungeneralized) return true
              case _ => 
            }
          case _ =>
          }
         case _ => 
        }
      }
    }
    false
  }

  
  def sameRedex(t1: Term1, t2: Term1) : Boolean = (decompose1(t1), decompose1(t2)) match {
    case (c1: Context1, c2: Context1) => c1.redex.getClass() == c2.redex.getClass()
    case _ => false
  }
  
  def drive(n: Node1): Unit = {
    tree.addChildren(n, driveExp(n.expr))
  }
  
  def makeAbstraction(alpha: Node1, beta: Node1): Unit = {
    val alphaTerm = alpha.expr.asInstanceOf[Term1]
    val betaTerm = beta.expr.asInstanceOf[Term1]
    val g = msg(alphaTerm, betaTerm)
    var t = g.term
    var subs = g.sub1
    println("GENERALIZING")
    println(can(alphaTerm))
    println(can(betaTerm))
    
    var resSub = List[Substitution]()
    var set = Set[Variable1]()
    for (sub <- subs) {
      sub match {
        case (v1, v2: Variable1) if v1.call => t = t\\Map(v1 -> v2);
        case _ => resSub = sub :: resSub
      }
    }
    
    val let = LetExpression1(resSub, t) 
    println(g)
    println(let)
    tree.replace(alpha, let)
  }
}
