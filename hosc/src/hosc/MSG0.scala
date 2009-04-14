package hosc

import HLanguage._
import TermAlgebra0._

object MSG0 {
  
  type Substitution = Tuple2[Variable, Expression]
  type DoubleSubstitution = Tuple3[Variable, Expression, Expression]
  case class Generalization(term: Expression, sub1: List[Substitution], sub2: List[Substitution])
  case class Generalization2(term: Expression, dSub: List[DoubleSubstitution])
  
  def msg(term1: Expression, term2: Expression): Generalization = {
    null
    def msg_(term1: Expression, term2: Expression): Generalization2 = {
      val initialVar = newVar()
      var g = Generalization2(initialVar, List((initialVar, term1, term2)))
      var exp = g.term
      do {
        exp = g.term
        g = applyCommonFunctorRule(g)
        g = applyCommonSubExpressionRule(g)
      } while (exp != g.term)    
      g
    }
    val g = msg_(term1, term2)
    def f(t1: Expression, t2: Expression): Boolean = (t1, t2) match {
      case (v1: Variable, v2: Variable) => v1.name == v2.name
      case _ => false
    }
    
    val evidentSub = g.dSub filter (tr => f(tr._2, tr._3))
    val residualSub = g.dSub remove (tr => f(tr._2, tr._3))
    val evidentMap = Map[Variable, Expression]() ++ (evidentSub map (tr => (tr._1, tr._2)))
    val term = applySubstitution(g.term, evidentMap)
    val s1 = residualSub.map(triple => (triple._1, triple._2))
    val s2 = residualSub.map(triple => (triple._1, triple._3))
    Generalization(term, s1, s2)
  }
  
  private def applyCommonFunctorRule(g: Generalization2): Generalization2 = {
    val l2 = new scala.collection.mutable.ListBuffer[DoubleSubstitution]()
    var t = g.term;
    for (dSub <- g.dSub) dSub match {
      case (v, Constructor(n1, a1), Constructor(n2, a2)) if n1 == n2 => {
        val newVars = a1.map(arg => newVar())
        val addDSubs = ((newVars zip a1) zip (newVars zip a2)) map (pair => (pair._1._1, pair._1._2, pair._2._2)) 
        t = applySubstitution(t, Map(v -> Constructor(n1, newVars)))
        l2 ++= addDSubs
      }
      case (v, LambdaAbstraction(a1, t1), LambdaAbstraction(a2, t2)) => {
        val arg = newVar() // binder!!
        val rs = newVar()
        val t1r = applySubstitution(t1, Map(a1 -> arg))
        val t2r = applySubstitution(t2, Map(a2 -> arg))        
        t = applySubstitution(t, Map(v -> LambdaAbstraction(arg, rs)))
        l2 ++= List((rs, t1r, t2r))
      }
      case (v, app1: Application, app2: Application) 
      if getAppLevel(app1) == getAppLevel(app2) && getCoreLocalHead(app1) == getCoreLocalHead(app2) => {        
        val head = getCoreLocalHead(app1)
        val args1 = extractAppArgs(app1)
        val args2 = extractAppArgs(app2)
        val newVars = args1.map(arg => newVar())
        val addDSubs = ((newVars zip args1) zip (newVars zip args2)) map (pair => (pair._1._1, pair._1._2, pair._2._2))
        t = applySubstitution(t, Map(v -> constructApplication(head, newVars)))
        l2 ++= addDSubs
      }
      case (v, CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) => {
        val bs1s = bs1 sort compareB
        val bs2s = bs2 sort compareB
        if (bs1s.head.pattern.name == bs2s.head.pattern.name){
          // binders are refreshed and the same
          val bsR = for(bs <- bs1s zip bs2s) yield {
            val newPVars = bs._1.pattern.args map (arg => newVar) //binders!!
            val freshPattern = Pattern(bs._1.pattern.name, newPVars)
            val freshT1 = applySubstitution(bs._1.term, Map[Variable, Expression]() ++ (bs._1.pattern.args zip newPVars))            
            val freshT2 = applySubstitution(bs._2.term, Map[Variable, Expression]() ++ (bs._2.pattern.args zip newPVars))
            (freshPattern, freshT1, freshT2)
          }
          val bVars = bs1s.map(b => newVar())
          val selVar = newVar
          val addDSubs = (selVar, sel1, sel2) :: 
            ((bVars zip bsR) map (pair => (pair._1, pair._2._2, pair._2._3)))
          val newBs = (bsR zip bVars) map (pair => Branch(pair._1._1, pair._2))  
          val newCase = CaseExpression(selVar, newBs)
          t = applySubstitution(t, Map(v -> CaseExpression(selVar, newBs)))
          l2 ++= addDSubs
        } else {
          l2 += dSub
        }
      } 
      case d => l2 += d
    }
    Generalization2(t, l2.toList)
  }
  
  private def f1(ds: DoubleSubstitution, p: Pair[List[DoubleSubstitution], List[DoubleSubstitution]]) = p match {
    case (Nil, l) => l.partition(triple => triple._2 == ds._2 && triple._3 == ds._3) match {
      case (Nil, _) => (Nil, ds :: l) 
      case (same, dif) => (ds :: same, dif)
    }
    case (l1 @ s :: _, l2) => if (ds._2 == s._2 && ds._3 == s._3) (ds :: l1, l2) else (l1, ds :: l2)
  } 

  private def applyCommonSubExpressionRule(g: Generalization2): Generalization2 = {
    g.dSub.foldRight((List[DoubleSubstitution](), List[DoubleSubstitution]()))(f1) match {
      case (Nil, _) => g
      case ((s @ (v, _, _)) :: o1, o2) => 
        Generalization2(o1.foldRight(g.term)((ds, t) => applySubstitution(t, Map[Variable, Expression](ds._1 -> v))), s :: o2)
    }
  }
  // term1 is equivalent with msg
  def strongMsg(term1: Expression, term2: Expression): Generalization = {
    val g = msg(term1, term2)
    var term = g.term
    for (s <- g.sub1) term = applySubstitution(term, Map(s))
    
    var newS = ((g.sub1 zip g.sub2) map {p => (p._1._2.asInstanceOf[Variable], p._2._2)}) remove (p => p._1 == p._2)
    Generalization(term, Nil, newS)    
  }
  
  private def getAppLevel(app: Application): Int = app.head match {
    case a: Application => 1 + getAppLevel(a);
    case h => 1;
  }
}
