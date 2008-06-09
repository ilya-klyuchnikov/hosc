package hosc;

import HLanguage1._
import TermAlgebra1._

object MSG1 {
  type Substitution = Tuple2[Variable1, Term1]
  type DoubleSubstitution = Tuple3[Variable1, Term1, Term1]
  case class Generalization(term: Term1, sub1: List[Substitution], sub2: List[Substitution])
  case class Generalization2(term: Term1, dSub: List[DoubleSubstitution])
  
  def msg(term1: Term1, term2: Term1): Generalization = {
    
    def msg_(term1: Term1, term2: Term1): Generalization2 = {
      val initialVar = newVar1()
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
    val bv = getBoundedVars(g.term) // alpha conversion!!
    def f(t1: Term1, t2: Term1): Boolean = (t1, t2) match {
      case (v1: Variable1, v2: Variable1) =>
        v1.name == v2.name  && (v1.call == true && v2.call == true || bv.contains(v1) && bv.contains(v2)) 
      case _ => false
    }
    
    val evidentSub = g.dSub filter (tr => f(tr._2, tr._3))
    val residualSub = g.dSub remove (tr => f(tr._2, tr._3))
    val evidentMap = Map[Variable1, Term1]() ++ (evidentSub map (tr => (tr._1, tr._2)))
    val term = g.term/evidentMap
    val s1 = residualSub.map(triple => (triple._1, triple._2))
    val s2 = residualSub.map(triple => (triple._1, triple._3))
    Generalization(term, s1, s2)
  }
  
  private def applyCommonFunctorRule(g: Generalization2): Generalization2 = {    
    val l2 = new scala.collection.mutable.ListBuffer[DoubleSubstitution]()
    var t = g.term;
    // terms need to have the same label to match:
    for (dSub <- g.dSub) if (dSub._2.label != dSub._3.label) l2 += dSub else dSub match {
      case (v, Constructor1(n1, a1), Constructor1(n2, a2)) if n1 == n2 => {
        val newVars = a1.map(arg => newVar1())
        val addDSubs = ((newVars zip a1) zip (newVars zip a2)) map (pair => (pair._1._1, pair._1._2, pair._2._2)) 
        t = t/Map(v -> Constructor1(n1, newVars))
        l2 ++= addDSubs
      }
      case (v, LambdaAbstraction1(a1, t1), LambdaAbstraction1(a2, t2)) => {
        val arg = newVar1() // binder!!
        val rs = newVar1()
        val t1r = t1/Map(a1 -> arg)
        val t12 = t2/Map(a2 -> arg)        
        t = t/Map(v -> LambdaAbstraction1(arg, rs))
        l2 += (rs, t1, t2)
      }
      case (v, app1: Application1, app2: Application1) 
      if getAppLevel(app1) == getAppLevel(app2) && getCoreLocalHead(app1) == getCoreLocalHead(app2) => {        
        val head = getCoreLocalHead(app1)
        val args1 = extractAppArgs(app1)
        val args2 = extractAppArgs(app2)
        val newVars = args1.map(arg => newVar1())
        val addDSubs = ((newVars zip args1) zip (newVars zip args2)) map (pair => (pair._1._1, pair._1._2, pair._2._2))
        t = t/Map(v -> constructApplication1(head, newVars))
        l2 ++= addDSubs
      }
      case (v, LetRecExpression1((f1, a1), e1), LetRecExpression1((f2, a2), e2)) => {
        val f = newVar1(); f.call = true
        val e = newVar1(); val a = newVar1();
        l2 += (e, e1/Map(f1->f), e2/Map(f2->f))
        l2 += (f, f1/Map(f1->f), f2/Map(f2->f))
        l2 += (a, a1/Map(f1->f), a2/Map(f2->f))
        t = t/Map(v -> LetRecExpression1((f, a), e))
      }
      case (v, CaseExpression1(sel1, bs1), CaseExpression1(sel2, bs2)) => {
        val bs1s = bs1 sort compareB
        val bs2s = bs2 sort compareB
        if (bs1s.head.pattern.name == bs2s.head.pattern.name){
          // binders are refreshed and the same
          val bsR = for(bs <- bs1s zip bs2s) yield {
            val newPVars = bs._1.pattern.args map (arg => newVar1())//binders!!
            val rp = Pattern1(bs._1.pattern.name, newPVars)
            val rt1 = bs._1.term/(Map[Variable1, Term1]() ++ (bs._1.pattern.args zip newPVars))            
            val rt2 = bs._2.term/(Map[Variable1, Term1]() ++ (bs._2.pattern.args zip newPVars))
            (rp, rt1, rt2)
          }
          val bVars = bs1s.map(b => newVar1())
          val selVar = newVar1
          val addDSubs = (selVar, sel1, sel2) :: 
            ((bVars zip bsR) map (pair => (pair._1, pair._2._2, pair._2._3)))
          val newBs = (bsR zip bVars) map (pair => Branch1(pair._1._1, pair._2))  
          val newCase = CaseExpression1(selVar, newBs)
          t = t/Map(v -> CaseExpression1(selVar, newBs))
          l2 ++= addDSubs
        }
      }
      case d => l2 += d
    }
    Generalization2(t, l2.toList)
  }
  
  def getCoreLocalHead(app: Application1): Term1 = app.head match {
    case a: Application1 => getCoreLocalHead(a)
    case h => h
  }
  
  private def getAppLevel(app: Application1): Int = app.head match {
    case a: Application1 => 1 + getAppLevel(a);
    case h => 1;
  }
  
  //returns Pair[List[DoubleSubstitution], List[DoubleSubstitution]]
  // the first element of pair is list of common subs
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
        Generalization2(o1.foldRight(g.term)((ds, t) => t/Map(ds._1 -> v)), s :: o2)
    }
  }
  
}
