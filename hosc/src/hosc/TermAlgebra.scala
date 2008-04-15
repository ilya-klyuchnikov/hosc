package hosc;

import HLanguage._

object TermAlgebra {
  var i = 0
  def newVar() = {
    i += 1
    Variable("$" + i) 
  }
  type Substitution = Tuple2[Variable, Term]
  type DoubleSubstitution = Tuple3[Variable, Term, Term]
  case class Generalization(term: Term, sub1: List[Substitution], sub2: List[Substitution])
  case class Generalization2(term: Term, dSub: List[DoubleSubstitution])
  
  sealed abstract class TermDecomposition 
  sealed abstract class Observable extends TermDecomposition {
    val term: Term
  }
  //such observable can not be encountered during interpretation
  case class ObservableVar(v: Variable) extends Observable {
    val term = v
  }
  // such observable can not be encountered during interpretation
  case class ObservableVarApp(v: Variable, app: Application) extends Observable {
    val term = app
  }
  case class ObservableCon(c: Constructor) extends Observable {
    val term = c
  }
  case class ObservableLam(l: LambdaAbstraction) extends Observable {
    val term = l
  }
  
  sealed abstract class Redex {
    val term : Term
  }
  // f - global var
  case class RedexCall(v: Variable) extends Redex {
    val term = v
  }
  //app = lam arg
  case class RedexLamApp(lam: LambdaAbstraction, app: Application) extends Redex {
    val term = app
  }
  // ce = case (v e1 e2 e3) of ...
  // such redex can not be encountered during interpretation
  case class RedexCaseVarApp(a: Application, ce: CaseExpression) extends Redex {
    val term = ce;
  }
  //ce = case (v) of ...
  // such redex can not be encountered during interpretation
  case class RedexCaseVar(v: Variable, ce: CaseExpression) extends Redex {
    val term = ce;
  }
  
  case class RedexCaseCon(c: Constructor, ce: CaseExpression) extends Redex {
    val term = ce;
  }
  
  sealed abstract class Context extends TermDecomposition {
    def replaceHole(t: Term): Term
    def redex: Redex
  }
  
  case class ContextHole(redex: Redex) extends Context {
    def replaceHole(t: Term) = t
  }
  // app = con e 
  case class ContextApp(head: Context, app: Application) extends Context {
    def redex = head.redex
    def replaceHole(t: Term) = Application(head.replaceHole(t), app.arg)
  }
  // ce = case selector of ....
  case class ContextCase(selector: Context, ce: CaseExpression) extends Context {
    def redex = selector.redex
    def replaceHole(t: Term) = CaseExpression(selector.replaceHole(t), ce.branches)
  }  
  
  def decompose(t: Term): TermDecomposition = t match {
    // observable
    case c: Constructor => ObservableCon(c)
    case l: LambdaAbstraction => ObservableLam(l)
    case app: Application if getCoreLocalVar(app)!=null => ObservableVarApp(getCoreLocalVar(app), app)
    // context
    case t => createContext(t)
  }
  
  def createContext(t: Term): Context = t match {
    case v: Variable if (v.global) => ContextHole(RedexCall(v))
    case app @ Application(l: LambdaAbstraction, arg) => ContextHole(RedexLamApp(l, app))
    case ce @ CaseExpression(v: Variable, _) if !v.global => ContextHole(RedexCaseVar(v, ce))
    case ce @ CaseExpression(a: Application, _) if (getCoreLocalVar(a) != null) => 
      ContextHole(RedexCaseVarApp(a, ce))
    case ce @ CaseExpression(c: Constructor, _) => ContextHole(RedexCaseCon(c, ce))
    case a @ Application(h, _) => ContextApp(createContext(h), a)
    case ce @ CaseExpression(s, _) => ContextCase(createContext(s), ce)
    case _ => throw new IllegalArgumentException(t.toString)
  }
  
  private def getCoreLocalVar(app: Application): Variable = app.head match {
    case v: Variable if (!v.global)=> v
    case a: Application => getCoreLocalVar(a)
    case _ => null
  }
  
  def applySubstitution(term: Term, s: Map[Variable, Term]): Term = term match {
    case v: Variable => s.get(v) match {case Some(t) => t; case None => v}
    case Constructor(n, args) => Constructor(n, args map {applySubstitution(_, s)})
    case LambdaAbstraction(v, t) => LambdaAbstraction(v, applySubstitution(t, s))
    case Application(h, a) => Application(applySubstitution(h, s), applySubstitution(a, s))
    case CaseExpression(sel, bs) => 
      CaseExpression(applySubstitution(sel, s), bs map {b => Branch(b.pattern, applySubstitution(b.term, s))})
  }
  
  def he(term1: Term, term2: Term): Boolean = 
    heByVar(term1, term2) || heByDiving(term1, term2) || heByCoupling(term1, term2)
  
  private def heByVar(term1: Term, term2: Term): Boolean = (term1, term2) match {
    case (v1: Variable, v2: Variable) => v1.global == true && v2.global == true && v1.name == v2.name ||
      v1.global == false && v2.global == false  
    case _ => false
  }
  
  private def heByDiving(term1: Term, term2: Term): Boolean = term2 match {
    case Constructor(_, args) => args exists (he(term1, _))
    case LambdaAbstraction(_, t) => he(term1, t)
    case Application(h, a) => he(term1, h) || he(term1, a)
    case CaseExpression(sel, bs) => he(term1, sel) || (bs exists (b => he(term1, b.term)))
    case _ => false
  }
  
  private def heByCoupling(term1: Term, term2: Term): Boolean = (term1, term2) match {
    case (Constructor(name1, args1), Constructor(name2, args2)) if name1 == name2 => 
      (args1 zip args2) forall (args => he(args._1, args._2))
    case (LambdaAbstraction(_, t1), LambdaAbstraction(_, t2)) => he(t1, t2)
    case (Application(h1, a1), Application(h2, a2)) => he(h1, h1) && he(a1, a2)
    case (CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) => {
      val bs1_ = bs1 sort compareB
      val bs2_ = bs2 sort compareB
      he(sel1, sel2) && 
        ((bs1_ zip bs2_) forall (bs => bs._1.pattern.name == bs._2.pattern.name && he(bs._1.term, bs._2.term)))
    }
    case _ => false
  }  
  
  def msg(term1: Term, term2: Term): Generalization = {
    def msg_(term1: Term, term2: Term): Generalization2 = {
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
    def sameCall(t1: Term, t2: Term) = (t1, t2) match {
      case (v1: Variable, v2: Variable) => v1.global == true && v2.global == true && v1.name == v2.name
      case _ => true
    }
    val evidentSub = g.dSub filter (tr => sameCall(tr._2, tr._3))
    val residualSub = g.dSub remove (tr => sameCall(tr._2, tr._3))
    val evidentMap = Map[Variable, Term]() ++ (evidentSub map (tr => (tr._1, tr._2)))
    val term = applySubstitution(g.term, evidentMap)
    val s1 = residualSub.map(triple => (triple._1, triple._2))
    val s2 = residualSub.map(triple => (triple._1, triple._3))
    Generalization(g.term, s1, s2)
  }
  
  def compareB(b1: Branch, b2: Branch) = b1.pattern.name.compareTo(b2.pattern.name) < 0
  
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
        val arg = newVar()
        val rs = newVar()
        t = applySubstitution(t, Map(v -> LambdaAbstraction(arg, rs)))
        l2 ++= List((arg, a1, a2), (rs, t1, t2))
      }
      case (v, Application(h1, a1), Application(h2, a2)) => {
        val head = newVar()
        val arg = newVar()
        t = applySubstitution(t, Map(v -> Application(head, arg)))
        l2 ++= List((head, h1, h2), (arg, a1, a2))
      }
      case (v, CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) => {
        val bs1s = bs1 sort compareB
        val bs2s = bs2 sort compareB
        if (bs1s.head.pattern.name == bs2s.head.pattern.name){
          val bsR = for(bs <- bs1s zip bs2s) yield {
            val newPVars = bs._1.pattern.args map (arg => newVar)
            val rp = Pattern(bs._1.pattern.name, newPVars)
            val rt1 = applySubstitution(bs._1.term, Map[Variable, Term]() ++ (bs._1.pattern.args zip newPVars))            
            val rt2 = applySubstitution(bs._2.term, Map[Variable, Term]() ++ (bs._2.pattern.args zip newPVars))
            (rp, rt1, rt2)
          }
          val bVars = bs1s.map(b => newVar())
          val selVar = newVar
          val addDSubs = (selVar, sel1, sel2) :: 
            ((bVars zip bsR) map (pair => (pair._1, pair._2._2, pair._2._3)))
          val newBs = (bsR zip bVars) map (pair => Branch(pair._1._1, pair._2))  
          val newCase = CaseExpression(selVar, newBs)
          t = applySubstitution(t, Map(v -> CaseExpression(selVar, newBs)))
          l2 ++= addDSubs
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
        Generalization2(o1.foldRight(g.term)((ds, t) => applySubstitution(t, Map[Variable, Term](ds._1 -> v))), s :: o2)
    }
  }
  
  // During unfolding we always rename functions 
  // in a way that bound vars (in lambda absractions and case expressions) are refreshed.
  // This method assumes that binders always have different names.
  def equivalent(term1: Term, term2: Term): Boolean = {
    val map1to2 = scala.collection.mutable.Map[Variable, Variable]()
    val map2to1 = scala.collection.mutable.Map[Variable, Variable]()
    def eq1(t1: Term, t2: Term): Boolean = (t1, t2) match {
      case (v1: Variable, v2: Variable) if v1.global == true && v2.global == true =>
        v1.name == v2.name
      case (v1: Variable, v2: Variable) if v1.global == false && v2.global == false => 
      (map1to2.get(v1), map2to1.get(v2)) match {
        case (Some(v3), Some(v4)) => 
          v2 == v3 && v1 == v4
        case (None, None) => map1to2(v1) = v2; map2to1(v2) = v1; true
        case _ => false
      }
      case (Constructor(name1, args1), Constructor(name2, args2)) if name1 == name2 =>
        ((args1 zip args2) forall (args => eq1(args._1, args._2)))
      case (Application(h1, a1), Application(h2, a2)) => 
        eq1(h1, h2) && eq1(h1, h2)
      case (LambdaAbstraction(b1, v1), LambdaAbstraction(b2, v2)) =>
        eq1(b1, b2) && eq1(v1, v2)
      case (CaseExpression(sel1, bs1), CaseExpression(sel2, bs2)) => {
        val bs1s = bs1 sort compareB
        val bs2s = bs2 sort compareB
        if (bs1s.head.pattern.name == bs2s.head.pattern.name){
          eq1(sel1, sel2) && ((bs1s zip bs2s) forall {
            b => ((b._1.pattern.args zip b._2.pattern.args) forall (args => eq1(args._1, args._2))) &&
              eq1(b._1.term, b._2.term)
          })
        } else {
          false
        }
      }
      case _ => 
        false
    }    
    eq1(term1, term2)
  }
  
}
