package hosc;

import HLanguage1._
import MSG1.msg
import scala.collection.mutable.ListBuffer

object TermAlgebra1 {
  var i = 0
  def newVar1() = {
    i += 1
    Variable1("$" + i) 
  }

  sealed abstract class TermDecomposition1
  
  sealed abstract class Observable1(val term: Term1) extends TermDecomposition1
  case class ObservableVar1(v: Variable1) extends Observable1(v)
  case class ObservableVarApp1(v: Variable1, app: Application1) extends Observable1(app)
  case class ObservableCon1(c: Constructor1) extends Observable1(c)
  case class ObservableLam1(l: LambdaAbstraction1) extends Observable1(l)
  
  sealed abstract class Redex1(val term : Term1)
  case class RedexCall1(v: Variable1) extends Redex1(v)
  case class RedexLetRec1(letrec: LetRecExpression1) extends Redex1(letrec)
  case class RedexLamApp1(lam: LambdaAbstraction1, app: Application1) extends Redex1(app)
  case class RedexCaseVarApp1(a: Application1, ce: CaseExpression1) extends Redex1(ce)
  case class RedexCaseVar1(v: Variable1, ce: CaseExpression1) extends Redex1(ce)  
  case class RedexCaseCon1(c: Constructor1, ce: CaseExpression1) extends Redex1(ce)
  
  sealed abstract class Context1(val redex: Redex1) extends TermDecomposition1 {
    def replaceHole(t: Term1): Term1
  }  
  case class ContextHole1(override val redex: Redex1) extends Context1(redex) {
    def replaceHole(t: Term1) = t
  }
  // app = con e 
  case class ContextApp1(head: Context1, app: Application1) extends Context1(head.redex) {
    def replaceHole(t: Term1) = Application1(head.replaceHole(t), app.arg)
  }
  // ce = case selector of ....
  case class ContextCase1(selector: Context1, ce: CaseExpression1) extends Context1(selector.redex) {
    def replaceHole(t: Term1) = CaseExpression1(selector.replaceHole(t), ce.branches)
  }
  
  def decompose1(t: Term1): TermDecomposition1 = t match {
    case c: Constructor1 => ObservableCon1(c)
    case l: LambdaAbstraction1 => ObservableLam1(l)
    case app: Application1 if getCoreLocalVar(app) != null => ObservableVarApp1(getCoreLocalVar(app), app)
    case v: Variable1 if !v.call => ObservableVar1(v)
    case contextTerm => createContext(contextTerm)
  }
  
  def createContext(t: Term1): Context1 = t match {
    case v: Variable1 if (v.call) => ContextHole1(RedexCall1(v))
    case letrec: LetRecExpression1 => ContextHole1(RedexLetRec1(letrec))
    // suppose that such term is imposssible in distilled form
    case app @ Application1(l: LambdaAbstraction1, arg) => ContextHole1(RedexLamApp1(l, app))
    case ce @ CaseExpression1(v: Variable1, _)  => ContextHole1(RedexCaseVar1(v, ce))
    case ce @ CaseExpression1(a: Application1, _) if (getCoreLocalVar(a) != null) => 
      ContextHole1(RedexCaseVarApp1(a, ce))
    case ce @ CaseExpression1(c: Constructor1, _) => ContextHole1(RedexCaseCon1(c, ce))
    case a @ Application1(h, _) => ContextApp1(createContext(h), a)
    case ce @ CaseExpression1(s, _) => ContextCase1(createContext(s), ce)
    case _ => throw new IllegalArgumentException(t.toString)
  }
  
  private def getCoreLocalVar(app: Application1): Variable1 = app.head match {
    case v: Variable1 if (!v.call) => v
    case a: Application1 => getCoreLocalVar(a)
    case _ => null
  }
  
  def getCoreAppVar(app: Application1): Variable1 = app.head match {
    case v: Variable1 => v
    case a: Application1 => getCoreAppVar(a)
    case _ => null
  }
  
  // replace all occurrences of t1 in term by t2 
  def replaceTerm1(term: Term1, t1: Term1, t2: Term1): Term1 = if (term == t1) t2 else term match {
    case v: Variable1 => v
    case c@Constructor1(n, args) => {
      val newC = Constructor1(n, args map {a => replaceTerm1(a, t1, t2)})
      newC.label = c.label
      newC
    }
    case app@Application1(h, a) => {
      val newApp = Application1(replaceTerm1(h, t1, t2), replaceTerm1(a, t1, t2))
      newApp.label = app.label
      newApp
    }
    case lambda@LambdaAbstraction1(v, t) => {
      val newLambda = LambdaAbstraction1(v, replaceTerm1(t, t1, t2))
      newLambda.label = lambda.label
      newLambda
    }
    case caze@CaseExpression1(sel, bs) => { 
      val newCase = CaseExpression1(replaceTerm1(sel, t1, t2), 
          bs map {b => Branch1(b.pattern, replaceTerm1(b.term, t1, t2))})
      newCase.label = caze.label
      newCase
    }
    case letrec @ LetRecExpression1((v, lambda), expr) => {
      val newLetrec = LetRecExpression1((v, replaceTerm1(lambda, t1, t2)), replaceTerm1(expr, t1, t2))
      newLetrec.label = letrec.label
      newLetrec
    }
  }
  
  def extractAppArgs(term: Term1): List[Term1] = term match {
    case Application1(h, a) => extractAppArgs(h) ::: List(a)
    case _ => Nil
  }
  
  def extractAppArgs1(term: Term1): List[Term1] = term match {
    case Application1(h, a) => extractAppArgs1(h) ::: List(a)
    case _ => Nil
  }
  
  def getAllVars1(expr: Expression1): Set[Variable1] = expr match {
    case v: Variable1 => Set(v)
    case Constructor1(_, args) => (Set[Variable1]() /: args) {(vs, term) => vs ++ getAllVars1(term)}
    case LambdaAbstraction1(x, term) => getAllVars1(term) + x
    case Application1(head, arg) => getAllVars1(head) ++ getAllVars1(arg)
    case CaseExpression1(sel, bs) => 
      getAllVars1(sel) ++ (Set[Variable1]() /: bs) {(vs, b) => vs ++ getAllVars1(b.term) ++ b.pattern.args}
    case LetExpression1(bs, expr) =>
      getAllVars1(expr) ++ (Set[Variable1]() /: bs) {(vs, b) => vs ++ getAllVars1(b._2) + b._1}
    case LetRecExpression1(b, expr) =>
      getAllVars1(expr) ++ getAllVars1(b._2) + b._1
  }
  
  def getVarsOrdered(exprr: Term1): List[Variable1] = {
    val buffer = new ListBuffer[Variable1]
    def dump(expr: Term1): Unit = expr match {
      case v: Variable1 => if (!buffer.contains(v)) buffer + v 
      case Constructor1(_, args) => args map dump
      case LambdaAbstraction1(x, term) => dump(x); dump(term)
      case Application1(head, arg) => dump(head); dump(arg);
      case CaseExpression1(sel, bs) => dump(sel); bs map {b => b.pattern.args map dump; dump(b.term)};
      case LetRecExpression1(b, term) => dump(b._1); dump(b._2); dump(term);
    }
    dump(exprr)
    buffer.toList
  }
  
  def constructLambda1(vs: List[Variable1], e: Term1): Term1 = {
    def constructLambda_(vs_ : List[Variable1]) : Term1 = vs_ match {
      case Nil => e;
      case v :: vv => LambdaAbstraction1(v, constructLambda_(vv))
    }
    constructLambda_(vs)
  }
  
  def constructApplication1(head: Term1, args: List[Term1]): Term1 = {
    var res = head
    var list = args
    while (!list.isEmpty) {
      res = Application1(res, list.head)
      list = list.tail
    }
    res
  }
  
  def compareB1(b1: Branch1, b2: Branch1) = b1.pattern.name.compareTo(b2.pattern.name) < 0
  
  def getBoundedVars(t: Term1): Set[Variable1] = t match {
    case v: Variable1 => Set()
    case Constructor1(_, args) => (Set[Variable1]() /: args) {(vs, term) => vs ++ getBoundedVars(term)}
    case LambdaAbstraction1(x, term) => getBoundedVars(term) + x
    case Application1(head, arg) => getBoundedVars(head) ++ getBoundedVars(arg)
    case CaseExpression1(sel, bs) => 
      getBoundedVars(sel) ++ (Set[Variable1]() /: bs) {(vs, b) => vs ++ (getBoundedVars(b.term) ++ b.pattern.args)}
    case LetRecExpression1(binding, expr) => getBoundedVars(binding._2) ++ getBoundedVars(expr)
  }
  
  def instanceOf(t1: Term1, t2: Term1): Boolean = equivalent(msg(t1, t2).term, t1)
  
  // During unfolding we always rename functions 
  // in a way that bound vars (in lambda absractions and case expressions) are refreshed.
  // This method assumes that binders always have different names.
  def equivalent(term1: Term1, term2: Term1): Boolean = {
    val map1to2 = scala.collection.mutable.Map[Variable1, Variable1]()
    val map2to1 = scala.collection.mutable.Map[Variable1, Variable1]()
    def eq1(t1: Term1, t2: Term1): Boolean = if (t1.label != t2.label) false else (t1, t2) match {
      case (v1: Variable1, v2: Variable1) if v1.call == true && v2.call == true =>
        if (v1.name == v2.name) 
          true
        else (map1to2.get(v1), map2to1.get(v2)) match {
          case (Some(v3), Some(v4)) => v2 == v3 && v1 == v4
          case _ => false
        }
      case (v1: Variable1, v2: Variable1) if v1.call == false && v2.call == false => 
      (map1to2.get(v1), map2to1.get(v2)) match {
        case (Some(v3), Some(v4)) => 
          v2 == v3 && v1 == v4
        case (None, None) => map1to2(v1) = v2; map2to1(v2) = v1; true
        case _ => false
      }
      case (Constructor1(name1, args1), Constructor1(name2, args2)) if name1 == name2 =>
        ((args1 zip args2) forall (args => eq1(args._1, args._2)))
      case (Application1(h1, a1), Application1(h2, a2)) => 
        eq1(h1, h2) && eq1(a1, a2)
      case (LambdaAbstraction1(b1, v1), LambdaAbstraction1(b2, v2)) =>
        eq1(b1, b2) && eq1(v1, v2)
      case (CaseExpression1(sel1, bs1), CaseExpression1(sel2, bs2)) => {
        val bs1s = bs1 sort compareB1
        val bs2s = bs2 sort compareB1
        if (bs1s.head.pattern.name == bs2s.head.pattern.name){
          eq1(sel1, sel2) && ((bs1s zip bs2s) forall {
            b => ((b._1.pattern.args zip b._2.pattern.args) forall (args => eq1(args._1, args._2))) &&
              eq1(b._1.term, b._2.term)
          })
        } else {
          false
        }
      }
      case (LetRecExpression1((f1, a1), e1), LetRecExpression1((f2, a2), e2)) => {
        map1to2(f1) = f2; map2to1(f2) = f1;
        eq1(a1, a2) && eq1(e1, e2)
      }
      case _ => 
        false
    }    
    eq1(term1, term2)
  }
  
  def freshBinders(term: Term1): Term1 = term match {
    case v: Variable1 => val v1 = Variable1(v.name); v1.call = v.call; v1.label = v.label; v1
    case c@Constructor1(name, args) => val t=Constructor1(name, args map (freshBinders(_))); t.label = c.label; t 
    case app@Application1(h, a) => val t=Application1(freshBinders(h), freshBinders(a)); t.label=app.label;t
    case la@LambdaAbstraction1(v, t) => {
      val freshV = newVar1()
      val t1 = LambdaAbstraction1(freshV, freshBinders(t)/Map(v -> freshV))
      t1.label = la.label; t1
    }
    case ce@CaseExpression1(sel, bs) => {
      val t = CaseExpression1(freshBinders(sel), bs map {freshBinders(_)})
      t.label = ce.label
      t
    }
    case lr@LetRecExpression1((v, term), expr) => {
      val t = LetRecExpression1((v, freshBinders(term)), freshBinders(expr))
      t.label = lr.label; t
    }
  }
  
  def freshAllBinders(term: Term1): Term1 = term match {
    case v: Variable1 => val v1 = Variable1(v.name); v1.call = v.call; v1.label = v.label; v1
    case c@Constructor1(name, args) => val t=Constructor1(name, args map (freshAllBinders(_))); t.label = c.label; t 
    case app@Application1(h, a) => val t=Application1(freshAllBinders(h), freshAllBinders(a)); t.label=app.label;t
    case la@LambdaAbstraction1(v, t) => {
      val freshV = newVar1()
      val t1 = LambdaAbstraction1(freshV, freshAllBinders(t)/Map(v -> freshV))
      t1.label = la.label; t1
    }
    case ce@CaseExpression1(sel, bs) => {
      val t = CaseExpression1(freshAllBinders(sel), bs map {freshAllBinders(_)})
      t.label = ce.label
      t
    }
    case lr@LetRecExpression1((v, term), expr) => {
      val freshV = newVar1()
      val t = LetRecExpression1((freshV, freshAllBinders(term)/Map(v -> freshV)), freshAllBinders(expr)/Map(v -> freshV))
      t.label = lr.label; t
    }
  }
  
  def freshAllBinders(b: Branch1): Branch1 = {
    val args = b.pattern.args
    val newVars = args map {x => newVar1()}
    Branch1(Pattern1(b.pattern.name, newVars), freshAllBinders(b.term)/Map((args zip newVars):_*))
  }

  def freshBinders(b: Branch1): Branch1 = {
    val args = b.pattern.args
    val newVars = args map {x => newVar1()}
    Branch1(Pattern1(b.pattern.name, newVars), freshBinders(b.term)/Map((args zip newVars):_*))
  }
  
  def callInRedex1_?(t: Term1) = decompose1(t) match {
    case c: Context1 => c.redex match { 
      case r: RedexCall1 => true
      case _ => false
    }
    case _ => false
  }
  
  def getFreeVars(t: Term1): Set[Variable1] = t match {
    case v: Variable1 => if (v.call) Set() else Set(v)
    case Constructor1(_, args) => (Set[Variable1]() /: args) {(vs, term) => vs ++ getFreeVars(term)}
    case LambdaAbstraction1(x, term) => getFreeVars(term) - x
    case Application1(head, arg) => getFreeVars(head) ++ getFreeVars(arg)
    case CaseExpression1(sel, bs) => 
      getFreeVars(sel) ++ (Set[Variable1]() /: bs) {(vs, b) => vs ++ (getFreeVars(b.term) -- b.pattern.args)}
    case LetRecExpression1((v, term), expr) => getFreeVars(term) ++ getFreeVars(expr) - v
  }

}
