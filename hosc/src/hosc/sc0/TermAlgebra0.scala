package hosc.sc0

import HLanguage._
import MSG0._

object TermAlgebra0 {
  var i = 0
  def newVar() = {
    i += 1
    Variable("$" + i) 
  }
  
  sealed abstract class ExpressionDecomposition
  
  sealed abstract class Observable(val term: Expression) extends ExpressionDecomposition
  case class ObservableVar(v: Variable) extends Observable(v)
  case class ObservableVarApp(v: Variable, app: Application) extends Observable(app)
  case class ObservableCon(c: Constructor) extends Observable(c)
  case class ObservableLam(l: LambdaAbstraction) extends Observable(l)
  
  sealed abstract class Redex(val term : Expression)
  case class RedexCall(v: Variable) extends Redex(v)
  case class RedexLamApp(lam: LambdaAbstraction, app: Application) extends Redex(app)
  case class RedexCaseVarApp(a: Application, ce: CaseExpression) extends Redex(ce)
  case class RedexCaseVar(v: Variable, ce: CaseExpression) extends Redex(ce)  
  case class RedexCaseCon(c: Constructor, ce: CaseExpression) extends Redex(ce)
  
  sealed abstract class Context(val redex: Redex) extends ExpressionDecomposition {
    def replaceHole(t: Expression): Expression
  }  
  case class ContextHole(override val redex: Redex) extends Context(redex) {
    def replaceHole(t: Expression) = t
  }
  // app = con e 
  case class ContextApp(head: Context, app: Application) extends Context(head.redex) {
    def replaceHole(t: Expression) = Application(head.replaceHole(t), app.arg)
  }
  // ce = case selector of ....
  case class ContextCase(selector: Context, ce: CaseExpression) extends Context(selector.redex) {
    def replaceHole(t: Expression) = CaseExpression(selector.replaceHole(t), ce.branches)
  }  
  
  def decompose(t: Expression): ExpressionDecomposition = t match {
    // observable
    case c: Constructor => ObservableCon(c)
    case l: LambdaAbstraction => ObservableLam(l)
    case app: Application if getCoreLocalVar(app)!=null => ObservableVarApp(getCoreLocalVar(app), app)
    case v: Variable if !v.global => ObservableVar(v)
    // context
    case t => createContext(t)
  }
  
  def createContext(t: Expression): Context = t match {
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
  
  def applySubstitution(term: Expression, s: Map[Variable, Expression]): Expression = term match {
    case v: Variable => s.get(v) match {case Some(t) => t; case None => v}
    case Constructor(n, args) => Constructor(n, args map {applySubstitution(_, s)})
    case LambdaAbstraction(v, t) => 
      LambdaAbstraction(v, applySubstitution(t, s))
    case Application(h, a) => Application(applySubstitution(h, s), applySubstitution(a, s))
    case CaseExpression(sel, bs) => 
      CaseExpression(applySubstitution(sel, s), 
          bs map {b => Branch(b.pattern, applySubstitution(b.term, s))})
  }
  
  private def getBoundedVars(t: Expression): Set[Variable] = t match {
    case v: Variable => Set()
    case Constructor(_, args) => (Set[Variable]() /: args) {(vs, term) => vs ++ getBoundedVars(term)}
    case LambdaAbstraction(x, term) => getBoundedVars(term) + x
    case Application(head, arg) => getBoundedVars(head) ++ getBoundedVars(arg)
    case CaseExpression(sel, bs) => 
      getBoundedVars(sel) ++ (Set[Variable]() /: bs) {(vs, b) => vs ++ (getBoundedVars(b.term) ++ b.pattern.args)}
  }
  
  def getFreeVars(t: Expression): Set[Variable] = t match {
    case v: Variable => if (v.global) Set() else Set(v)
    case Constructor(_, args) => (Set[Variable]() /: args) {(vs, term) => vs ++ getFreeVars(term)}
    case LambdaAbstraction(x, term) => getFreeVars(term) - x
    case Application(head, arg) => getFreeVars(head) ++ getFreeVars(arg)
    case CaseExpression(sel, bs) => 
      getFreeVars(sel) ++ (Set[Variable]() /: bs) {(vs, b) => vs ++ (getFreeVars(b.term) -- b.pattern.args)}
    case LetRecExpression((f, e), e0) => getFreeVars(e) ++ getFreeVars(e0) - f
  }
  
  def compareB(b1: Branch, b2: Branch) = b1.pattern.name.compareTo(b2.pattern.name) < 0
  
  def getCoreLocalHead(app: Application): Expression = app.head match {
    case a: Application => getCoreLocalHead(a)
    case h => h
  }
  
  def constructApplication(head: Expression, args: List[Expression]): Expression = {
    var res = head
    var list = args
    while (!list.isEmpty) {
      res = Application(res, list.head)
      list = list.tail
    }
    res
  }
  
  // During unfolding we always rename functions 
  // in a way that bound vars (in lambda absractions and case expressions) are refreshed.
  // This method assumes that binders always have different names.
  def equivalent(term1: Expression, term2: Expression): Boolean = {
    val map1to2 = scala.collection.mutable.Map[Variable, Variable]()
    val map2to1 = scala.collection.mutable.Map[Variable, Variable]()
    def eq1(t1: Expression, t2: Expression): Boolean = (t1, t2) match {
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
        eq1(h1, h2) && eq1(a1, a2)
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
  
  def freshBinders(term: Expression): Expression = term match {
    case Constructor(name, args) => Constructor(name, args map (freshBinders(_)))
    case Application(h, a) => Application(freshBinders(h), freshBinders(a))
    case LambdaAbstraction(v, t) => {
      val freshV = newVar()
      LambdaAbstraction(freshV, applySubstitution(freshBinders(t), Map(v -> freshV)))
    }
    case CaseExpression(sel, bs) => CaseExpression(freshBinders(sel), bs map {freshBinders(_)})
    case v: Variable => v
  }
  
  def freshBinders(b: Branch): Branch = {
    val args = b.pattern.args
    val newVars = args map {x => newVar()}
    Branch(Pattern(b.pattern.name, newVars), 
        applySubstitution(freshBinders(b.term), Map[Variable, Expression]() ++ (args zip newVars)))
  }

  // replace all occurrences of t1 in term by t2 
  def replaceTerm(term: Expression, t1: Expression, t2: Expression): Expression = if (term == t1) t2 else term match {
    case v: Variable => v
    case Constructor(n, args) => Constructor(n, args map {a => replaceTerm(a, t1, t2)})
    case Application(h, a) => Application(replaceTerm(h, t1, t2), replaceTerm(a, t1, t2))
    case LambdaAbstraction(v, t) => LambdaAbstraction(v, replaceTerm(t, t1, t2))
    case CaseExpression(sel, bs) => 
      CaseExpression(replaceTerm(sel, t1, t2), bs map {b => Branch(b.pattern, replaceTerm(b.term, t1, t2))})
  }
  
  def instanceOf(t1: Expression, t2: Expression): Boolean = equivalent(msg(t1, t2).term, t1)
  
  def extractAppArgs(term: Expression): List[Expression] = term match {
    case Application(h, a) => extractAppArgs(h) ::: List(a)
    case _ => Nil
  }
  
  def lineApp(term: Expression): List[Expression] = term match {
    case Application(h, a) => lineApp(h) ::: (a:: Nil)
    case t => t :: Nil
  }
  
  def getAllVars(expr: Expression): Set[Variable] = expr match {
    case v: Variable => Set(v)
    case Constructor(_, args) => (Set[Variable]() /: args) {(vs, term) => vs ++ getAllVars(term)}
    case LambdaAbstraction(x, term) => getAllVars(term) + x
    case Application(head, arg) => getAllVars(head) ++ getAllVars(arg)
    case CaseExpression(sel, bs) => 
      getAllVars(sel) ++ (Set[Variable]() /: bs) {(vs, b) => vs ++ getAllVars(b.term) ++ b.pattern.args}
    case LetExpression(bs, expr) =>
      getAllVars(expr) ++ (Set[Variable]() /: bs) {(vs, b) => vs ++ getAllVars(b._2) + b._1}
    case LetRecExpression(bs, expr) =>
      getAllVars(expr) ++ (Set[Variable]() /: (bs :: Nil)) {(vs, b) => vs ++ getAllVars(b._2) + b._1}
  }
  
}
