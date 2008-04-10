package hosc;

import HLanguage._

object TermAlgebra {
  
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
    def redex: Redex
  }
  
  case class ContextHole(redex: Redex) extends Context
  // app = con e 
  case class ContextApp(head: Context, app: Application) extends Context {
    def redex = head.redex
  }
  // ce = case selector of ....
  case class ContextCase(selector: Context, ce: CaseExpression) extends Context {
    def redex = selector.redex
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
    case a @ Application(h, _) => ContextApp(createContext(h), a)
    case ce @ CaseExpression(s, _) => ContextCase(createContext(s), ce)
    case _ => throw new IllegalArgumentException(t.toString)
  }
  
  private def getCoreLocalVar(app: Application): Variable = app.head match {
    case v: Variable if (!v.global)=> v
    case a: Application => getCoreLocalVar(a)
    case _ => null
  }
  
}
