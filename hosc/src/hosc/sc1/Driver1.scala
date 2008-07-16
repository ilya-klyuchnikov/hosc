package hosc.sc1

import HLanguage1._
import TermAlgebra1._


class Driver1(val varsUtil: Vars1Util) {
  val EM = Map[Variable1, Term1]()
 
  /** 
   *   
   * Drives specified expression in HL1 language.
   *
   * @param expr expression in HL1 language 
   */
  def drive(expr: Expression1): List[Pair[Term1, Map[Variable1, Term1]]] = expr match {
    case LetExpression1(bs, t) => (t, Map[Variable1, Term1]()) :: Nil 
    case t: Term1 => decompose1(t) match {
      case ObservableVar1(_) => Nil
      case ObservableCon1(c) => c.args map {a => (a, EM)}
      case ObservableVarApp1(_, app) => extractAppArgs1(app) map {a => (a, EM)}
      case ObservableLam1(l) => (l.t, EM) :: Nil
      case context: Context1 => context.redex match {
        case RedexLamApp1(lam, app) => 
          (context.replaceHole(lam.t/Map(lam.v -> app.arg)), EM) :: Nil
        case RedexCaseCon1(c, ce) => {
          val b = ce.branches.find(_.pattern.name == c.name).get
          val sub = Map((b.pattern.args zip c.args):_*)
          (context.replaceHole(b.term/sub), EM) :: Nil          
        }
        case RedexCaseVar1(v, CaseExpression1(sel, bs)) =>
          (sel, EM) :: (bs map 
            {b => (freshB(replaceTerm1(context.replaceHole(b.term), v, Constructor1(b.pattern.name, b.pattern.args)), false), 
                Map(v-> Constructor1(b.pattern.name, b.pattern.args)))})
        case RedexCaseVarApp1(a, CaseExpression1(sel, bs)) =>
          (sel, EM) :: (bs map 
            {b => (freshB(replaceTerm1(context.replaceHole(b.term), a, Constructor1(b.pattern.name, b.pattern.args)), false), EM)})
        case RedexLetRec1(letrec) => {
          (context.replaceHole(unfold(letrec)), EM) :: Nil}
        case RedexCall1(f) =>
            throw new IllegalArgumentException(t.toString())
      }
    }    
  }
  
  def unfold(letrec: LetRecExpression1) = 
    inline(letrec.expr / Map(letrec.binding), letrec)
    
  private def inline(term: Term1, letrec: LetRecExpression1): Term1 = {
    val (oldF, body) = letrec.binding
    val letrecApp = letrec.expr
    def traverse(t: Term1): Term1 = t match {
      case v : Variable1 => v
      case Constructor1(name, args) => Constructor1(name, args map traverse)
      case CaseExpression1(sel, bs) => CaseExpression1(traverse(sel), bs map {b => Branch1(b.pattern, traverse(b.term)) })
      case LetRecExpression1((f, fdef), expr) => LetRecExpression1((f, traverse(fdef)), traverse(expr))
      case LambdaAbstraction1(v, expr) => LambdaAbstraction1(v, traverse(expr))
      case app @ Application1(head, arg) => {
        if (getCoreAppVar(app) == oldF) {
          val newF = varsUtil.createFreshLetrecVar()
          val fargs = extractAppArgs(app)
          val newBody = freshB(body, true)/Map(oldF -> newF)
          LetRecExpression1((newF, newBody), freshB(constructApplication1(newF, fargs), true)) 
        } else {
          Application1(traverse(head), traverse(arg))
        }
      }
    }    
    traverse(term)
  }
  
  /** 
   *   
   * Refresh binders in specified HL1 term. Used to eliminate possible name collisions.
   *
   * @param term HL1 term
   * @param freshLetrecs should variable <code>f<code> in construction
   * <code> letrec f = ... in f x1 x2 ...</code> be refreshed 
   */
  private def freshB(term: Term1, freshLetrecs: Boolean): Term1 = term match {
    case v: Variable1 => v
    case Constructor1(name, args) => Constructor1(name, args map (freshB(_, freshLetrecs))) 
    case Application1(h, a) => Application1(freshB(h, freshLetrecs), freshB(a, freshLetrecs))
    case LambdaAbstraction1(v, t) => {
      val fv = varsUtil.createFreshVar()
      LambdaAbstraction1(fv, freshB(t, freshLetrecs)/Map(v -> fv))
    }
    case CaseExpression1(sel, bs) => CaseExpression1(freshB(sel, freshLetrecs), bs map {fresh(_, freshLetrecs)})
    case LetRecExpression1((v, term), expr) => 
      if (freshLetrecs){
        val fv = varsUtil.createFreshLetrecVar()
        LetRecExpression1((fv, freshB(term, freshLetrecs)/Map(v -> fv)), freshB(expr, freshLetrecs)/Map(v -> fv))
      } else {
        LetRecExpression1((v, freshB(term, freshLetrecs)), freshB(expr, freshLetrecs))
      }
  }
  
  private def fresh(b: Branch1, freshLetrecs: Boolean): Branch1 = {
    val args = b.pattern.args
    val fargs = args map {x => varsUtil.createFreshVar()}
    Branch1(Pattern1(b.pattern.name, fargs), freshB(b.term, freshLetrecs)/Map((args zip fargs):_*))
  }
  
}
