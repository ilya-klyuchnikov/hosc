package hosc.exp

import hosc.HLanguage._
import hosc.SuperCompiler
import hosc.ProcessTree
import hosc.ProcessTree._
import hosc.TermAlgebra._

/** 
 * This supercompiler remembers configuration narrowing 
 * and applies it at every step.
 * 
 * This is useful in higher-order setting
 * (since we can narrow f x1 x2 x3 = C e1 e2 e3 when function f is unknown).
 */
class FullPositiveSuperCompiler(program: Program) extends SuperCompiler(program) {
  renameVars = false
  def fullDriveExp(tree: ProcessTree, n: Node): Boolean = {
  var res = true
  n.expr match {
    case LetExpression(bs, t) => Some(t :: (bs map {_._2}))
    case t => decompose(t) match {
      case ObservableVar(_) => 
      case ObservableCon(c) => tree.addChildren(n, c.args)
      case ObservableVarApp(_, app) => tree.addChildren(n, extractAppArgs(app))
      case ObservableLam(l) => tree.addChildren(n, l.t :: Nil)
      case context@Context(redex) => redex match {
        case RedexCall(v) => {
          val lam = program.getFunction(v.name).get.body
          tree.addChildren(n, freshBinders(context.replaceHole(freshBinders(lam))) :: Nil) 
        }
        case RedexLamApp(lam, app) => 
          tree.addChildren(n, freshBinders(context.replaceHole(applySubstitution(lam.t, Map(lam.v -> app.arg)))) :: Nil)
        case RedexCaseCon(c, ce) => {
          ce.branches.find(_.pattern.name == c.name) match {
            case Some(b) => {
              val sub = Map(b.pattern.args zip c.args:_*)
              tree.addChildren(n, freshBinders(context.replaceHole(applySubstitution(b.term, sub))) :: Nil)
            }
            case None => res = false
          }
        }
        case RedexChoice(Choice(e1, e2)) => 
          tree.addChildren(n, List(context.replaceHole(freshBinders(e1)),context.replaceHole(freshBinders(e2))))
        // the most interesting case!
        case RedexCaseVar(_, CaseExpression(sel, bs)) =>
          val es: List[Expression] = freshBinders(sel) :: 
            (bs map {b => freshBinders(replaceTerm(context.replaceHole(b.term), sel, Constructor(b.pattern.name, b.pattern.args)))})
          val ns: List[Option[(Expression, Expression)]] = 
            None :: (bs map {b => Some(sel, Constructor(b.pattern.name, b.pattern.args))})
          tree.addChildren(n, es, ns)
      }
    }
  }
  res
  }
  
  override def step(p: ProcessTree, beta: Node): Unit = {
    val narrowings = beta.allNarrowing()
    var expr = beta.expr
    var before = expr
    val init = expr
    var b1 = beta
    println("-----")
    do {
      before = expr 
      for ((e1, e2) <- narrowings) {
        println(e1 + " ===> " + e2)
    	expr = replaceTerm(expr, e1, e2)
      }
    } while (before != expr)
    if (init != expr) {
    	println("before:")
    	println(init)
    	println("after:")
    	println(expr)
    	println("-----")
    	b1 = p.replace(beta, expr)
    }
    super.step(p, b1)
  }
  
  override def drive(t: ProcessTree, n: Node): Unit = {
    fullDriveExp(t, n) match {
      case false => processMissingMatch(t, n)
      case _ =>
    }
  }
}
