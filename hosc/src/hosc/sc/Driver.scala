package hosc.sc

import hosc.HLanguage._
import hosc.ProcessTree
import hosc.ProcessTree._
import hosc.TermAlgebra._

// the basic driver
// it doesn't support propagation of missing pattern
// upward the process tree - just replaces 
// con<case C of {...}> => case C of {} 
trait Driver {
  def program: Program
  
  def driveNode(tree: ProcessTree, node: Node): ProcessTree = node.expr match {
    case LetExpression(bs, t) => { 
      val childExprs = t :: (bs map {case (v, e) => e})
      tree.addChildren(node, childExprs)
    }
    case expr => decompose(expr) match {
      case ObservableCon(ctr) if !ctr.args.isEmpty => { 
        val childExprs = ctr.args
        tree.addChildren(node, childExprs)
      }
      case ObservableVarApp(_, app) => { 
        val childExprs = extractAppArgs(app)
        tree.addChildren(node, childExprs)
      }
      case ObservableLam(lam) => {
        val childExprs = List(lam.t)
        tree.addChildren(node, childExprs)
      }
      case context@Context(redex) => redex match {
        case RedexCall(v) => {
          val lam = program.getFunction(v.name).get.body
          val childExprs = List(betaReduce(context.replaceHole(freshBinders(lam))))
          tree.addChildren(node, childExprs)
        }
        case RedexCaseCon(c, ce) => {
          ce.branches.find(_.pattern.name == c.name) match {
            case Some(b) => {
              val sub = Map(b.pattern.args zip c.args:_*)
              val childExprs = List(freshBinders(context.replaceHole(applySubstitution(b.term, sub))))
              tree.addChildren(node, childExprs)
            }
            case None => {
              tree.replace(node, CaseExpression(ce.selector, Nil))
            }
          }
        }
        case RedexLamApp(lam, app) => {
          val childExprs = List(betaReduce(expr))
          tree.addChildren(node, childExprs)
        }
        case RedexCaseVar(_, CaseExpression(sel, bs)) => {
          val childForSel = freshBinders(sel)
          // subtle place for full positive propagation
          val branchTransform = {b: Branch =>
            replaceTerm(context.replaceHole(b.term), sel, Constructor(b.pattern.name, b.pattern.args))
          }
          val childrenForBranches = bs map {b => freshBinders(branchTransform(b))}
          val childExprs = childForSel :: childrenForBranches
          tree.addChildren(node, childExprs)
        }
        case RedexChoice(Choice(e1, e2)) => {
          val childExprs = List(context.replaceHole(freshBinders(e1)),context.replaceHole(freshBinders(e2)))
          tree.addChildren(node, childExprs)
        }
      }
      case _ => throw new IllegalArgumentException("non reducible expression: " + expr)
    }
  }
  
  def betaReduce(expr: Expression): Expression = decompose (expr) match {
    case context@Context(RedexLamApp(lam, app)) =>  
      betaReduce(context.replaceHole(applySubstitution(lam.t, Map(lam.v -> app.arg))))
    case _ => freshBinders(expr)
  }
}
