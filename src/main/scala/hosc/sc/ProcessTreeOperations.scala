package hosc.sc

import hosc.HLanguage._
import hosc.ProcessTree
import hosc.ProcessTree._
import hosc.TermAlgebra._

trait ProcessTreeOperations {
  def program: Program

  def driveNode(tree: ProcessTree, node: Node): ProcessTree = node.expr match {

    case LetExpression(bs, t) => {
      val childExprs = t :: (bs map { case (v, e) => e })
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

      case context@Context(RedexCall(v)) => {
        val lam = program.getFunction(v.name).get.body
        val childExprs = List(context.replaceHole(freshBinders(lam)))
        tree.addChildren(node, childExprs)
      }

      case context@Context(RedexCaseCon(c, ce)) => {
        val branch = ce.branches.find(_.pattern.name == c.name).get
        val sub = Map(branch.pattern.args zip c.args: _*)
        val childExprs = List(freshBinders(context.replaceHole(applySubstitution(branch.term, sub))))
        tree.addChildren(node, childExprs)
      }

      case context@Context(RedexLamApp(lam, app)) => {
        val child = freshBinders(context.replaceHole(applySubstitution(lam.t, Map(lam.v -> app.arg))))
        tree.addChildren(node, List(child))
      }

      // TODO: make full information propagation here:
      // 1) make "promised" propagation
      case context@Context(RedexCaseVar(_, CaseExpression(sel, bs))) => {
        val childForSel = freshBinders(sel)
        val branchTransform = { b: Branch =>
          replaceTerm(context.replaceHole(b.term), sel, Constructor(b.pattern.name, b.pattern.args))
        }
        val childrenForBranches = bs map { b => freshBinders(branchTransform(b)) }
        val childExprs = childForSel :: childrenForBranches
        tree.addChildren(node, childExprs)
      }

      case _ => throw new IllegalArgumentException("non reducible expression: " + expr)

    }
  }

}