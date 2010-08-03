package hosc.sc

import hosc.HLanguage._
import hosc.ProcessTree
import hosc.ProcessTree._
import hosc.TermAlgebra._
import hosc.{ Instance, MSG }
import hosc.MSG.Generalization

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

  def fold(tree: ProcessTree, funNode: Node, repNode: Node): ProcessTree = {
    repNode.repeatedOf = funNode
    tree
  }

  // TODO: switch to msg?
  def prepareForFold(tree: ProcessTree, node: Node, subNode: Node): ProcessTree = {
    val configuration = node.expr
    val subConfiguration = subNode.expr
    val sub = Instance.findSubst(configuration, subConfiguration)
    val keyMap = Map(sub.keys.toList map { k => (k, newVar()) }: _*)
    val eg = configuration / keyMap
    val sub2 = (sub map { case (k, v) => (keyMap(k), v) }).toList
    tree.replace(subNode, LetExpression(sub2, eg))
  }
  
  def abstractUp(tree: ProcessTree, up: Node, down: Node): ProcessTree = {
	 MSG.msgExt(up.expr, down.expr) match {
      case Generalization(Variable(_), _, _) => throw new IllegalArgumentException("Unexpected exprs: " + up.expr + " " + down.expr) 
      case Generalization(genExpr, sub1, _) => tree.replace(up, LetExpression(sub1, genExpr))
    } 
  }

  def abstractUpOrBinarySplitDown(tree: ProcessTree, nodeToGeneralize: Node, companionNode: Node): ProcessTree = {
    MSG.msgExt(nodeToGeneralize.expr, companionNode.expr) match {
      case Generalization(Variable(_), _, _) => null
      case Generalization(genExpr, sub1, _) => tree.replace(nodeToGeneralize, LetExpression(sub1, genExpr))
    }
  }

  def splitNaive(tree: ProcessTree, node: Node): ProcessTree = node.expr match {

    case Application(e1, e2) => {
      val (v1, v2) = (newVar, newVar)
      tree.replace(node, LetExpression(List((v1, e1), (v2, e2)), Application(v1, v2)))
    }
    
    case CaseExpression(Variable(_), bs) => {
    	throw new IllegalArgumentException("not splittable expression: " + node.expr)
    }
    
    case CaseExpression(sel, bs) => {
    	val v = newVar
    	tree.replace(node, LetExpression(List((v, sel)), CaseExpression(v, bs)))
    }

    case expr => {
      throw new IllegalArgumentException("not splittable expression: " + expr)
    }
  }
}