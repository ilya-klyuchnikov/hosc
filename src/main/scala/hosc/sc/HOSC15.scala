package hosc.sc

import hosc.HLanguage._
import hosc.ProcessTree
import hosc.ProcessTree.Node
import hosc.TermAlgebra._
import hosc.Instance
import hosc.HE

/**
 * The main goal of the Supercompiler HOSC 1.5 is program analysis.
 *
 */
class HOSC15(val program: Program) extends SuperCompiler {

  def eligibleForRenaming(node: Node) = decompose(node.expr) match {
    case Context(RedexCall(_)) => true
    case Context(RedexCaseVar(_, _)) => true
    case Context(RedexLamApp(lam, app)) => true
    case Context(RedexCaseCon(_, _)) => true
    case _ => false
  }

  def eligibleForInstance(node: Node) = decompose(node.expr) match {
    case Context(RedexCall(_)) => true
    case Context(RedexCaseVar(_, _)) => true
    case Context(RedexLamApp(lam, app)) => true
    case Context(RedexCaseCon(_, _)) => true
    case _ => false
  }

  def eligibleForEmbedding(node: Node) = decompose(node.expr) match {
    case Context(RedexCall(_)) => true
    case Context(RedexCaseVar(_, _)) => true
    case Context(RedexCaseCon(_, _)) => true
    case _ => false
  }

  def findRenaming(relevantAncs: List[Node], beta: ProcessTree.Node) =
    if (eligibleForRenaming(beta)) {
      relevantAncs find { alpha => equivalent(alpha.expr, beta.expr) }
    } else {
      None
    }

  def findInstance(relevantAncs: List[Node], beta: ProcessTree.Node) =
    if (eligibleForInstance(beta)) {
      relevantAncs find { alpha => Instance.instanceOf(alpha.expr, beta.expr) }
    } else {
      None
    }

  def findEmbedding(relevantAncs: List[Node], beta: ProcessTree.Node) =
    if (eligibleForEmbedding(beta)) {
      relevantAncs find { alpha => HE.heByCoupling(alpha.expr, beta.expr) && sameRedex(alpha.expr, beta.expr) }
    } else {
      None
    }

  def processRenaming(tree: ProcessTree, up: Node, down: Node) =
    fold(tree, up, down)

  def processInstance(tree: ProcessTree, up: Node, down: Node) =
    prepareForFold(tree, up, down)

  def processEmbedding(tree: ProcessTree, up: Node, down: Node) =
    abstractUp(tree, up, down)

  def processOther(tree: ProcessTree, node: Node): ProcessTree =
    driveNode(tree, node)

  def ancestors(beta: Node): List[Node] =
    beta.ancestors filter { !isLetNode(_) }

  def relevantAncestors(beta: Node): List[Node] =
    if (isGlobal(beta)) {
      beta.ancestors.filter(isGlobal)
    } else {
      beta.ancestors takeWhile { !isGlobal(_) } filter { !isLetNode(_) }
    }

  def isGlobal(n: Node): Boolean = n.expr match {
    case LetExpression(_, _) => false
    case e => decompose(e) match {
      case Context(RedexCaseVar(_, _)) => true
      case _ => false
    }
  }

  def isLetNode(n: Node): Boolean = n.expr match {
    case LetExpression(_, _) => true
    case _ => false
  }

  def sameRedex(t1: Expression, t2: Expression): Boolean = (decompose(t1), decompose(t2)) match {
    case (Context(redex1), Context(redex2)) => redex1.getClass == redex2.getClass
    case _ => false
  }
}

object HOSC15 extends SuperCompilerFacade {
  def createSuperCompiler(program: Program) = new HOSC15(program)
}
