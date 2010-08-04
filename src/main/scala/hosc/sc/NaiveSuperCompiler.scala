package hosc.sc

import hosc.HLanguage._
import hosc.ProcessTree
import hosc.ProcessTree.Node
import hosc.TermAlgebra._
import hosc.Instance
import hosc.EmbeddingNaiveBinary

/**
 * This is an adaptation for a higher-order setting of the Algorithm 4.10 from:
 * 
 * Sørensen, Glück. An Algorithm of Generalization in Positive Supercompilation. 
 *
 */
class NaiveSuperCompiler(val program: Program) extends SuperCompiler {

  def relevant(node: Node) = decompose(node.expr) match {
    case Context(RedexLamApp(lam, app)) => false
    case _ => true
  }

  def findRenaming(relevantAncs: List[Node], beta: ProcessTree.Node) =
    findEmbedding(relevantAncs, beta) filter { alpha => equivalent(alpha.expr, beta.expr) }

  def findInstance(relevantAncs: List[Node], beta: ProcessTree.Node) =
    findEmbedding(relevantAncs, beta) filter { alpha => Instance.instanceOf(alpha.expr, beta.expr) }

  def findEmbedding(relevantAncs: List[Node], beta: ProcessTree.Node) =
    if (relevant(beta)) {
      relevantAncs find { alpha => EmbeddingNaiveBinary.he(alpha.expr, beta.expr) }
    } else {
      None
    }

  def processRenaming(tree: ProcessTree, up: Node, down: Node) =
    fold(tree, up, down)

  def processInstance(tree: ProcessTree, up: Node, down: Node) =
    prepareForFold(tree, up, down)

  def processEmbedding(tree: ProcessTree, up: Node, down: Node) =
    abstractUpOrBinarySplitDown(tree, up, down)

  def processOther(tree: ProcessTree, node: Node): ProcessTree =
    driveNode(tree, node)

  def relevantAncestors(beta: Node): List[Node] =
    beta.ancestors filter { !isLetNode(_) } filter { relevant }

  def isLetNode(n: Node): Boolean = n.expr match {
    case LetExpression(_, _) => true
    case _ => false
  }
}

object NaiveSuperCompiler extends SuperCompilerFacade {
  val name = "NaiveSuperCompiler"
  def createSuperCompiler(program: Program) = new NaiveSuperCompiler(program)
}
