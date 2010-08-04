package hosc.sc

import hosc.HLanguage._
import hosc.ProcessTree
import hosc.ProcessTree.Node
import hosc.TermAlgebra._
import hosc.Instance
import hosc.EmbeddingNaiveBinary

class NaiveSuperCompilerWithControlAndCoupling(program: Program) extends NaiveSuperCompiler(program) {

  override def findEmbedding(relevantAncs: List[Node], beta: ProcessTree.Node) =
    if (relevant(beta)) {
      relevantAncs find { alpha => EmbeddingNaiveBinary.heByCoupling(alpha.expr, beta.expr) }
    } else {
      None
    }

  override def relevantAncestors(beta: Node): List[Node] =
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

}

object NaiveSuperCompilerWithControlAndCoupling extends SuperCompilerFacade {
  def createSuperCompiler(program: Program) = new NaiveSuperCompilerWithControlAndCoupling(program)
}