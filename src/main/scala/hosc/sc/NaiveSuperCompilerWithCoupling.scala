package hosc.sc

import hosc.HLanguage._
import hosc.ProcessTree
import hosc.ProcessTree.Node
import hosc.TermAlgebra._
import hosc.Instance
import hosc.EmbeddingNaiveBinary

class NaiveSuperCompilerWithCoupling(program: Program) extends NaiveSuperCompiler(program) {

  override def findEmbedding(relevantAncs: List[Node], beta: ProcessTree.Node) =
    if (relevant(beta)) {
      relevantAncs find { alpha => EmbeddingNaiveBinary.heByCoupling(alpha.expr, beta.expr) }
    } else {
      None
    }

}

object NaiveSuperCompilerWithCoupling extends SuperCompilerFacade {
  val name = "NaiveSuperCompilerWithCoupling"
  def createSuperCompiler(program: Program) = new NaiveSuperCompilerWithCoupling(program)
}