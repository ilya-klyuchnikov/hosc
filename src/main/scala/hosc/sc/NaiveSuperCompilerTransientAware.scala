package hosc.sc

import hosc.HLanguage._
import hosc.ProcessTree
import hosc.ProcessTree.Node
import hosc.TermAlgebra._
import hosc.Instance
import hosc.EmbeddingNaiveBinary

class NaiveSuperCompilerTransientAware(program: Program) extends NaiveSuperCompiler(program) {

  override def relevant(node: Node) = decompose(node.expr) match {

    // transient nodes
    case Context(RedexLamApp(lam, app)) => false
    case Context(RedexCall(_)) => false
    case Context(RedexCaseCon(_, _)) => false

    case o: Observable => false
    case _ => true
  }

}

object NaiveSuperCompilerTransientAware extends SuperCompilerFacade {
  def createSuperCompiler(program: Program) = new NaiveSuperCompilerTransientAware(program)
}