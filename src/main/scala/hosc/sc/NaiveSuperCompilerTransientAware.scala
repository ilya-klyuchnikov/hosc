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

  var steps = 0

  // since this supercompiler may non-terminate we explicitly 
  // limit the number of steps
  override def step(tree: ProcessTree, beta: ProcessTree.Node) = {
    steps += 1
    if (steps > 1000) {
      throw new Exception("1000 steps")
    }
    super.step(tree, beta)
  }

}

object NaiveSuperCompilerTransientAware extends SuperCompilerFacade {
  val name = "NaiveSuperCompilerTransientAware"
  def createSuperCompiler(program: Program) = new NaiveSuperCompilerTransientAware(program)
}