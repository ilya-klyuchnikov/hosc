package hosc.sc

import hosc.HLanguage._
import hosc.ProcessTree
import hosc.ProcessTree.Node
import hosc.TermAlgebra._
import hosc.Instance
import hosc.EmbeddingNaiveBinary

class NaiveSuperCompilerWithControl(program: Program) extends NaiveSuperCompiler(program) {

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

object NaiveSuperCompilerWithControl extends SuperCompilerFacade {
  def createSuperCompiler(program: Program) = new NaiveSuperCompilerWithControl(program)
}