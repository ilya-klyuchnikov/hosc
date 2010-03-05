package hosc.sc

import hosc.{HE, Instance}
import hosc.ProcessTree.Node
import hosc.HLanguage._
import hosc.TermAlgebra._

object RenamingOrdering extends PartialOrdering[Node] {
  def lteq(n1: Node, n2: Node) = TermAlgebra.equivalent(n1.expr, n2.expr)
} 

object InstanceOrdering extends PartialOrdering[Node] {
  def lteq(n1: Node, n2: Node) = Instance.instanceOf(n1.expr, n2.expr)
}

object EmbeddingOrdering extends PartialOrdering[Node] {
  def lteq(n1: Node, n2: Node) = HE.heByCoupling(n1.expr, n2.expr)
}

object RenamingOrderingWithControl extends PartialOrdering[Node] {
  def lteq(n1: Node, n2: Node) = TermAlgebra.equivalent(n1.expr, n2.expr) && Control.checkControl(n1, n2)
}

object InstanceOrderingWithControl extends PartialOrdering[Node] {
  def lteq(n1: Node, n2: Node) = Instance.instanceOf(n1.expr, n2.expr) && Control.checkControl(n1, n2)
}

object EmbeddingOrderingWithControl extends PartialOrdering[Node] {
  def lteq(n1: Node, n2: Node) = HE.heByCoupling(n1.expr, n2.expr) && Control.checkControl(n1, n2)
}

object Control {
  def isGlobal(n: Node): Boolean = n.expr match {
    case LetExpression(_, _) => false
    case e => decompose(e) match {
      case Context(RedexCaseVar(_, _)) => true
      case _ => false
    }
  }
  
  def checkControl(up: Node, down: Node): Boolean = {
    val globalUp = isGlobal(up)
    val globalDown = isGlobal(down)
    (globalUp == globalDown) && 
      (globalUp || down.ancestors.takeWhile(_ != up).forall(!isGlobal(_)))
  }
}