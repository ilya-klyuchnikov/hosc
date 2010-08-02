package hosc.sc

import hosc.HLanguage._
import hosc.ProcessTree
import hosc.ProcessTree._
import hosc.TermAlgebra._

trait SuperCompilerAlgorithm {
  
  def renaming:  PartialOrdering[Node]
  def embedding: PartialOrdering[Node]
  def instance:  PartialOrdering[Node]
  
  def driveNode(tree: ProcessTree, node: Node): ProcessTree
  def fold(tree: ProcessTree, funNode: Node, recNode:Node): ProcessTree
  def abstractDown(tree: ProcessTree, upper: Node, down: Node): ProcessTree
  def abstractUp(tree: ProcessTree, upper: Node, n2: Node): ProcessTree
  
  def buildProcessTree(e: Expression): ProcessTree = {
    var tree = ProcessTree(e)
    var unprocessedNodeOpt = tree.leafs.find(!_.isProcessed)
    while (unprocessedNodeOpt.isDefined) {
      tree = processNode(tree, unprocessedNodeOpt.get)
      unprocessedNodeOpt = tree.leafs.find(!_.isProcessed)
    }
    tree
  }
  
  def processNode(tree: ProcessTree, beta: Node): ProcessTree =
    if (trivial(beta)) {
      driveNode(tree, beta)
    } else {
      beta.ancestors find {renaming.lteq(_, beta)}  map {fold(tree, _, beta)}
    } orElse {
      beta.ancestors find {instance.lteq(_, beta)}  map {abstractDown(tree, _, beta)}
    } orElse {
      beta.ancestors find {embedding.lteq(_, beta)} map {abstractUp(tree, _, beta)}
    } getOrElse {
      driveNode(tree, beta)
    }
  
  def trivial(node: Node) = node.expr match {
    case LetExpression(_, _) => true
    case e => decompose(e) match {
      case Context(RedexCall(_)) => false
      case Context(RedexCaseVar(_, _)) => false
      case Context(RedexLamApp(lam, app)) => {
        val sizeBefore = size(app)
        val sizeAfter = size(applySubstitution(lam.t, Map(lam.v -> app.arg)))
        sizeBefore > sizeAfter
      }
      case Context(RedexCaseCon(c, ce)) => {
        ce.branches.find(_.pattern.name == c.name) match {
          case Some(b) => {
            val sub = Map(b.pattern.args zip c.args:_*)
            val sizeBefore = size(ce)
            val sizeAfter = size(applySubstitution(b.term, sub))
            sizeBefore > sizeAfter
          }
          case None => true
        }
      }
      case _ => true
    }
  }

}
