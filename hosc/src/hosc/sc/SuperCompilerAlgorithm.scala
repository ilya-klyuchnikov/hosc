package hosc.sc

import hosc.HLanguage._
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
      println(tree)
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
      case _ => true
    }
  }

}
