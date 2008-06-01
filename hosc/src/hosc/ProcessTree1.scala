package hosc

import HLanguage1._
import TermAlgebra1._

/* the copy of ProcessTree adapted for hl1 language */
object ProcessTree1 {
  def apply(expr: Expression1) = 
    new ProcessTree1(new Node1(expr, null, Nil))
  
  class Node1(var expr: Expression1, val in: Edge1, var outs: List[Edge1]) {
    override def toString = toString("")
    var signature: (String, List[Variable1]) = null
      
    def toString(indent: String): String = {
      val sb = new StringBuilder(indent + "|__" + expr)
      for (edge <- outs) {
        sb.append("\n  " + indent + "|" + edge.substitution.toList.map(kv => kv._1 + "=" + kv._2).mkString("", ", ", ""))
        sb.append("\n" + edge.child.toString(indent + "  "))
      }
      sb.toString
    }
    
    def children(): List[Node1] = outs map {edge => edge.child}

    def ancestors(): List[Node1] = if (in == null) Nil else in.parent :: in.parent.ancestors
  }
  
  class Edge1(val parent: Node1, var child: Node1, val substitution: Map[Variable1, Term1]) {
    override def toString = "Edge("+ substitution + ", " + child + ")"
  }
  
}

import ProcessTree1._
class ProcessTree1 {
  var rootNode: Node1 = null
  private var leafs_ = List[Node1]()
  
  def this(root: Node1) {
   this()
   rootNode = root
   leafs_ = List[Node1](rootNode)
  }
  
  def leafs = leafs_
  
  def addChildren(node: Node1, children: List[Pair[Term1, Map[Variable1, Term1]]]) = {
    assume(leafs_.contains(node))
    leafs_ = leafs_.remove(_ == node)
    val edges = new scala.collection.mutable.ListBuffer[Edge1]
    for (pair <- children){
      val edge = new Edge1(node, null, pair._2)
      val childNode = new Node1(pair._1, edge, Nil)
      leafs_ = childNode :: leafs
      edge.child = childNode
      edges += edge
    }
    node.outs = edges.toList
  }
  
  def replace(node: Node1, exp: Expression1) = {
    // the node can be not leaf - but from any part of tree
    leafs_ = leafs_.remove(_ == node)
    leafs_ = leafs_.remove(_.ancestors.contains(node))
    val childNode = new Node1(exp, node.in, Nil)
    // the node can be root node:
    if (node == rootNode){
      rootNode = childNode
    } else {
      node.in.child = childNode
    }
    leafs_ = childNode :: leafs
  }
  
  //def isClosed = leafs_.forall(_.isProcessed)
  
  override def toString = rootNode.toString
}


