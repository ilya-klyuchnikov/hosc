package hosc

import HLanguage._
import TermAlgebra._

object ProcessTree {
  def apply(expr: BaseExpression) = 
    new ProcessTree(new Node(expr, null, Nil))
  
  class Node(val expr: BaseExpression, val in: Edge, var outs: List[Edge]) {
    override def toString = toString("")
      
    def toString(indent: String): String = {
      val sb = new StringBuilder(indent + "|__" + expr)
      for (edge <- outs) {
        sb.append("\n  " + indent + "|" + edge.substitution.toList.map(kv => kv._1 + "=" + kv._2).mkString("", ", ", ""))
        sb.append("\n" + edge.child.toString(indent + "  "))
      }
      sb.toString
    }    

    def ancestors(): List[Node] = if (in == null) Nil else in.parent :: in.parent.ancestors

    def isProcessed: Boolean = expr match {
      case Constructor(_, Nil) => true
      case v : Variable => true
      case t: Term => {
        decompose(t) match {
          case o: Observable => false
          case c: Context => c.redex match {
            case r: RedexCall => {
              var edge = in
              while (edge != null) {
                val node1 = edge.parent
                node1.expr match {
                  case pt: Term => if (instanceOf(pt, t)) return true
                  case _ => 
                }
                edge = node1.in
              }
              false
            }
            case _ => false
          }
        }
      }
      case _ => false
    }
  }
  
  class Edge(val parent: Node, var child: Node, val substitution: Map[Variable, Term]) {
    override def toString = "Edge("+ substitution + ", " + child + ")"
  }
}

import ProcessTree._
class ProcessTree {
  var rootNode: Node = null
  private var leafs_ = List[Node]()
  
  def this(root: Node) {
   this()
   rootNode = root
   leafs_ = List[Node](rootNode)
  }
  
  def leafs = leafs_
  
  def addChildren(node: Node, children: List[Pair[Term, Map[Variable, Term]]]) = {
    assume(leafs_.contains(node))
    leafs_ = leafs_.remove(_ == node)
    val edges = new scala.collection.mutable.ListBuffer[Edge]
    for (pair <- children){
      val edge = new Edge(node, null, pair._2)
      val childNode = new Node(pair._1, edge, Nil)
      leafs_ = childNode :: leafs
      edge.child = childNode
      edges += edge
    }
    node.outs = edges.toList
  }
  
  def replace(node: Node, exp: BaseExpression) = {
    // the node can be not leaf - but from any part of tree
    leafs_ = leafs_.remove(_ == node)
    leafs_ = leafs_.remove(_.ancestors.contains(node))
    val childNode = new Node(exp, node.in, Nil)
    // the node can be root node:
    if (node == rootNode){
      rootNode = childNode
    } else {
      node.in.child = childNode
    }
    leafs_ = childNode :: leafs
  }
  
  def isClosed = leafs_.forall(_.isProcessed)
  
  override def toString = rootNode.toString
}


