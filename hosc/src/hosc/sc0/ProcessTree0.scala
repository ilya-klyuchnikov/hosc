package hosc.sc0

import HLanguage._

object ProcessTree0 {
  def apply(expr: BaseExpression) = 
    new ProcessTree0(new Node(expr, null, Nil))
  
  class Node(var expr: BaseExpression, val in: Edge, var outs: List[Edge]) {
    override def toString = toString("")
    var signature: (String, List[Variable]) = null
    var repeatedOf: Node = null
      
    def toString(indent: String): String = {
      val sb = new StringBuilder(indent + "|__" + expr)
      for (edge <- outs) {
        sb.append("\n  " + indent + "|" + edge.substitution.toList.map(kv => kv._1 + "=" + kv._2).mkString("", ", ", ""))
        sb.append("\n" + edge.child.toString(indent + "  "))
      }
      sb.toString
    }
    
    def children(): List[Node] = outs map {edge => edge.child}

    def ancestors(): List[Node] = if (in == null) Nil else in.parent :: in.parent.ancestors

    def isProcessed: Boolean = (repeatedOf != null || 
      (expr match {
      case Constructor(_, Nil) => true
      case v : Variable if v.global == false => true
      case _ => false
    }))
    
    def getRepParent(): Node = repeatedOf
    
    def getAllVars(): Set[Variable] = {
      var vars = TermAlgebra0.getAllVars(expr)
      for (e <- outs) {
        vars = vars ++ e.child.getAllVars()
      }
      vars
    }
    
    def sub(map: Map[Variable, Variable]): Unit = {
      expr = expr\\map        
      for (e <- outs) {
        e.substitution = Map(e.substitution.toList.map({p => Pair[Variable, Term](p._1\\map, p._2\\map)}):_*)
        e.child.sub(map)
      }
    }
  }
  
  class Edge(val parent: Node, var child: Node, var substitution: Map[Variable, Term]) {
    override def toString = "Edge("+ substitution + ", " + child + ")"
  }
  
}

import ProcessTree0._
class ProcessTree0 {
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
  
  def replace(node: Node, exp: BaseExpression): Node = {
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
    childNode
  }
  
  def isClosed = leafs_.forall(_.isProcessed)
  
  override def toString = rootNode.toString
}


