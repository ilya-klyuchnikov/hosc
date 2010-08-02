package hosc

import HLanguage._

object ProcessTree {
  def apply(expr: Expression) = 
    new ProcessTree(new Node(expr, null, Nil))
  
  class Node(var expr: Expression, val in: Edge, var outs: List[Edge]) {
    override def toString = toString("")
    var signature: (Variable, List[Variable]) = null
    var repeatedOf: Node = null
    
    def toString(indent: String): String = {
      val sb = new StringBuilder(indent + "|__" + expr)
      for (edge <- outs) {
        sb.append("\n  " + indent + "|")
        sb.append("\n" + edge.child.toString(indent + "  "))
      }
      sb.toString
    }
    
    def children(): List[Node] = outs map {edge => edge.child}

    def ancestors(): List[Node] = if (in == null) Nil else in.parent :: in.parent.ancestors
    
    def allNarrowing(): List[(Expression, Expression)] = {
      if (in == null) Nil
      else {
        in.narrowing match {
          case None => in.parent.allNarrowing()
          case Some(pair) => pair :: in.parent.allNarrowing()
        }
      } 
    }

    def isProcessed: Boolean = (repeatedOf != null || 
      (expr match {
      case Constructor(_, Nil) => true
      case v : Variable if v.global == false => true
      case CaseExpression(Constructor(_, _), Nil) => true
      case _ => false
    }))
    
    def getRepParent(): Node = repeatedOf
    
    def getAllVars(): Set[Variable] = {
      var vars = TermAlgebra.getAllVars(expr)
      for (e <- outs) {
        vars = vars ++ e.child.getAllVars()
      }
      vars
    }
    
    def sub(map: Map[Variable, Variable]): Unit = {
      expr = expr\\map        
      for (e <- outs) {
        e.child.sub(map)
      }
    }
  }
  
  class Edge(val parent: Node, var child: Node, val narrowing: Option[(Expression, Expression)]) {
    def this(p: Node, c: Node) {
      this(p, c, None)
    }
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
  
  def addChildren(node: Node, es: List[Expression]): ProcessTree = {
    assume(leafs_.contains(node))
    leafs_ = leafs_.remove(_ == node)
    val edges = new scala.collection.mutable.ListBuffer[Edge]
    for (e <- es){
      val edge = new Edge(node, null)
      val childNode = new Node(e, edge, Nil)
      leafs_ = childNode :: leafs
      edge.child = childNode
      edges += edge
    }
    node.outs = edges.toList
    this
  }
  
  def addChildren(node: Node, es: List[Expression], narrowings: List[Option[(Expression, Expression)]]): ProcessTree = {
    assume(leafs_.contains(node))
    leafs_ = leafs_.remove(_ == node)
    val edges = new scala.collection.mutable.ListBuffer[Edge]
    for ((e, nar) <- (es zip narrowings)){
      val edge = new Edge(node, null, nar)
      val childNode = new Node(e, edge, Nil)
      leafs_ = childNode :: leafs
      edge.child = childNode
      edges += edge
    }
    node.outs = edges.toList
    this
  }
  
  def replace(node: Node, exp: Expression): ProcessTree = {
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
    this
  }
  
  def isClosed = leafs_.forall(_.isProcessed)
  
  override def toString = rootNode.toString
}


