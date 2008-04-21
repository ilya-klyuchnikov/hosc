package hosc

import HLanguage._
import TermAlgebra._

object ProcessTree {
  def apply(expr: BaseExpression) = 
    new ProcessTree(new Node(expr, null, Nil))
  
  class Node(var expr: BaseExpression, val in: Edge, var outs: List[Edge]) {
    override def toString = toString("")
    var newFName: String = null
      
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
    
    def getRepParent(): Node = expr match {
      case Constructor(_, Nil) => null
      case v : Variable => null
      case t: Term => {
        decompose(t) match {
          case o: Observable => null
          case c: Context => c.redex match {
            case r: RedexCall => {
              var edge = in
              while (edge != null) {
                val node1 = edge.parent
                node1.expr match {
                  case pt: Term => if (instanceOf(pt, t)) return node1
                  case _ => 
                }
                edge = node1.in
              }
              null
            }
            case _ => null
          }
        }
      }
      case _ => null
    }
    
    def getAllVars(): Set[Variable] = {
      var vars = TermAlgebra.getAllVars(expr)
      for (e <- outs) {
        vars = vars ++ e.child.getAllVars()
      }
      vars
    }
    
    def sub(map: Map[Variable, Term]): Unit = {
      expr match {    
        case t: Term => expr = applySubstitution(t, map)
        case le: LetExpression => expr = applySubForLet(le, map) 
      }
      for (e <- outs) {
        e.child.sub(map)
      }
    }
  }
  
  class Edge(val parent: Node, var child: Node, val substitution: Map[Variable, Term]) {
    override def toString = "Edge("+ substitution + ", " + child + ")"
  }
  
  def applySubForLet(le: LetExpression, s: Map[Variable, Term]): LetExpression = {
    LetExpression(le.bs map {b => (applySubstitution(b._1, s).asInstanceOf[Variable], 
                                   applySubstitution(b._2.asInstanceOf[Term], s))}, 
        applySubstitution(le.expr.asInstanceOf[Term], s));
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


