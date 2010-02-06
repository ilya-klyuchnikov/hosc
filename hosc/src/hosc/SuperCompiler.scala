package hosc

import HLanguage._
import HE._
import MSG._
import TermAlgebra._
import ProcessTree._
import LangUtils._

class SuperCompiler(val program: Program) extends ASupercompiler with ProcessTreeRenamer {
  val emptyMap = Map[Variable, Expression]()
  val debug = false
  val useControl = true
  
  def buildProcessTree(e: Expression): ProcessTree = {
    val p = ProcessTree(e)
    if (debug) {
      println(program.toDocString)
    }
    while (!p.isClosed) {
      if (debug) { 
        println(p)
        println("==========")
      }
      val beta = p.leafs.find(!_.isProcessed).get
      val bExpr = beta.expr
      beta.expr match {
        case LetExpression(_, _) => drive(p, beta)
        case bTerm if canBeEnhanced_?(bTerm) => {
          beta.ancestors find equivalenceTest(beta) match {
            case Some(alpha) => beta.repeatedOf = alpha; 
            case None => {
              beta.ancestors find instanceTest(beta) match {
                case Some(alpha) => abstractDown(p, alpha, beta) 
                case None => { 
                  beta.ancestors find heByCouplingTest(beta) match {
                    case Some(alpha) => {
                      if (debug) {
                        println("GENERALIZATION FROM SC0")
                      }
                      abstractUp(p, alpha, beta)
                    }
                    case None => drive(p, beta)
                  }
                }
              }
            }
          }
        }
        case _ => drive(p, beta)
      }      
    }
    renameVars(p)
  }
  
  private def instanceTest(bNode: Node)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => instanceOf(aTerm, bNode.expr) && checkControl(aNode, bNode);
  }
  
  private def equivalenceTest(bNode: Node)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => equivalent(aTerm, bNode.expr) && checkControl(aNode, bNode);
  }
  
  private def heByCouplingTest(bNode: Node)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => heByCoupling(aTerm, bNode.expr) && checkControl(aNode, bNode);
  }
  
  def canBeEnhanced_?(t: Expression) = decompose(t) match {
    case Context(RedexCall(_)) => true
    case Context(RedexCaseVar(_, _)) => true
    case _ => false
  }
  
  private def checkControl(aNode: Node, bNode: Node): Boolean = {
    if (!useControl) {
      return true
    }
    if (isGlobal(aNode) == isGlobal(bNode)) {
      if (isGlobal(bNode)) {
        true
      } else {
        val nodesBetween = bNode.ancestors.takeWhile(_ != aNode)
        nodesBetween.forall(!isGlobal(_))
      }
    } else {
      false
    }
  }
  
  def drive(t: ProcessTree, n: Node): Unit = {
    driveExp(n.expr) match {
      case Some(es) => t.addChildren(n, es)
      case None => processMissingMatch(t, n)
    }
  }
  
  private def processMissingMatch(t: ProcessTree, n: Node): Unit = {
    if (debug) {
    	println("propagating match error...")
    	println(n.expr)
    }
    val ancs = n.ancestors 
    ancs find isGlobal match {
      case Some(globalNode) => {
        // finding corresponding globalNode
        val Context(RedexCaseVar(_, CaseExpression(sel, bs))) = decompose(globalNode.expr)
        // finding 'missing' childNode
        val missingChildNode = globalNode.children.find{(n:: ancs).contains(_)}.get
        // finding branch with 'missing' pattern
        val (missingBranch, _) = bs.zip(globalNode.children.tail).find{case (b, childNode) => childNode == missingChildNode}.get
        val newBs = bs remove {_ == missingBranch}
        val newCaseExp = CaseExpression(sel, newBs)
        t.replace(globalNode, newCaseExp)
      }
      case None => {
        // no proparagition to top is possible 
        // removing all patterns from the current node - it becomes complete one
        val CaseExpression(sel, _) = n.expr
        t.replace(n, CaseExpression(sel, Nil))
      }
    }
  }
  
  private def isGlobal(n: Node): Boolean = n.expr match {
    case LetExpression(_, _) => false
    case e => decompose(e) match {
      case Context(RedexCaseVar(_, _)) => true
      case _ => false
    }
  }
  
  def abstractDown(t: ProcessTree, up: Node, down: Node): Unit = {
    makeAbstraction(t, down, up)
  }
  
  def abstractUp(t: ProcessTree, up: Node, down: Node): Unit = {
    makeAbstraction(t, up, down)
  }
  
  def makeAbstraction(t: ProcessTree, alpha: Node, beta: Node): Unit = {
    val aTerm = alpha.expr
    val bTerm = beta.expr
    val g = msg(aTerm, bTerm)
    if (g.sub1.isEmpty){
      t.replace(alpha, g.term)
    } else {
      if (debug){
        println(format(canonize(aTerm)))
        println(format(canonize(bTerm)))
      }
      var term = g.term
      var subs = g.sub1
      if (debug) {
        println(format((LetExpression(g.sub1, g.term))))
      }
      t.replace(alpha, LetExpression(g.sub1, g.term))
    }    
  }
}