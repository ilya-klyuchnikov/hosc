package hosc

import HLanguage._
import HE._
import MSG._
import TermAlgebra._
import ProcessTree._
import LangUtils._

class SuperCompiler(val program: Program) extends ASupercompiler{
  val emptyMap = Map[Variable, Expression]()
  val debug = false  
  
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
          beta.ancestors find equivalenceTest(bTerm) match {
            case Some(alpha) => beta.repeatedOf = alpha; 
            case None => {
              beta.ancestors find instanceTest(bTerm) match {
                case Some(alpha1) => makeAbstraction(p, beta, alpha1) 
                case None => { 
                  beta.ancestors find heByCouplingTest(bTerm) match {
                    case Some(alpha) => {
                      if (debug) {
                        println("GENERALIZATION FROM SC0")
                      }
                      makeAbstraction(p, alpha, beta)
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
  
  private def instanceTest(bTerm: Expression)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => instanceOf(aTerm, bTerm);
  }
  
  private def equivalenceTest(bTerm: Expression)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => equivalent(aTerm, bTerm);
  }
  
  private def heByCouplingTest(bTerm: Expression)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => heByCoupling(aTerm, bTerm);
  }
  
  def canBeEnhanced_?(t: Expression) = decompose(t) match {
    case Context(RedexCall(_)) => true
    case Context(RedexCaseVar(_, _)) => true
    case _ => false
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
  
  def renameVars(p: ProcessTree): ProcessTree = {
    val vars = p.rootNode.getAllVars()
    var i = 0
    def createVar(): Variable = {      
      var nv: Variable = null
      do {
        nv = varFor(i)
        i += 1
      } while (vars contains nv)
      nv
    }
    var map = Map[Variable, Variable]()
    for (v <- vars.toList) {
      if (isSynthetic(v)) {
        map = map + (v -> createVar)
      }
    }
    p.rootNode sub map
    p
  }
  
  private val vNames = "xyzuvwprst".toArray
  
  private def varFor(j: Int) = {
    if (j <= 9) 
      Variable("" + vNames(j))
    else 
      Variable("" + vNames(j % 10) + Integer.toString(j / 10))   
  }
  
  private def isSynthetic(v: Variable) = v.name startsWith "$" 
  
}