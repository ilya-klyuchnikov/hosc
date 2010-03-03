package hosc

import HLanguage._
import HE._
import MSG._
import TermAlgebra._
import ProcessTree._
import LangUtils._

class SuperCompiler0(val program: Program) extends ASupercompiler with ProcessTreeRenamer {
  val emptyMap = Map[Variable, Expression]()
  var debug = false
  var useControl = true
  var renameVars = true
  
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
      step(p, beta)
    }
    if (renameVars) {
      renameVars(p)
    } else {
      p
    }
  }
  
  def step(p: ProcessTree, beta: Node): Unit = {
      val bExpr = beta.expr
      beta.expr match {
        case LetExpression(_, _) => drive(p, beta)
        case bTerm if canBeEnhanced_?(bTerm) => {
          beta.ancestors find equivalenceTest(beta) match {
            case Some(alpha) => beta.repeatedOf = alpha
            case None => {
              beta.ancestors find instanceTest(beta) match {
                case Some(alpha) => abstractDown(p, alpha, beta) 
                case None => { 
                  beta.ancestors find heByCouplingTest(beta) match {
                    case Some(alpha) => abstractUp(p, alpha, beta)
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
  
  private def instanceTest(bNode: Node)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => Instance.instanceOf(aTerm, bNode.expr) && checkControl(aNode, bNode)
  }
  
  private def equivalenceTest(bNode: Node)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => equivalent(aTerm, bNode.expr) && checkControl(aNode, bNode)
  }
  
  protected def heByCouplingTest(bNode: Node)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => HE.heByCoupling(aTerm, bNode.expr) && checkControl(aNode, bNode)
  }
  
  def canBeEnhanced_?(t: Expression) = decompose(t) match {
    case Context(RedexCall(_)) => true
    case Context(RedexCaseVar(_, _)) => true
    case _ => false
  }
  
  protected def checkControl(aNode: Node, bNode: Node): Boolean = {
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
  
  def processMissingMatch(t: ProcessTree, n: Node): Unit = {
    if (debug) {
    	println("propagating match error...")
    	println(n.expr)
    }
    val Context(RedexCaseCon(_, CaseExpression(sel, _))) = decompose(n.expr)
    val newExp = CaseExpression(sel, Nil)
    t.replace(n, newExp) 
    return
    val ancs = n.ancestors 
    
    (ancs takeWhile reducable) find isGlobal match {
      case Some(globalNode) => {
        // finding corresponding globalNode
        val con@Context(RedexCaseVar(_, CaseExpression(sel, bs))) = decompose(globalNode.expr)
        // finding 'missing' childNode
        val missingChildNode = globalNode.children.find{(n:: ancs).contains(_)}.get
        // finding branch with 'missing' pattern
        val (missingBranch, _) = bs.zip(globalNode.children.tail).find{case (b, childNode) => childNode == missingChildNode}.get
        val newBs = bs remove {_ == missingBranch}
        val newCaseExp = CaseExpression(sel, newBs)
        val newExpr = con.replaceHole(newCaseExp)
        //println("was:")
        //println(globalNode.expr)
        //println("now:")
        //println(newExpr)
        t.replace(globalNode, newExpr)
      }
      case None => {
        // no proparagition to top is possible 
        // removing all patterns from the current node - it becomes complete one
        val CaseExpression(sel, _) = n.expr
        t.replace(n, CaseExpression(sel, Nil))
      }
    }
  }
  
  def reducable(n: Node) = n.expr match {
    case le: LetExpression => false
    case c: Constructor => false
    case a: Application if getCoreLocalVar(a) != null => false
    case lambda: LambdaAbstraction => false
    case _ => true
  }
  
  def isGlobal(n: Node): Boolean = n.expr match {
    case LetExpression(_, _) => false
    case e => decompose(e) match {
      case Context(RedexCaseVar(_, _)) => true
      case _ => false
    }
  }
  
  def abstractDown(t: ProcessTree, up: Node, down: Node): Unit = {
    val a = up.expr
    val b = down.expr
    val sub = Instance.findSubst(a, b)
    val keyMap = Map(sub.keys.toList map {k => (k, TermAlgebra.newVar())}:_*)
    val eg = a/keyMap
    val sub1 = (sub map {case (k, v) => (keyMap(k), v)}).toList
    t.replace(down, LetExpression(sub1, eg))
  }
  
  def abstractUp(t: ProcessTree, up: Node, down: Node): Unit = {
    val upTerm = up.expr
    val downTerm = down.expr
    val g = msg(upTerm, downTerm)
    if (g.sub1.isEmpty){
      t.replace(up, g.term)
    } else {
      if (debug){
        println(format(canonize(upTerm)))
        println(format(canonize(downTerm)))
      }
      var term = g.term
      var subs = g.sub1
      if (debug) {
        println(format((LetExpression(g.sub1, g.term))))
      }
      t.replace(up, LetExpression(g.sub1, g.term))
    }
  }
}