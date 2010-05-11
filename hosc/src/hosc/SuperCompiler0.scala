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
  var info = false
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
    val res = 
    if (renameVars) {
      renameVars(p)
    } else {
      p
    }
    if (info) {
      println(res)
    }
    res
  }
  
  def step(p: ProcessTree, beta: Node): Unit = {
      val bExpr = beta.expr
      beta.expr match {
        case LetExpression(_, _) => drive(p, beta)
        case bTerm => {
          beta.ancestors find equivalenceTest(beta) match {
            case Some(alpha) => { 
              beta.repeatedOf = alpha
              if (info) {
              		println("==========RENAMING")
                    println(format(canonize(alpha.expr)))
                    println(format(canonize(beta.expr)))
                    println("==========")
              }
            }
            case None => {
              beta.ancestors find instanceTest(beta) match {
                case Some(alpha) => {
                  if (info) {
                    println(p)
                    println("==========INSTANCE")
                    println(format(canonize(alpha.expr)))
                    println(format(canonize(beta.expr)))
                    println("==========")
                  }
                  abstractDown(p, alpha, beta)
                  
                }
                case None => { 
                  beta.ancestors find heByCouplingTest(beta) match {
                    case Some(alpha) => {
                      if (info) {
                        println(p)
                        println("==========HE")
                        println(format(canonize(alpha.expr)))
                        println(format(canonize(beta.expr)))
                        println("==========")
                      }
                      abstractUp(p, alpha, beta)
                      //abstractUp(p, beta, alpha)
                    }
                    case None => drive(p, beta)
                  }
                }
              }
            }
          }
        }
      }    
  }
  
  private def instanceTest(bNode: Node)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => eligibleForInstance(bNode.expr) && Instance.instanceOf(aTerm, bNode.expr) && checkControl(aNode, bNode)
  }
  
  private def equivalenceTest(bNode: Node)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => eligibleForFold(bNode.expr) && equivalent(aTerm, bNode.expr) && checkControl(aNode, bNode)
  }
  
  protected def heByCouplingTest(bNode: Node)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => eligibleForWhistle(bNode.expr) && HE.heByCoupling(aTerm, bNode.expr) && checkControl(aNode, bNode)
  }
  
  // non-trivial expression
  def canBeEnhanced_?(t: Expression) = decompose(t) match {
    case Context(RedexCall(_)) => true
    case Context(RedexCaseVar(_, _)) => true
    //case ObservableVarApp(_, _) => true
    //case Context(RedexNestedCase(_, _)) => true
    
    case Context(RedexLamApp(lam, app)) => {
      val sizeBefore = TermAlgebra.size(app)
      val sizeAfter = TermAlgebra.size(TermAlgebra.applySubstitution(lam.t, Map(lam.v -> app.arg)))
      sizeBefore <= sizeAfter
    }
    
    /*
    case Context(RedexCaseCon(c, ce)) => {
      ce.branches.find(_.pattern.name == c.name) match {
        case Some(b) => {
          val sub = Map(b.pattern.args zip c.args:_*)
          val sizeBefore = TermAlgebra.size(ce)
          val sizeAfter = TermAlgebra.size(applySubstitution(b.term, sub))
          sizeBefore <= sizeAfter
        }
        case None => true
      }
    }
     */
    case _ => false
  }
  
  def eligibleForFold(t: Expression) = decompose(t) match {
    case Context(RedexCall(_)) => true
    case Context(RedexCaseVar(_, _)) => true 
    case Context(RedexLamApp(lam, app)) => true
    case _ => false
  }
  
  def eligibleForInstance(t: Expression) = decompose(t) match {
    case Context(RedexCall(_)) => true
    case Context(RedexCaseVar(_, _)) => true
    case Context(RedexLamApp(lam, app)) => true
    case _ => false
  }
  
  def eligibleForWhistle(t: Expression) = decompose(t) match {
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
      if (false){
        println(format(canonize(upTerm)))
        println("----------")
        println(format(canonize(downTerm)))
        println("----------")
      }
      var term = g.term
      var subs = g.sub1
      if (info) {
        println(format((LetExpression(g.sub1, g.term))))
        println("==========")
      }
      t.replace(up, LetExpression(g.sub1, g.term))
    }
  }
}