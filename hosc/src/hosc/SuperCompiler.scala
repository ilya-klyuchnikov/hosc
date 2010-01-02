package hosc

import HLanguage._
import HE._
import MSG._
import TermAlgebra._
import ProcessTree._
import LangUtils._

class SuperCompiler(program: Program){
  val emptyMap = Map[Variable, Expression]()
  val debug = false
  
  // None means case C of with missing pattern for C
  def driveExp(expr: Expression): Option[List[Expression]] = expr match {
    case LetExpression(bs, t) => Some(t :: (bs map {_._2}))
    case t => decompose(t) match {
      case ObservableVar(_) => Some(Nil)
      case ObservableCon(c) => Some(c.args)
      case ObservableVarApp(_, app) => Some(extractAppArgs(app))
      case ObservableLam(l) => Some(l.t :: Nil)
      case context: Context => context.redex match {
        case RedexCall(v) => {
          val lam = program.getFunction(v.name).get.lam
          Some(freshBinders(context.replaceHole(freshBinders(lam))) :: Nil) 
        }
        case RedexLamApp(lam, app) => Some(freshBinders(context.replaceHole(applySubstitution(lam.t, Map(lam.v -> app.arg)))) :: Nil)
        case RedexCaseCon(c, ce) => {
          ce.branches.find(_.pattern.name == c.name) match {
            case Some(b) => {
              val sub = Map[Variable, Expression]() ++ (b.pattern.args zip c.args)
              Some(freshBinders(context.replaceHole(applySubstitution(b.term, sub))) :: Nil)
            }
            case None => None
          }
        }
        case RedexCaseVar(_, CaseExpression(sel, bs)) =>
          Some(freshBinders(sel) :: 
            (bs map {b => freshBinders(replaceTerm(context.replaceHole(b.term), sel, Constructor(b.pattern.name, b.pattern.args)))}))
      }
    }
  }  
  
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
    case aTerm => sameRedex(aTerm, bTerm) && instanceOf(aTerm, bTerm);
  }
  
  private def equivalenceTest(bTerm: Expression)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => equivalent(aTerm, bTerm);
  }
  
  private def heByCouplingTest(bTerm: Expression)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm => sameRedex(aTerm, bTerm) && heByCoupling(aTerm, bTerm);
  }
  
  def canBeEnhanced_?(t: Expression) = decompose(t) match {
    case c@ContextHole(_) => c.redex match { 
      case r: RedexCall => true
      case r: RedexCaseVar => true
      case _ => false
    }
    case c@ContextApp(_, _) => c.redex match { 
      case r: RedexCall => true
      case r: RedexCaseVar => true
      case _ => false
    }
    case c@ContextCase(_, _) => c.redex match { 
      case r: RedexCall => true
      case r: RedexCaseVar => true
      case _ => false
    }
    case _ => false
  }
  
  def canBeEnhanced1_?(t: Expression) = decompose(t) match {
    case c@ContextCase(_, _) => c.redex match { 
      case r: RedexCall => true
      case r: RedexCaseVar => true
      case _ => false
    }
    case _ => false
  }
  
  def sameRedex(t1: Expression, t2: Expression) : Boolean = (decompose(t1), decompose(t2)) match {
    case (c1: Context, c2: Context) => true//c1.redex.getClass() == c2.redex.getClass()
    case _ => false
  }
  
  def drive(t: ProcessTree, n: Node): Unit = {
    if (debug) {
    	println("driving...")
    	println(n.expr)
    }
    driveExp(n.expr) match {
      case Some(es) => t.addChildren(n, es)
      case None => propagateMatchError(t, n)
    }
  }
  
  private def propagateMatchError(t: ProcessTree, n: Node): Unit = {
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
        println("missing node:")
        println(globalNode)
        val missingChildNode = globalNode.children.find{(n:: ancs).contains(_)}.get
        // finding branch with 'missing' pattern
        val (missingBranch, _) = bs.zip(globalNode.children.tail).find{case (b, childNode) => childNode == missingChildNode}.get
        val newBs = bs remove {_ == missingBranch}
        val newCaseExp = CaseExpression(sel, newBs)
        t.replace(globalNode, newCaseExp)
      }
      // TODO: we need more elegant missing case propagation
      // for now it is good to have just exception
      case None => throw new Exception("No patterns will be matched at all..")
    }
  }
  
  private def isGlobal(n: Node): Boolean = n.expr match {
    case LetExpression(_, _) => false
    case e => decompose(e) match {
      case c: Context => c.redex match {
        case RedexCaseVar(_, _) => true
        case _ => false
      }
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