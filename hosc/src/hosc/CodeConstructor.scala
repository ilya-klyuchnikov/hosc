package hosc

import HLanguage._
import ProcessTree._
import MSG._
import TermAlgebra._

class CodeConstructor(val originalProgram: Program, val tree: ProcessTree, freeVarsInLetrecs: Boolean) {
  private val vNames = "xyzuvwprst".toArray
  private val fNames = "fgh".toArray
  private var fUsed = Set[String]() ++ (getAllVars(originalProgram.goal) map {v => v.name})
  
  def generateProgram() = Program(originalProgram.ts, construct(tree.rootNode), Nil)
  
  private def construct(node: Node): Expression = node.expr match {
    case LetExpression(bs, t) => {
      val node0 :: nodes = node.children
      val residualBs = nodes map construct
      val subs = Map[Variable, Expression]() ++ ((bs map (_._1)) zip residualBs)
      construct(node0)/subs
    }
    case t => decompose(t) match {
      case ObservableVar(v) => Variable(v.name)
      case ObservableCon(c) => Constructor(c.name, node.children map construct)
      case ObservableVarApp(v, app) => constructApplication(Variable(v.name), node.children map construct)
      case ObservableLam(l) => LambdaAbstraction(Variable(l.v.name), construct(node.children.head))
      case context: Context =>
        if (node.getRepParent() != null) {
          val fNode = node.getRepParent()
          val (f, args) = fNode.signature
          val app = constructApplication(f, args)
          val msg = strongMsg(fNode.expr, t)
          val sub = Map[Variable, Expression]() ++ msg.sub2
          applySubstitution(app, sub)
        } else {
          context.redex match {        
        	case RedexLamApp(lam, app) => construct(node.children.head)
        	case RedexCaseCon(c, CaseExpression(sel, Nil)) => {
        	  CaseExpression(sel, Nil)
            }
        	case RedexCaseCon(c, ce) => construct(node.children.head)
        	case ntr@NonTrivialRedex(x) => {
        	  lazy val traversed = ntr match {
        	    case RedexCall(_) => 
        	      construct(node.children.head);
        	    case RedexCaseVar(_, CaseExpression(_, bs)) => {
        	      val newBs = (bs zip node.children.tail) map {case (Branch(p, _), n) => Branch(p, construct(n))}
              	  CaseExpression(construct(node.children.head), newBs)
        	    }
        	  }
        	  tree.leafs.filter(_.repeatedOf == node) match {
              	case Nil => traversed
              	case repeatNodes => {
              	  val (f, vars) = createSignature(node, repeatNodes) 
              	  node.signature = (f, vars)
              	  val newVars = vars map {p => createVar()}
              	  val sub = Map[Variable, Expression]() ++ ((vars zip newVars))
              	  val body = traversed/sub
              	  LetRecExpression((f, constructLambda(newVars, body)), constructApplication(f, vars))
              	}
        	  }
        	}
          }
        }
    }
  }
  
  def createSignature(fNode: Node, recNodes: List[Node]) = {
    var vars: List[Variable] = TermAlgebra.getFreeVars(fNode.expr)
    if (freeVarsInLetrecs){
      var changedVars = Set[Variable]()
      for (n <- recNodes) {
        val betaT = n.expr
        val msg = strongMsg(fNode.expr, betaT)
        val args0 = msg.sub2 map {p => p._1}
        changedVars = changedVars ++ args0
      }
      vars = vars filter {changedVars.contains}
    }
    (Variable(createFName()), vars)
  }
  
  var i = 0;
  val treeVars = tree.rootNode.getAllVars()
  def createVar(): Variable = {      
    var nv: Variable = null
    do {
      nv = varFor(i)
      i += 1
    } while (treeVars contains nv)
    nv
  }
  
  var fi = 0;
  def createFName(): String = {      
    var name: String = null
    do {
      name = fName(fi)
      fi += 1
    } while (fUsed contains name)
    fUsed = fUsed + name
    name
  }
  
  private def varFor(j: Int) = {
    if (j < 10) Variable("" + vNames(j))
    else Variable("" + vNames(j % 10) + Integer.toString(j / 10))   
  }
  
  private def fName(j: Int): String = {
    if (j < 3) "" + fNames(j)
    else fNames(j % 3) + Integer.toString(j / 3)   
  }
  
  private def isSynthetic(v: Variable) = v.name startsWith "$";    
  
}