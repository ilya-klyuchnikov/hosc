package hosc

import HLanguage._
import LangUtils._
import ProcessTree._
import MSG._
import TermAlgebra._

class CodeConstructor(val originalProgram: Program, val tree: ProcessTree, freeVarsInLetrecs: Boolean) {
  def generateProgram() = Program(originalProgram.ts, construct(tree.rootNode), Nil)
  
  private def construct(node: Node): Expression = node.expr match {
    case LetExpression(bs, t) => {
      val node0 = node.outs.head.child
      val nodes = node.outs.tail map {edge => edge.child}
      val ts = nodes map construct
      val subs = Map[Variable, Expression]() ++ ((bs zip ts) map {pair => (pair._1._1, pair._2)})
      construct(node0)/subs
    }
    case t => decompose(t) match {
      case ObservableVar(v) => Variable(v.name)
      case ObservableCon(c) => Constructor(c.name, node.children map construct)
      case ObservableVarApp(v, app) => constructApplication(Variable(v.name), node.children map construct)
      case ObservableLam(l) => LambdaAbstraction(Variable(l.v.name), construct(node.children.head))
      case context: Context => context.redex match {        
        case RedexLamApp(lam, app) => construct(node.children.head)
        case RedexCaseCon(c, ce) => construct(node.children.head)
        case RedexCaseVar(v, CaseExpression(sel, bs)) => {
          if (node.getRepParent != null) {
            val alphaNode: Node = node.getRepParent()
            val alphaT = alphaNode.expr
            
            val (appHead, args) = alphaNode.signature
            val z = constructApplication(Variable(appHead), args)
            
            val msg = strongMsg(alphaT, t)
            // after substitution:
            val sub = Map[Variable, Expression]() ++ msg.sub2
            val z1 = applySubstitution(z, sub)
            z1
          } else {{
            tree.leafs.filter(n => n.repeatedOf == node) match {
              case Nil => {
                val newBs = (bs zip node.children.tail) map {p => Branch(p._1.pattern, construct(p._2))}
                val newSel = construct(node.children.head)
                CaseExpression(newSel, newBs)
              }
              case repeatNodes => {
                var vars: Set[Variable] = null
                if (freeVarsInLetrecs){
                  vars = Set[Variable]()
                  for (n <- repeatNodes) {
                    val betaT = n.expr
                    val msg = strongMsg(t, betaT)
                    val args0 = msg.sub2 map {p => p._1}
                    vars = vars ++ args0
                  }
                }
                else {
                  vars = TermAlgebra.getFreeVars(t)
                }
                val args0 = vars.toList 
                val args = args0
                val newVars = args map {p => createVar()}
                val sub = Map[Variable, Expression]() ++ ((args0 zip newVars) map {p => (Variable(p._1.name), p._2)})
                node.signature = (createFName(), args0)
                
                val newBs = (bs zip node.children.tail) map {p => Branch(p._1.pattern, construct(p._2))}
                val newSel = construct(node.children.head)
                val ce = CaseExpression(newSel, newBs)/sub
                
                val lam = constructLambda(newVars, ce)
                val appHead = Variable(node.signature._1)
                appHead.global = true
                LetRecExpression((appHead, lam), constructApplication(appHead, args))
              }
            }          
          }}
        }
        case RedexCaseVarApp(a, CaseExpression(sel, bs)) => {
          val newBs = (bs zip node.children.tail) map {p => Branch(p._1.pattern, construct(p._2))}
          val newSel = construct(node.children.head)
          CaseExpression(newSel, newBs)
        }
        case RedexCall(f) => {
          if (node.getRepParent != null) {
            val alphaNode: Node = node.getRepParent()
            val alphaT = alphaNode.expr
            
            val (appHead, args) = alphaNode.signature
            val z = constructApplication(Variable(appHead), args)
            
            val msg = strongMsg(alphaT, t)
            // after substitution:
            val sub = Map[Variable, Expression]() ++ msg.sub2
            val z1 = applySubstitution(z, sub)
            z1
          } else {
            tree.leafs.filter(n => n.repeatedOf == node) match {
              // call to this function does't result in recursive definition
              case Nil => {
                construct(node.children.head);
              }
              // call to this function results in recursive definition
              case repeatNodes => {
                var vars: Set[Variable] = null
                if (freeVarsInLetrecs){
                  vars = Set[Variable]()
                  for (n <- repeatNodes) {
                    val betaT = n.expr
                    val msg = strongMsg(t, betaT)
                    val args0 = msg.sub2 map {p => p._1}
                    vars = vars ++ args0
                  }
                }
                else {
                  vars = TermAlgebra.getFreeVars(t)
                }
                val args0 = vars.toList 
                val args = args0
                val newVars = args map {p => createVar()}
                val sub = Map[Variable, Expression]() ++ ((args0 zip newVars))
                node.signature = (createFName(), args0)
                val expr = construct(node.children.head)/sub
                val lam = constructLambda(newVars, expr)
                val appHead = Variable(node.signature._1)
                appHead.global = true
                LetRecExpression((appHead, lam), constructApplication(appHead, args))
              }
            }            
          }
        }
      }
    }
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
  
  private val vNames = "xyzuvwprst".toArray
  private val fNames = "fgh".toArray
  
  // set of already used variables
  private var fUsed = Set[String]() ++ (getAllVars(originalProgram.goal) map {v => v.name})
  
  private def varFor(j: Int) = {
    if (j < 10) 
      Variable("" + vNames(j))
    else 
      Variable("" + vNames(j % 10) + Integer.toString(j / 10))   
  }
  
  private def fName(j: Int): String = {
    if (j < 3) 
      "" + fNames(j)
    else 
      fNames(j % 3) + Integer.toString(j / 3)   
  }
  
  private def isSynthetic(v: Variable) = v.name startsWith "$";    
  
}