package hosc.sc0

import HLanguage._
import HLanguage1._
import HLUtils._
import ProcessTree0._
import TermAlgebra0._
import sc1.TermAlgebra1._

class CodeConstructor0(val originalProgram: Program, val tree: ProcessTree0, freeVarsInLetrecs: Boolean) {
  def generateProgram() = Program1(originalProgram.ts, construct(tree.rootNode))
  
  private def construct(node: Node): Term1 = node.expr match {
    case t: Term => decompose(t) match {
      case ObservableVar(v) => Variable1(v.name)
      case ObservableCon(c) => Constructor1(c.name, node.children map construct)
      case ObservableVarApp(v, app) => constructApplication1(Variable1(v.name), node.children map construct)
      case ObservableLam(l) => LambdaAbstraction1(Variable1(l.v.name), construct(node.children.head))
      case context: Context => context.redex match {        
        case RedexLamApp(lam, app) => construct(node.children.head)
        case RedexCaseCon(c, ce) => construct(node.children.head)
        case RedexCaseVar(v, CaseExpression(sel, bs)) => {
          if (node.getRepParent != null) {
            val alphaNode: Node = node.getRepParent()
            val alphaT = alphaNode.expr.asInstanceOf[Term]
            
            val (appHead, args) = alphaNode.signature
            val z = constructApplication(Variable(appHead), args)
            
            val msg = strongMsg(alphaT, t)
            // after substitution:
            val sub = Map[Variable, Term]() ++ msg.sub2
            val z1 = applySubstitution(z, sub)
            val z2 = hlToHl1(z1)
            z2
          } else {{
            tree.leafs.filter(n => n.repeatedOf == node) match {
              case Nil => {
                val newBs = (bs zip node.children.tail) map 
                  {p => Branch1(hlToHl1(p._1.pattern), construct(p._2))}
                val newSel = construct(node.children.head)
                CaseExpression1(newSel, newBs)
              }
              case repeatNodes => {
                var vars: Set[Variable] = null
                if (freeVarsInLetrecs){
                  vars = Set[Variable]()
                  for (n <- repeatNodes) {
                    val betaT = n.expr.asInstanceOf[Term]
                    val msg = strongMsg(t, betaT)
                    val args0 = msg.sub2 map {p => p._1}
                    vars = vars ++ args0
                  }
                }
                else {
                  vars = TermAlgebra0.getFreeVars(t)
                }
                val args0 = vars.toList 
                val args = args0 map {hlToHl1(_)}
                val newVars = args map {p => Variable1(createVar().name)}
                val sub = Map[Variable1, Term1]() ++ 
                  ((args0 zip newVars) map {p => (Variable1(p._1.name), p._2)})
                node.signature = (createFName(), args0)
                
                val newBs = (bs zip node.children.tail) map 
                 {p => Branch1(hlToHl1(p._1.pattern), construct(p._2))}
                val newSel = construct(node.children.head)
                val ce = CaseExpression1(newSel, newBs)/sub
                
                val lam = constructLambda1(newVars, ce)
                val appHead = Variable1(node.signature._1)
                appHead.call = true
                LetRecExpression1((appHead, lam), constructApplication1(appHead, args))
              }
            }          
          }}
        }
        case RedexCaseVarApp(a, CaseExpression(sel, bs)) => {
          val newBs = (bs zip node.children.tail) map 
            {p => Branch1(hlToHl1(p._1.pattern), construct(p._2))}
          val newSel = construct(node.children.head)
          CaseExpression1(newSel, newBs)
        }
        case RedexCall(f) => {
          if (node.getRepParent != null) {
            val alphaNode: Node = node.getRepParent()
            val alphaT = alphaNode.expr.asInstanceOf[Term]
            
            val (appHead, args) = alphaNode.signature
            val z = constructApplication(Variable(appHead), args)
            
            val msg = strongMsg(alphaT, t)
            // after substitution:
            val sub = Map[Variable, Term]() ++ msg.sub2
            val z1 = applySubstitution(z, sub)
            val z2 = hlToHl1(z1)
            z2
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
                    val betaT = n.expr.asInstanceOf[Term]
                    val msg = strongMsg(t, betaT)
                    val args0 = msg.sub2 map {p => p._1}
                    vars = vars ++ args0
                  }
                }
                else {
                  vars = TermAlgebra0.getFreeVars(t)
                }
                val args0 = vars.toList 
                val args = args0 map {hlToHl1(_)}
                val newVars = args map {p => Variable1(createVar().name)}
                val sub = Map[Variable1, Term1]() ++ 
                  ((args0 zip newVars) map {p => (Variable1(p._1.name), p._2)})
                node.signature = (createFName(), args0)
                val expr = construct(node.children.head)/sub
                val lam = constructLambda1(newVars, expr)
                val appHead = Variable1(node.signature._1)
                appHead.call = true
                LetRecExpression1((appHead, lam), constructApplication1(appHead, args))
              }
            }            
          }
        }
      }
    }
    case LetExpression(bs, t) => {
      val node0 = node.outs.head.child
      val nodes = node.outs.tail map {edge => edge.child}
      val ts = nodes map construct
      val subs = Map[Variable1, Term1]() ++ ((bs zip ts) map 
          {pair => (Variable1(pair._1._1.name), pair._2)})
      construct(node0)/subs
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
  
  private val vNames = Array('x', 'y', 'z', 'u', 'v', 'w', 'p', 'r', 's', 't');
  private val fNames = Array('f', 'g', 'h');
  private var fUsed = Set[String]()
  
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