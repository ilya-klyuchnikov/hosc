package hosc.sc1

import HLanguage1._
import ProcessTree1._
import TermAlgebra1._
import MSG1._

class CodeConstructor1(program: Program1, tree: ProcessTree1, varGen: VarGen1) {
  
  def constructProgram(node: Node1): Program1 = Program1(program.ts, construct(node))
  
  private def construct(node: Node1): Term1 = node.expr match {
    /*
    case LetExpression1(bindings, _) => {
      construct(node.children.head)/Map(bindings:_*)
    }
    */
    case LetExpression1(bs, t) => {
      val node0 = node.outs.head.child
      val nodes = node.outs.tail map {edge => edge.child}
      val ts = nodes map construct
      val subs = Map[Variable1, Term1]() ++ ((bs zip ts) map 
          {pair => (Variable1(pair._1._1.name), pair._2)})
      construct(node0)/subs
    }
    case term: Term1 => decompose1(term) match {
      case ObservableVar1(v) => v
      case ObservableCon1(c) => Constructor1(c.name, node.children map construct)
      case ObservableVarApp1(v, app) => constructApplication1(v, node.children map construct)
      case ObservableLam1(lambda) => LambdaAbstraction1(lambda.v, construct(node.children.head))
      case context: Context1 => context.redex match {
        case rc: RedexCall1 => throw new IllegalArgumentException("unexpected term: " + term.toString)
        case RedexLamApp1(lam, app) => construct(node.children.head)
        case RedexCaseCon1(c, ce) => construct(node.children.head)
        case RedexCaseVar1(v, CaseExpression1(sel, bs)) => {
          node.repeatedOf match {
            case null => {
              tree.leafs filter {_.repeatedOf == node} match {
                case Nil => {
                  val branches = (bs zip node.children.tail) map {p => Branch1(p._1.pattern, construct(p._2))}
                  val selector = construct(node.children.head)
                  CaseExpression1(selector, branches)
                }
                case repeatNodes => {
                  var vars = Set[Variable1]()                  
                  for (n <- repeatNodes) {
                    val repeatTerm = n.expr.asInstanceOf[Term1]
                    val msg = strongMsg(term, repeatTerm)
                    val args0 = msg.sub2 map {p => p._1}
                    vars = vars ++ args0
                  }                  
                  val fargs = getVarsOrdered(term) filter {vars.contains(_)}
                  val appHead = createFVar
                  node.signature = constructApplication1(appHead, fargs)                  
                  
                  val freshVars = fargs map {x => createVar()}
                  val sub = Map((fargs zip freshVars):_*)
                  
                  val newBs = (bs zip node.children.tail) map {p => Branch1(p._1.pattern, construct(p._2))}
                  val newSel = construct(node.children.head)
                  val ce = CaseExpression1(newSel, newBs)/sub
                  
                  val lambda = constructLambda1(freshVars, ce)
                  LetRecExpression1((appHead, lambda), node.signature)
                }
              }
            }
            case parentNode => {
              val parentTerm = parentNode.expr.asInstanceOf[Term1]          
              val msg = strongMsg(parentTerm, term)
              parentNode.signature/Map(msg.sub2:_*)
            }
          }
        }
        case RedexCaseVarApp1(a, CaseExpression1(sel, bs)) => {
          // TODO it also possible to have cycle here
          val branches = (bs zip node.children.tail) map {p => Branch1(p._1.pattern, construct(p._2))}
          val selector = construct(node.children.head)
          CaseExpression1(selector, branches)
        }
        case RedexLetRec1(letrec) => {            
          node.repeatedOf match {
            case null => {
              tree.leafs filter {_.repeatedOf == node} match {
                case Nil => {
                  construct(node.children.head)
                }
                case repeatNodes => {
                  var vars = Set[Variable1]()                  
                  for (n <- repeatNodes) {
                    val repeatTerm = n.expr.asInstanceOf[Term1]
                    val msg = strongMsg(term, repeatTerm)
                    val args0 = msg.sub2 map {p => p._1}
                    vars = vars ++ args0
                  }                  
                  val fargs = getVarsOrdered(term) filter {vars.contains(_)}
                  val appHead = createFVar()//varsUtil.createFreshLetrecVar()
                  node.signature = constructApplication1(appHead, fargs)
                  val freshVars = fargs map {x => createVar()}
                  val sub = Map((fargs zip freshVars):_*)
                  val lambdaBody = construct(node.children.head)/sub
                  val lambda = constructLambda1(freshVars, lambdaBody)
                  LetRecExpression1((appHead, lambda), node.signature)
                }
              }
            }
            case parentNode => {
              val parentTerm = parentNode.expr.asInstanceOf[Term1]          
              val msg = strongMsg(parentTerm, term)
              parentNode.signature/Map(msg.sub2:_*)
            }
          }
        }
      }
    }    
  }
  
  var i = 0;
  val usedVars = tree.rootNode.getAllVars1()
  def createVar(): Variable1 = {      
    var nv: Variable1 = null
    do {
      nv = varFor(i)
      i += 1
    } while (usedVars contains nv)
    nv
    varGen.createFreshVar
  }
  
  var fi = 0;
  def createFVar(): Variable1 = {      
    var nv: Variable1 = null
    do {
      nv = fVarFor(fi)
      fi += 1
    } while (usedVars contains nv)
    nv.call = true
    nv
    varGen.createFreshLetrecVar
  }
  
  private val vNames = Array('x', 'y', 'z', 'u', 'v', 'w', 'p', 'r', 's', 't');
  private val fNames = Array('f', 'g', 'h');
  private var fUsed = Set[String]()
  
  private def varFor(j: Int) = {
    if (j < 10) 
      Variable1("" + vNames(j))
    else 
      Variable1("" + vNames(j % 10) + Integer.toString(j / 10))   
  }
  
  private def fVarFor(j: Int) = {
    if (j < 3) 
      Variable1("" + fNames(j))
    else 
      Variable1(fNames(j % 3) + Integer.toString(j / 3))   
  }
}
