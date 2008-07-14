package hosc;

import HLanguage1._
import sc1.ProcessTree1
import sc1.ProcessTree1._
import TermAlgebra1._
import MSG1._

class CodeConstructor1(val tree: ProcessTree1, val varsUtil: Vars1Util) {
  
  def construct(node: Node1): Term1 = node.expr match {
    case LetExpression1(bindings, _) => {
      construct(node.children.head)/Map(bindings:_*)
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
                  val appHead = varsUtil.createFreshLetrecVar()
                  node.signature = constructApplication1(appHead, fargs)
                  
                  
                  val freshVars = fargs map {x => newVar1()}
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
                  val appHead = varsUtil.createFreshLetrecVar()
                  node.signature = constructApplication1(appHead, fargs)
                  val freshVars = fargs map {x => newVar1()}
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
}
