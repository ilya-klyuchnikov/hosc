package hosc;

import HLanguage1._
import ProcessTree1._
import TermAlgebra1._
import MSG1._

class CodeConstructor(val tree: ProcessTree1) {
  val freeVarsInLetrecs = true
  var i = 0
  def createFName() = {
    i += 1
    "f$" + i
  }
  
  def construct(node: Node1): Term1 = node.expr match {
    case LetExpression1(bindings, _) => {
      construct(node.children.head)/Map(bindings:_*)
    }
    case term: Term1 => term.label match { 
      case Loop() => node.repeatedOf match {
        case null => {
          tree.leafs filter {_.repeatedOf == node} match {
            case Nil => construct(node.children.head)
            case repeatNodes => {
              var vars = Set[Variable1]()
              if (freeVarsInLetrecs){
                for (n <- repeatNodes) {
                  val repeatTerm = n.expr.asInstanceOf[Term1]
                  val msg = strongMsg(term, repeatTerm)
                  val args0 = msg.sub2 map {p => p._1}
                  vars = vars ++ args0
                }
              } else {
                vars = getFreeVars(term)
              }
              val fargs = getVarsOrdered(term) filter {vars.contains(_)}
              val appHead = Variable1(createFName())
              appHead.call = true
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
      case Repeat() => construct(node.children.head)
      case null =>  decompose1(term) match {
        case ObservableVar1(v) => v
        case ObservableCon1(c) => Constructor1(c.name, node.children map construct)
        case ObservableVarApp1(v, app) => constructApplication1(v, node.children map construct)
        case ObservableLam1(lambda) => LambdaAbstraction1(lambda.v, construct(node.children.head))
        case context: Context1 => context.redex match {
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
                  case some => {
                    val branches = (bs zip node.children.tail) map {p => Branch1(p._1.pattern, construct(p._2))}
                    val selector = construct(node.children.head)
                    val code = CaseExpression1(selector, branches)
                    code.label = Loop()
                    code
                  }
                }
              }
              case a => {
                term.label = Repeat()
                term
              }
            }
          }
          case RedexCaseVarApp1(a, CaseExpression1(sel, bs)) => {
            val branches = (bs zip node.children.tail) map {p => Branch1(p._1.pattern, construct(p._2))}
            val selector = construct(node.children.head)
            CaseExpression1(selector, branches)
          }
          case RedexLetRec1(letrec) => {            
            if (node.children.isEmpty) node.repeatedOf match {
              // case 1: the leaf that is not an instance of it ancestor
              case null => term
              // case 2: the leaf that is an instance of it ancestor: folding is performed
              case parentNode => {
                val parentTerm = parentNode.expr.asInstanceOf[Term1]          
                val msg = strongMsg(parentTerm, term)
                parentNode.signature/Map(msg.sub2:_*)
              }
            } else {
              tree.leafs filter {_.repeatedOf == node} match {
                case Nil => {
                  val letrecCallNode = node.children.head
                  val letrecCall = letrecCallNode.expr
                  tree.leafs filter {_.repeatedOf == letrecCallNode} match {
                    case Nil => construct(letrecCallNode) 
                    case repeatNodes => {
                      var vars = Set[Variable1]()
                      if (freeVarsInLetrecs){
                        for (n <- repeatNodes) {
                          val repeatTerm = n.expr.asInstanceOf[Term1]
                          val msg = strongMsg(letrecCall.asInstanceOf[Term1], repeatTerm)
                          val args0 = msg.sub2 map {p => p._1}
                          vars = vars ++ args0
                        }
                      } else {
                        vars = getFreeVars(term)
                      }
                      val fargs = getVarsOrdered(term) filter {vars.contains(_)}
                      val appHead = Variable1(createFName())
                      appHead.call = true
                      node.signature = constructApplication1(appHead, fargs)
                      val freshVars = fargs map {x => newVar1()}
                      val sub = Map((fargs zip freshVars):_*)
                      val lambdaBody = construct(letrecCallNode.children.head)/sub
                      val lambda = constructLambda1(freshVars, lambdaBody)
                      LetRecExpression1((appHead, lambda), node.signature)
                    }
                  }
                }
                case repeatNodes => {
                  var vars = Set[Variable1]()
                  if (freeVarsInLetrecs){
                    for (n <- repeatNodes) {
                      val repeatTerm = n.expr.asInstanceOf[Term1]
                      val msg = strongMsg(term, repeatTerm)
                      val args0 = msg.sub2 map {p => p._1}
                      vars = vars ++ args0
                    }
                  } else {
                    vars = getFreeVars(term)
                  }
                  val fargs = getVarsOrdered(term) filter {vars.contains(_)}
                  val appHead = Variable1(createFName())
                  appHead.call = true
                  node.signature = constructApplication1(appHead, fargs)
                  val freshVars = fargs map {x => newVar1()}
                  val sub = Map((fargs zip freshVars):_*)
                  val lambdaBody = construct(node.children.head)/sub
                  val lambda = constructLambda1(freshVars, lambdaBody)
                  LetRecExpression1((appHead, lambda), node.signature)
                }
              }
              
            }
          }
          case RedexCall1(f) => node.repeatedOf match {
             case null => {
               val code = construct(node.children.head)
               if (tree.leafs exists {_.repeatedOf == node}) code.label = Loop()
               code
             }
             case parentNode => {
               val letrecP: Boolean = parentNode.in match {
                 case null => false
                 case some => some.parent.expr match {
                   case possibleLetrec: Term1 => decompose1(possibleLetrec) match {
                     case possibleLetRecContext: Context1 => possibleLetRecContext.redex match {
                       case RedexLetRec1(_) => true
                       case _ => false
                     }
                     case _ => false
                   }
                   case _ => false
                 }
               }
               if (letrecP) {
                 val signature = parentNode.in.parent.signature
                 val parentTerm = parentNode.expr.asInstanceOf[Term1]          
                 val msg = strongMsg(parentTerm, term)
                 signature/Map(msg.sub2:_*)
               } else {
                 term.label = Repeat()
                 term
               }
             }
          }
        }
      }    
    }
  }
}
