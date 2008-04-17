package hosc;

import HLanguage._
import HLanguage1._
import ProcessTree._
import TermAlgebra._

class ResidualProgramGenerator(val tree: ProcessTree) {
  
  def generateProgram() = construct(tree.rootNode)
  
  private def construct(node: Node): Expression1 = node.expr match {
    case t: Term => decompose(t) match {
      case ObservableVar(v) => Variable1(v.name)
      case ObservableCon(c) => Constructor1(c.name, node.children map construct)
      case ObservableVarApp(v, app) => constructApplication1(Variable1(v.name), node.children map construct)
      case ObservableLam(l) => LambdaAbstraction1(Variable1(l.v.name), construct(node.children.head))
      case context: Context => context.redex match {        
        case RedexLamApp(lam, app) => construct(node.children.head)
        case RedexCaseCon(c, ce) => construct(node.children.head)
        case RedexCaseVar(v, CaseExpression(sel, bs)) => {
          val newBs = (bs zip node.children.tail) map 
            {p => Branch1(convertPattern(p._1.pattern), construct(p._2))}
          val newSel = construct(node.children.head)
          CaseExpression1(newSel, newBs)
        }
        case RedexCaseVarApp(a, CaseExpression(sel, bs)) => {
          val newBs = (bs zip node.children.tail) map 
            {p => Branch1(convertPattern(p._1.pattern), construct(p._2))}
          val newSel = construct(node.children.head)
          CaseExpression1(newSel, newBs)
        }
        case RedexCall(f) => {
          if (node.outs.isEmpty) {
            var edge = node.in
            var alphaNode: Node = null
            while (alphaNode == null) {
              val node1 = edge.parent
              node1.expr match {
                case pt: Term => if (instanceOf(pt, t)) alphaNode = node1
                case _ => 
              }
              edge = node1.in
            }
            val alphaT = alphaNode.expr.asInstanceOf[Term]
            val msg = strongMsg(alphaT, t)
            val appHead = Variable1(alphaNode.newFName)
            val args = msg.sub2 map {p => convert(p._2)}
            constructApplication1(appHead, args)
          } else {
            tree.leafs.find(n => n.ancestors.contains(node) && 
              (n.expr match {case ct: Term => instanceOf(t, ct); case _=> false})) match {
              case None => 
                construct(node.children.head);
              case Some(repeatNode) => {
                val betaT = repeatNode.expr.asInstanceOf[Term]
                val msg = strongMsg(t, betaT)
                val args = msg.sub2 map {p => convert(p._1)}
                val newVars = msg.sub2 map {p => Variable1(newVar().name)}
                val sub = Map[Variable1, Expression1]() ++ 
                  ((msg.sub2 zip newVars) map {p => (Variable1(p._1._1.name), p._2)})
                node.newFName = newVar().name
                val expr = applySubstitution1(construct(node.children.head), sub)
                val lam = constructLambda(newVars, expr)
                val appHead = Variable1(node.newFName)
                LetRecExpression1(List((appHead, lam)), constructApplication1(appHead, args))
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
      val subs = Map[Variable1, Expression1]() ++ ((bs zip ts) map 
          {pair => (Variable1(pair._1._1.name), pair._2)})
      applySubstitution1(construct(node0), subs)
    }
  }
  
}