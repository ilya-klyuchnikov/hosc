package hosc;

import HLanguage1._
import ProcessTree1._
import TermAlgebra1._
import MSG1._

/**
  performs advanced folding (residual code generation) for a given 
  partial process tree
*/
class Folder(tree: ProcessTree1, constructor: CodeConstructor) {
  def fold(): Boolean = {
    // phase1 folding
    var folded = false
    var continue = true
    while (continue) {
      tree.leafs find {
        n => n.repeatedOf != null && 
          n.repeatedOf.allRepeatedOf.size == 1 &&
            ((n.repeatedOf.allLoops - n) - n.repeatedOf).isEmpty} 
      match {
        case None => continue = false
        case Some(node) => {
          val parentNode = node.repeatedOf
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
          if (letrecP){
            val alpha = parentNode.in.parent
            val code = constructor.construct(alpha)
            val alphaReplaced = tree.replace(alpha, code)
            code match {
              case l: LetRecExpression1 => alphaReplaced.permanent = true
              case _ =>
            }
            folded = true
          } else {
            val alpha = parentNode
            val code = constructor.construct(alpha)
            val alphaReplaced = tree.replace(alpha, code)
            code match {
              case l: LetRecExpression1 => alphaReplaced.permanent = true
              case _ =>
            }
            folded = true
          }
        }
      }      
    }
    folded
  }
  
}
