package hosc

import HLanguage1._
import ProcessTree1._
import TermAlgebra1._
import MSG1._

class Ungeneralizer(tree: ProcessTree1, constructor: CodeConstructor) {
  def ungeneralize(): Boolean = {
    println("ungeneralized")
    var ungeneralized = false
    var someWorkWasDone = false
    do {
      someWorkWasDone = false
      val letchilds = tree.leafs filter {n => n.expr match {case l: LetRecExpression1 => true; case _ => false;}}
      for (c <- letchilds) {
        c.ancestors() find {p => p.expr match {case l: LetExpression1 => true; case _ => false}} match {
          case Some(alpha) => {
            val code = constructor.construct(alpha)
            tree.replace(alpha, code)
            ungeneralized = true
            someWorkWasDone = true
          }
          case None =>
        }
      }} while (someWorkWasDone)
    ungeneralized
  }
}
