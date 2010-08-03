package hosc.sc_old

import hosc.{Instance, MSG, TermAlgebra}
import hosc.ProcessTree
import hosc.ProcessTree._
import hosc.HLanguage._

trait TreeOperations {
  def abstractDown(t: ProcessTree, up: Node, down: Node): ProcessTree = {
    val a = up.expr
    val b = down.expr
    val sub = Instance.findSubst(a, b)
    val keyMap = Map(sub.keys.toList map {k => (k, TermAlgebra.newVar())}:_*)
    val eg = a/keyMap
    val sub1 = (sub map {case (k, v) => (keyMap(k), v)}).toList
    t.replace(down, LetExpression(sub1, eg))
  }
  
  def abstractUp(t: ProcessTree, up: Node, down: Node): ProcessTree = {
    val g = MSG.msgExt(up.expr, down.expr)
    t.replace(up, LetExpression(g.sub1, g.term))
  }
  
}
