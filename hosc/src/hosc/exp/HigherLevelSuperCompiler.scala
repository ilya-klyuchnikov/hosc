package hosc.exp

import hosc.HLanguage._
import hosc.SuperCompiler
import hosc.ProcessTree
import hosc.ProcessTree._
import hosc.lemmas.LemmaFinder

class HigherLevelSuperCompiler(program: Program) extends SuperCompiler(program){
  val finder = new LemmaFinder(program)
  debug = true
  override def abstractUp(t: ProcessTree, up: Node, down: Node): Unit = {
    val exp = down.expr
    val eqExprs = finder.findEqExpressions(exp, redExps(down))
    eqExprs match {
      case Nil => super.abstractUp(t, up, down)
      case exp1 :: exps1 => t.replace(down, exp1)
    }
  } 

  def redExps(n: Node): List[Expression] = {
    n.ancestors.takeWhile(!isGlobal(_)) map {_.expr}
  }
}
