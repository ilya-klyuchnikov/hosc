package hosc.exp

import hosc.HLanguage._
import hosc.CodeConstructor
import hosc.SuperCompiler0
import hosc.ProcessTree
import hosc.ProcessTree._
import hosc.TermAlgebra
import hosc.lemmas.LemmaFinder

class HLSC1(program: Program) extends SuperCompiler0(program){
  val finder = new LemmaFinder(program)
  debug = false
  
  override def abstractUp(t: ProcessTree, up: Node, down: Node): Unit = {
    parents(down) find {good(down)} match {
      case None => {
        assume(false, "None")
        //super.abstractUp(t, up, down)
        println("passing")
        drive(t, down)
      }
      case Some(u) => {
        assume(false, "Some")
        println("********")
      }
    }
  } 

  def parents(n: Node) = {
    n.ancestors.find(isGlobal(_)).get.ancestors
  }
  
  private def sc(expr: Expression): Expression = {
    val sc0 = new SuperCompiler0(program)
    sc0.renameVars = false
    if (debug) {
      println("* sc0 *")
      println(expr)
    }
    val pt = sc0.buildProcessTree(expr)
    new CodeConstructor(program, pt, true).generateProgram().goal
  }
  
  def good(down: Node)(cand: Node): Boolean = {
    cand.expr match {
    case l:LetExpression => false
    case e => {
      val e0 = sc(down.expr)
      val e1 = sc(cand.expr)
      return TermAlgebra.equivalent(e0, e1)
    }
  }
    }
}
