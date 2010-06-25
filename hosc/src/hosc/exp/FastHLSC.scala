package hosc.exp

import hosc.HLanguage._
import hosc.CodeConstructor
import hosc.SuperCompiler0
import hosc.ProcessTree
import hosc.ProcessTree._
import hosc.TermAlgebra
import hosc.lemmas.LemmaFinder
import hosc.TicksAlgebra
import hosc.Instance

// Fast Higher-Level Supercompiler
// the source for lemmas is computation history.
class FastHLSC(program: Program) extends SuperCompiler0(program){
  val finder = new LemmaFinder(program)
  debug = false
  
  override def abstractUp(t: ProcessTree, up: Node, down: Node): Unit = {
    println("=======")
    println(up.expr)
    println(down.expr)
    parents(down) map {good(down)} filter{_.isDefined} match {
      case Nil => {
        assume(false, "None")
        //super.abstractUp(t, up, down)
        println("passing")
        drive(t, down)
      }
      case Some(u) :: _ => {
        println(u)
        //assume(false, "Some")
        println("********")
        println("replacing")
        println(down.expr)
        println("=>")
        println(u)
        t.replace(down, u)
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
  
  def good(down: Node)(cand: Node): Option[Expression] = {
    cand.expr match {
    case l:LetExpression => None
    case e => {
      val e0 = sc(down.expr)
      val e1 = sc(cand.expr)
      if (TermAlgebra.equivalent(e0, e1)) {
        //println(cand.expr)
        //println(down.expr)
        //println(e1)
        //println(e0)
        println(TicksAlgebra.isImprovement(e1, e0))
        val sub = Instance.findSubst(e1, e0)
        val res = TermAlgebra.applySubstitution(cand.expr, sub)
        //println(res)
        Some(res)
      } else {
        None
      }
    }
  }
  }
}
