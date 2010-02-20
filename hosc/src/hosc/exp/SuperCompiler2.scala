package hosc.exp

import hosc.HLanguage._
import hosc.HE._
import hosc.MSG._
import hosc.TermAlgebra._
import hosc.ProcessTree
import hosc.CodeConstructor
import hosc.SuperCompiler
import hosc.ProcessTree._
import hosc.LangUtils._

class SuperCompiler2(program: Program) extends SuperCompiler1(program){
  debug = true
  useControl = true
  
  override protected def heByCouplingTest(bNode: Node)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm if heByCoupling(aTerm, bNode.expr) && checkControl(aNode, bNode) => {
      val sca = sc(aTerm)
      val scb = sc(bNode.expr)
      HE1.heByCoupling(sca, scb)
    }
    case _ => false
  }
  
  private def sc(expr: Expression): Expression = {
    val sc0 = new SuperCompiler1(program)
    sc0.renameVars = false
    if (debug) {
      println("* sc1 *")
      println(expr)
    }
    val pt = sc0.buildProcessTree(expr)
    new CodeConstructor(program, pt, true).generateProgram().goal
  }
  
}