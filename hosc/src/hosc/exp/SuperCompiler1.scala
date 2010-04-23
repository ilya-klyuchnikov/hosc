package hosc.exp

import hosc.HLanguage._
import hosc.HE._
import hosc.MSG._
import hosc.TermAlgebra._
import hosc.ProcessTree
import hosc.CodeConstructor
import hosc.SuperCompiler0
import hosc.ProcessTree._
import hosc.LangUtils._

class SuperCompiler1(program: Program) extends SuperCompiler0(program){
  debug = false
  useControl = false
  
  override protected def heByCouplingTest(bNode: Node)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm if heByCoupling(aTerm, bNode.expr) && checkControl(aNode, bNode) => {
      val sca = sc(aTerm)
      val scb = sc(bNode.expr)
      HE1.heByCoupling(sca, scb)
    }
    case _ => false
  }
  
  /*
  private def heByCouplingTest(bNode: Node)(aNode: Node): Boolean = aNode.expr match {
    case LetExpression(_, _) => false
    case aTerm if (sameRedex(aTerm, bNode.expr) && heByCoupling(aTerm, bNode.expr)) && checkControl(aNode, bNode) => { 
      //return true;
      
      if (debug) {
        println(format(canonize(aTerm)))
        println("------")
        println(format(canonize(bNode.expr)))
        println("------")
      }
      
      val sca = sc(aTerm);
      val scb = sc(bNode.expr);
      
      if (debug) {
        println(format(sca))
        println("------")
        println(format(scb))
        println("------")
        println("------")
      }
      
      val r = HE1.heByCoupling(sca, scb)
      if (debug) {
        println(r)
      }
      r
    };
    case _ => false
  }
  */
  
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
  
}