package hosc.lemmas

import scala.collection.mutable.{ArrayBuffer, Buffer}

import hosc.{SuperCompiler0, CodeConstructor, TermAlgebra, Eq, ProcessTree, ProcessTreeAlgebra}
import hosc.HLanguage._
import hosc.TicksAlgebra

class LemmaFinder(val program: Program) {
  val scp = new SuperCompiler0(program)
  scp.renameVars = false
  scp.useControl = false
  val gen = new ExpressionGenerator(program)
  
  def findEqExpressions(expr: Expression, localExprs: List[Expression]): List[Expression] = {
    val (exprTree, exprSced) = sc(expr)
    val expSize = TermAlgebra.size(expr)
    val exprTreeSize = ProcessTreeAlgebra.size(exprTree)
    val vars = TermAlgebra.getFreeVars(expr)
    for (n <- 1 to (expSize - 1)) {
      val candidates = gen.generate(n, vars)
      val buf = new ArrayBuffer[Expression]()
      for (candidate <- candidates) {
        val (candidateTree, candidateSced) = sc(candidate)
          if (Eq.equivalent(exprSced, candidateSced)) {
            if (TicksAlgebra.isImprovement(candidateSced, exprSced)) {
              buf += candidate
            }
          }
      }
      if (!buf.isEmpty) {
        return buf.toList
      }
    }
    return Nil
  }
  
  private def sc(expr: Expression): (ProcessTree, Expression) = {
    val pt = scp.buildProcessTree(expr)
    val scExpr = new CodeConstructor(program, pt, true).generateProgram().goal
    (pt, scExpr)
  }
}
