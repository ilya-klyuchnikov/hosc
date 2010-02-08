package hosc.lemmas

import scala.collection.mutable.{ArrayBuffer, Buffer}

import hosc.{SuperCompiler, CodeConstructor, TermAlgebra, Eq}
import hosc.HLanguage._

class LemmaFinder(val program: Program) {
  val scp = new SuperCompiler(program)
  scp.renameVars = false
  val gen = new ExpressionGenerator(program)
  
  def findEqExpressions(expr: Expression, redExprs: List[Expression]): List[Expression] = {
    val exprSced = sc(expr)
    val size = TermAlgebra.size(expr)
    val vars = TermAlgebra.getFreeVars(expr)
    println(expr + " " + size)
    for (re <- redExprs) {
      println(re)
    }
    for (n <- 1 to (size - 1)) {
      println("trying size " + n)
      val candidates = gen.generate(n, vars)
      val buf = new ArrayBuffer[Expression]()
      for (candidate <- candidates) {
        val candidateSced = sc(candidate)
        if (redExprs.forall(e => !Eq.equivalent(candidate, e))) {
          if (Eq.equivalent(exprSced, candidateSced)) {
            println(candidate)
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
  
  private def sc(expr: Expression): Expression = {
    val pt = scp.buildProcessTree(expr)
    new CodeConstructor(program, pt, true).generateProgram().goal
  }
}
