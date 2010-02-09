package hosc.lemmas

import scala.collection.mutable.{ArrayBuffer, Buffer}

import hosc.{SuperCompiler, CodeConstructor, TermAlgebra, Eq, ProcessTree, ProcessTreeAlgebra}
import hosc.HLanguage._

class LemmaFinder(val program: Program) {
  val scp = new SuperCompiler(program)
  scp.renameVars = false
  val gen = new ExpressionGenerator(program)
  
  def findEqExpressions(expr: Expression, localExprs: List[Expression]): List[Expression] = {
    val (exprTree, exprSced) = sc(expr)
    val expSize = TermAlgebra.size(expr)
    val exprTreeSize = ProcessTreeAlgebra.size(exprTree)
    val vars = TermAlgebra.getFreeVars(expr)
    println(expr + "\n" + expSize + "\n" + exprTreeSize)
    for (re <- localExprs) {
      println(re)
    }
    for (n <- 1 to (expSize - 1)) {
      println("trying size " + n)
      val candidates = gen.generate(n, vars)
      val buf = new ArrayBuffer[Expression]()
      for (candidate <- candidates) {
        val (candidateTree, candidateSced) = sc(candidate)
        val candidateTreeSize = ProcessTreeAlgebra.size(candidateTree)
        if (localExprs.forall(e => !Eq.equivalent(candidate, e))) {
          if (Eq.equivalent(exprSced, candidateSced)) {
            println(candidate)
            if (candidateTreeSize < exprTreeSize) {
              println(candidateTreeSize + "<" + exprTreeSize)
              buf += candidate
            } else {
              println(candidateTreeSize + ">=" + exprTreeSize)
            }
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
