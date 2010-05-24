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
    println(expr + "\n" + expSize + "\n" + exprTreeSize)
    for (n <- 1 to (expSize - 1)) {
      println("trying size " + n)
      println("generating...")
      val candidates = gen.generate(n, vars)
      val buf = new ArrayBuffer[Expression]()
      println("testing...")
      for (candidate <- candidates) {
        val (candidateTree, candidateSced) = sc(candidate)
        //val candidateTreeSize = ProcessTreeAlgebra.size(candidateTree)
        //if (localExprs.forall(e => !Eq.equivalent(candidate, e))) {
          if (Eq.equivalent(exprSced, candidateSced)) {
            print(candidate)
            if (TicksAlgebra.isImprovement(candidateSced, exprSced)) {
              //println(candidate)
              //println(candidateSced)
              println(" <=")
              //println(exprSced)
              buf += candidate
            } else {
              //println(candidateSced)
              println(" !<=")
              //println(exprSced)
            }
          }
        //}
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
