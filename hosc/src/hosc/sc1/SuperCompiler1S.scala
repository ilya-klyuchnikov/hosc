package hosc.sc1;

import HLanguage1._

class SuperCompiler1S(val program: Program1, varsUtil: VarGen1) {
  def superCompile(): Program1 = {
    var superCompiledP1 = (new SuperCompiler1(program, varsUtil).superCompile())._2
    val eq0 = TermAlgebra1.equivalent(superCompiledP1.expr, program.expr)
     println("eq0 = " + eq0)
    var superCompiledP2 = (new SuperCompiler1(superCompiledP1, varsUtil).superCompile())._2
    val eq1 = TermAlgebra1.equivalent(superCompiledP1.expr, superCompiledP2.expr)
    println("eq1 = " + eq1)
    var i = 1
    while (!TermAlgebra1.equivalent(superCompiledP1.expr, superCompiledP2.expr)) {
      println("SuperCompiler1S " + i)
      superCompiledP1 = superCompiledP2
      superCompiledP2 = (new SuperCompiler1(superCompiledP2, varsUtil).superCompile())._2
      i += 1
    }
    superCompiledP2
  }
}
