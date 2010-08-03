package hosc.spec

import hosc.HLanguage._
import hosc.CodeConstructor

class EqualitySpecNaive extends EqualitySpec {
  import hosc.sc.NaiveSuperCompiler
  override def sc(program: Program, goal: Expression) = {
    val sc0 = new NaiveSuperCompiler(program)
    val pt = sc0.buildProcessTree(goal)
    val g = new CodeConstructor(program, pt, true)
    val p = g.generateProgram()
    p
  }
}