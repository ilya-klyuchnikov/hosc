package hosc.spec

import hosc.HLanguage._
import hosc.CodeConstructor

class EqualitySpecNaiveTransientAware extends EqualitySpec {
  import hosc.sc.NaiveSuperCompilerTransientAware
  override def sc(program: Program, goal: Expression) = {
    val sc0 = new NaiveSuperCompilerTransientAware(program)
    val pt = sc0.buildProcessTree(goal)
    val g = new CodeConstructor(program, pt, true)
    val p = g.generateProgram()
    p
  }
}