package hosc.spec

import hosc.HLanguage._
import hosc.CodeConstructor

class EqualitySpecNaiveWithCoupling extends EqualitySpec {
  import hosc.sc.NaiveSuperCompilerWithCoupling
  override def sc(program: Program, goal: Expression) = {
    val sc0 = new NaiveSuperCompilerWithCoupling(program)
    val pt = sc0.buildProcessTree(goal)
    val g = new CodeConstructor(program, pt, true)
    val p = g.generateProgram()
    p
  }
}

class EqualitySpecNaiveWithControl extends EqualitySpec {
  import hosc.sc.NaiveSuperCompilerWithControl
  override def sc(program: Program, goal: Expression) = {
    val sc0 = new NaiveSuperCompilerWithControl(program)
    val pt = sc0.buildProcessTree(goal)
    val g = new CodeConstructor(program, pt, true)
    val p = g.generateProgram()
    p
  }
}