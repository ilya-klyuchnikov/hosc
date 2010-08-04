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

class EqualitySpecNaiveWithControlAndCoupling extends EqualitySpec {
  import hosc.sc.NaiveSuperCompilerWithControlAndCoupling
  override def sc(program: Program, goal: Expression) = {
    val sc0 = new NaiveSuperCompilerWithControlAndCoupling(program)
    val pt = sc0.buildProcessTree(goal)
    val g = new CodeConstructor(program, pt, true)
    val p = g.generateProgram()
    p
  }
}