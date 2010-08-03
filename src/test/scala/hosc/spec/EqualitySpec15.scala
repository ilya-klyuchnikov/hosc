package hosc.spec

import org.junit.{Test, Ignore}
import org.junit.Assert._

import hosc.Util._
import hosc.TUtils._
import hosc.HLanguage._
import hosc.{SuperCompiler0, CodeConstructor, Eq, LambdaLifting, Postprocessor}

class EqualitySpec15 extends EqualitySpec {
  
  import hosc.sc.HOSC15
  override def sc(program: Program, goal: Expression) = {
    val sc0 = new HOSC15(program)
    val pt = sc0.buildProcessTree(goal)
    val g = new CodeConstructor(program, pt, true)
    val p = g.generateProgram()
    p
  }
}
