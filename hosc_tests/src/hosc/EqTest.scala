package hosc

import org.junit.Test
import org.junit.Assert._

import Util._

class EqTest {
  
  @Test def test() = {
    testEq("eq/app1.hs", "eq/app2.hs")
    testEq("eq/iter1.hs", "eq/iter2.hs")
    testEq("eq/comp1.hs", "eq/comp2.hs")
  }
  
  def testEq(f1: String, f2: String) = {
    val p1 = supercompile(f1)
    val p2 = supercompile(f2)
    assertTrue(f1 + " and " + f2 + " should be equivalent", Eq.equivalent(p1.goal, p2.goal))
  }
  
  def supercompile(file: String) = {
    val program = programFromFile(file)
    val sc = new sc0.SuperCompiler0(program)
    val pt = sc.buildProcessTree(program.goal)
    val g = new sc0.CodeConstructor0(program, pt, true)
    val p = g.generateProgram()
    p
  }
}
