package hosc

import org.junit.Test
import org.junit.Assert._

import Util._

class EqTest {
  
  @Test def test() = {
    testEq("eq/abs1.hs", "eq/abs2.hs")
    testEq("eq/add1.hs", "eq/add2.hs")
    testEq("eq/app1.hs", "eq/app2.hs")
    testEq("eq/churchAdd1.hs", "eq/churchAdd2.hs")
    testEq("eq/churchUnchurch1.hs", "eq/churchUnchurch2.hs")
    testEq("eq/churchPred1.hs", "eq/churchPred2.hs")
    testEq("eq/comp1.hs", "eq/comp2.hs")
    testEq("eq/filter1.hs", "eq/filter2.hs")
    testEq("eq/iter1.hs", "eq/iter2.hs")
    testEq("eq/join1.hs", "eq/join2.hs")
    testEq("eq/lemma1.hs", "eq/lemma2.hs")
    testEq("eq/map_app1.hs", "eq/map_app2.hs")
    testEq("eq/map_comp1.hs", "eq/map_comp2.hs")
    testEq("eq/oddEven1.hs", "eq/oddEven2.hs")
    testEq("eq/orEven1.hs", "eq/orEven2.hs")
    testEq("eq/orEvenOdd1.hs", "eq/orEvenOdd2.hs")
    testEq("eq/pairs1.hs", "eq/pairs2.hs")
    testEq("eq/rep1.hs", "eq/rep2.hs")
  }
  
  def testEq(f1: String, f2: String) = {
    val p1 = supercompile(f1)
    val p2 = supercompile(f2)
    assertTrue(f1 + " and " + f2 + " should be equivalent", Eq.equivalent(p1.goal, p2.goal))
  }
  
  def supercompile(file: String) = {
    val program = programFromFile(file)
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(program.goal)
    val g = new CodeConstructor(program, pt, true)
    val p = g.generateProgram()
    p
  }
}
