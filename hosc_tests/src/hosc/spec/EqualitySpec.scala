package hosc.spec

import org.junit.Test
import org.junit.Assert._

import hosc.Util._
import hosc.TestUtils._
import hosc.HLanguage._
import hosc.{SuperCompiler0, CodeConstructor, Eq}

class EqualitySpec {
  
  @Test def append =
    testEq("lists", 
           "appendR (appendR xs ys) zs", 
           "appendR xs (appendR ys zs)")
  
  @Test def map =
    testEq("lists", 
           """listR f xs""", 
           """foldR NilR (\a x -> Cons (f a) x) xs""")
  
  @Test def append1 =
    testEq("lists", 
           "appendR xs ys", 
           "foldR ys cons xs")
  
  @Test def append2 =
    testEq("lists", 
           "appendR xs ys", 
           """foldR id (\x f y -> cons x (f y)) xs ys""")
  
  @Test def concat =
    testEq("lists", 
           "concatR xs", 
           "foldR NilR catR xs")
  
  @Test def length1 =
    testEq("lists", 
           """lengthR xs""", 
           """foldR Z (\a -> plus (S Z)) xs""")
  
  @Test def length2 =
    testEq("lists", 
           """lengthR xs""", 
           """(compose sum (listR (\x -> S Z))) xs""")
  
  @Test def convert =
    testEq("lists", 
           """convertL2R xs""", 
           """convertL2R xs""")
  
  @Test def filter1 =
    testEq("lists", 
           """filterR p xs""", 
           """(compose concatR (listR (cond p wrapR nilpR))) xs""")
  
  @Test def filter2 =
    testEq("lists", 
           """filterR p xs""", 
           """(compose (foldR NilR catR) (listR (cond p wrapR nilpR))) xs""")
  
  @Test def filter3 =
    testEq("lists", 
           """filterR p xs""", 
           """(compose (foldR NilR catR) (foldR NilR (\a x -> Cons ((cond p wrapR nilpR) a) x))) xs""")
  
  @Test def filter4 =
    testEq("lists", 
           """filterR p xs""", 
           """foldR1 NilR (cond (compose p outl) (uncurry cons) outr) xs""")
  
  @Test def filter5 =
    testEq("lists", 
           """filterR p xs""", 
           """foldR NilR (curry (cond (compose p outl) (uncurry cons) outr)) xs""")

  def testEq(input: String, goal1: String, goal2: String): Unit = {
    
    val file = "spec_eq/" + input + ".hs"
    val program = programFromFile(file)
    val input1 = termFromString(goal1, program)
    val input2 = termFromString(goal2, program)
    val p1 = sc(program, input1)
    val p2 = sc(program, input2)
    println("===================")
    println(goal1 + " == " + goal2)
    println(p1.toDocString)
    println(p2.toDocString)
    assertTrue(goal1 + " and " + goal2 + " should be equivalent", Eq.equivalent(p1.goal, p2.goal))
  }
  
  def sc(program: Program, goal: Expression) = {
    val sc0 = new SuperCompiler0(program)
    val pt = sc0.buildProcessTree(goal)
    val g = new CodeConstructor(program, pt, true)
    val p = g.generateProgram()
    p
  }
}
