package hosc;

import org.junit.Test
import org.junit.Assert._
import sc0.TermAlgebra0._
import sc0.HE0._
import TestUtils._
import hosc.LangUtils.{canonize => can}

class MSGTest {
  @Test def msg1(): Unit = {
    testInstanceOf( 
      """
      case x1 of {
        S s1 : f s1 (S (S v)) g;
        Z : g (S v) d;
      }
      """
      , 
      """
      case x2 of {
        S s2 : f s2 (S (S (S v))) g;
        Z : g (S (S v)) d;
      }
      """
      , 
      true)    
  }
  
  @Test def msg2(): Unit = {
    testInstanceOf( 
      """
      case x1 of {
        S s1 : f s1 (S (S v)) g;
        Z : g (S v) d;
      }
      """
      , 
      """
      case x2 of {
        S s2 : f s2 (S (S (S v))) g;
        Z : g (S (S v)) d;
      }
      """
      , 
      true)    
  }
  
  @Test def msg3(): Unit = {
    val input1 = 
      """
         |case (x1) of {
         |  S s1 : (S (S v));
         |  Z : (S v);
         |}
      """.stripMargin;
      
    val input2 = 
      """
         |case (s1) of {
         |  S s2 : (S (S (S v)));
         |  Z : (S (S v));
         }
      """.stripMargin;     
    
    val expectedMsgInput = 
      """
        |case y of {
        |  Nil : Nil;
        |  Cons a1 b1 : app (rev z) (Cons aa1 Nil);
        |}
      """.stripMargin;
    val term1 = termFromString(input1)
    val term2 = termFromString(input2)
    val term = termFromString(expectedMsgInput)
    val actualMsg = msg(term1, term2)
    println(actualMsg)
    assertTrue(heByCoupling(term1, term2))
    assertTrue(instanceOf(term1, term2))
  }
  
  @Test def case01(): Unit = {
    val input1 = "C a a"      
    val input2 = "C b b"
      
    val term1 = termFromString(input1)
    val term2 = termFromString(input2)
    val actualMsg = msg(term1, term2)
    println(actualMsg)
  }
  
  @Test def case02(): Unit = {
    val input1 = "case u1 of {S y1: S (add y1 (fib (S w1))); Z: fib (S w1);}"
      
    val input2 = "case y1 of {S p1: S (add p1 (fib (S w1))); Z: fib (S w1);}"     
    
    val term1 = termFromString(input1)
    val term2 = termFromString(input2)
    val actualMsg = msg(term1, term2)
    println(actualMsg)
    assertTrue(heByCoupling(term1, term2))
    assertTrue(instanceOf(term1, term2))
  }
  
  def testInstanceOf(input1: String, input2: String, expected: Boolean) = {
    println("TEST")
    val term1 = termFromString(input1)
    val term2 = termFromString(input2)
    val msgTerm = msg(term1, term2).term
    println("term1:")
    println(can(term1))
    println("term2:")
    println(can(term2))
    println("msg:")
    println(can(msgTerm))
    assertEquals(expected, instanceOf(term1, term2))
  }
}
