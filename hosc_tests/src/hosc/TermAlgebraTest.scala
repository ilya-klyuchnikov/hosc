package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import HLanguage.{Application => A, _}
import sc0.TermAlgebra0._
import sc0.HE0._
import TestUtils._
import Util._

class TermAlgebraTest {
  
  val inputFile = "hl/term_algebra/01.hl"
  
  @Test def equivalency01(): Unit = {
    val appCall = Variable("app")
    appCall.global = true
    assertTrue(equivalent(appCall, appCall))
  }
  
  @Test def equivalency02(): Unit = {
    val appCall = Variable("app")
    appCall.global = true
    val revCall = Variable("rev")
    appCall.global = true
    assertFalse(equivalent(appCall, revCall))
  }
  
  @Test def equivalency03(): Unit = {
    val appCall = Variable("app")
    appCall.global = true
    assertFalse(equivalent(appCall, Variable("x")))
  }
  
  @Test def equivalency04(): Unit = {
    val appCall = Variable("app")
    appCall.global = true
    assertFalse(equivalent(Variable("y"), appCall))
  }
  
  @Test def equivalency05(): Unit = {
    assertTrue(equivalent(Variable("y"), Variable("x")))
  }
  
  @Test def equivalency06(): Unit = {
    val program = programFromFile(inputFile)
    val f1 = program.getFunction("f1").get
    val f2 = program.getFunction("f2").get
    assertTrue(equivalent(f1.lam, f2.lam))
  }
  
  @Test def equivalency07(): Unit = {
    val program = programFromFile(inputFile)
    val term1 = termFromString("app (app x y) z", program)
    val term2 = termFromString("app (app x y) x", program)
    assertFalse(equivalent(term1, term2))
  }
  
  @Test def he01(): Unit = {
    val appCall = Variable("app")
    appCall.global = true
    assertTrue(he(appCall, appCall))
  }
  
  @Test def he02(): Unit = {
    val appCall = Variable("app")
    appCall.global = true
    val revCall = Variable("rev")
    appCall.global = true
    assertFalse(he(appCall, revCall))
  }
  
  @Test def he03(): Unit = {
    val appCall = Variable("app")
    appCall.global = true
    assertFalse(he(appCall, Variable("x")))
  }
  
  @Test def he04(): Unit = {
    val appCall = Variable("app")
    appCall.global = true
    assertFalse(he(Variable("y"), appCall))
  }
  
  @Test def he05(): Unit = {
    assertTrue(he(Variable("y"), Variable("x")))
  }
  
  @Test def he06(): Unit = {
    val program = programFromFile(inputFile)
    val f1 = program.getFunction("f1").get
    val f2 = program.getFunction("f2").get
    assertTrue(he(f1.lam, f2.lam))
  }
  
  @Test def he07(): Unit = {
    val program = programFromFile(inputFile)
    val f4 = program.getFunction("f4").get
    val f5 = program.getFunction("f5").get
    assertFalse(he(f4.lam, f5.lam))
  }
  
  @Test def he08(): Unit = {
    val program = programFromFile(inputFile)
    val f1 = program.getFunction("f1").get
    val f6 = program.getFunction("f6").get
    assertFalse(he(f1.lam, f6.lam))
  }
  
  @Test def he09(): Unit = {
    val program = programFromFile(inputFile)
    val f3 = program.getFunction("f3").get
    val f2 = program.getFunction("f2").get
    assertFalse(he(f3.lam, f2.lam))
  }
  
  @Test def he10(): Unit = {
    val program = programFromFile(inputFile)
    val f3 = program.getFunction("f3").get
    val f2 = program.getFunction("f2").get
    assertFalse(he(f2.lam, f3.lam))
  }
  
  @Test def he11(): Unit = {
    val program = programFromFile(inputFile)
    val f7 = program.getFunction("f7").get
    val f8 = program.getFunction("f8").get
    assertFalse(he(f7.lam, f8.lam))
  }
  
  @Test def he12(): Unit = {
    val program = programFromFile(inputFile)
    val appCall = termFromString("app x", program)
    assertTrue(he(appCall, appCall))
  }
  
  @Test def he13(): Unit = {
    val program = programFromFile(inputFile)
    val t01 = termFromString("app x y", program)
    val t02 = termFromString("app x", program)
    assertFalse(he(t01, t02))
    val t1 = termFromString("((app ((app x) y)) z)", program)
    val t2 = termFromString("((app x) y)", program)
    assertFalse(he(t1, t2))
  }
  
  @Test def msg1(): Unit = {
    val program = programFromFile(inputFile)
    val term = termFromString("app x", program)
    val msg_ = msg(term, term)
    println(msg_)
    assertTrue(equivalent(msg_.term, term))
  }
  
  @Test def msg2(): Unit = {
    val program = programFromFile(inputFile)
    val term1 = termFromString("app x", program)
    val term2 = termFromString("app y", program)
    val term = termFromString("app z", program)
    val msg_ = msg(term1, term2)
    println(msg_)
    assertTrue(equivalent(msg_.term, term))
  }
  
  @Test def msg3(): Unit = {
    val program = programFromFile(inputFile)
    val input1 = 
      """
        |case rev x of {
        |  Nil : Nil;
        |  Cons a1 b1 : app (rev b1) (Cons aa1 Nil);
        |}
      """.stripMargin;
      
    val input2 = 
      """
        |case x of {
        |  Nil : Nil;
        |  Cons a1 b1 : app (rev (rev b1)) (Cons a1 Nil);
        |}
      """.stripMargin;     
    
    val expectedMsgInput = 
      """
        |case y of {
        |  Nil : Nil;
        |  Cons a1 b1 : app (rev z) (Cons aa1 Nil);
        |}
      """.stripMargin;
    val term1 = termFromString(input1, program)
    val term2 = termFromString(input2, program)
    val term = termFromString(expectedMsgInput, program)
    val actualMsg = msg(term1, term2)
    println(actualMsg)
    assertTrue(equivalent(actualMsg.term, term))
  }
  
  @Test def msg4(): Unit = {
    val actualMsg = msg(Variable("a"), Variable("a"))
    println(actualMsg)
    
    val f1 = Variable("f1");   f1.global = true
    val f2 = Variable("f2");   f2.global = true
    val f3 = Variable("f3");   f3.global = true
    val f4 = Variable("f4");   f4.global = true
    val x = Variable("y");
    val y = Variable("y");
    val e1 = A(f1, A(f2, x));
    val e2 = A(f3, A(f1, A(f4, A (f2, y))));
    val msg1 = msg(e1, e2);
    println(msg1);
    //assertTrue(equivalent(actualMsg.term, term))
  }
  
  @Test def msg5(): Unit = {
    val program = programFromFile(inputFile)
    val input1 = 
      """
        |case rev x of {
        |  Nil : Nil;
        |  Cons a1 b1 : app (rev b1) (Cons aa1 Nil);
        |}
      """.stripMargin;
      
    val input2 = 
      """
        |case x of {
        |  Nil : Nil;
        |  Cons a1 b1 : app (rev (rev b1)) (Cons a1 Nil);
        |}
      """.stripMargin;     
    
    val expectedMsgInput = 
      """
        |case y of {
        |  Nil : Nil;
        |  Cons a1 b1 : app (rev z) (Cons aa1 Nil);
        |}
      """.stripMargin;
    val term1 = termFromString(input1, program)
    val term2 = termFromString(input2, program)
    val term = termFromString(expectedMsgInput, program)
    val actualMsg = msg(term1, term2)
    println(actualMsg)
    assertTrue(equivalent(actualMsg.term, term))
  }
}
