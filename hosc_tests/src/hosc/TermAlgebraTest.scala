package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import HLanguage._
import TermAlgebra._
import Util._

class TermAlgebraTest {
  
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
    val program = programFromFile("input/ta.hl")
    val f1 = program.getFunction("f1").get
    val f2 = program.getFunction("f2").get
    assertTrue(equivalent(f1.lam, f2.lam))
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
    val program = programFromFile("input/ta.hl")
    val f1 = program.getFunction("f1").get
    val f2 = program.getFunction("f2").get
    assertTrue(he(f1.lam, f2.lam))
  }
  
  @Test def he07(): Unit = {
    val program = programFromFile("input/ta.hl")
    val f4 = program.getFunction("f4").get
    val f5 = program.getFunction("f5").get
    assertTrue(he(f4.lam, f5.lam))
  }
  
  @Test def he08(): Unit = {
    val program = programFromFile("input/ta.hl")
    val f1 = program.getFunction("f1").get
    val f6 = program.getFunction("f6").get
    assertTrue(he(f1.lam, f6.lam))
  }
  
  @Test def he09(): Unit = {
    val program = programFromFile("input/ta.hl")
    val f3 = program.getFunction("f3").get
    val f2 = program.getFunction("f2").get
    assertTrue(he(f3.lam, f2.lam))
  }
  
  @Test def he10(): Unit = {
    val program = programFromFile("input/ta.hl")
    val f3 = program.getFunction("f3").get
    val f2 = program.getFunction("f2").get
    assertFalse(he(f2.lam, f3.lam))
  }
  
  @Test def he11(): Unit = {
    val program = programFromFile("input/ta.hl")
    val f7 = program.getFunction("f7").get
    val f8 = program.getFunction("f8").get
    assertTrue(he(f7.lam, f8.lam))
  }
  
  @Test def he12(): Unit = {
    val program = programFromFile("input/ta.hl")
    val appCall = termFromString("app x", program)
    assertTrue(he(appCall, appCall))
  }
  
  @Test def mgu01(): Unit = {
    val program = programFromFile("input/ta.hl")
    val term = termFromString("app x", program)
    val msg_ = msg(term, term)
    println(msg_)
    assertTrue(equivalent(msg_.term, term))
  }
  
  @Test def mgu02(): Unit = {
    val program = programFromFile("input/ta.hl")
    val term1 = termFromString("app x", program)
    val term2 = termFromString("app y", program)
    val term = termFromString("app z", program)
    val msg_ = msg(term1, term2)
    println(msg_)
    assertTrue(equivalent(msg_.term, term))
  }
  
  @Test def mgu03(): Unit = {
    val program = programFromFile("input/ta.hl")
    val input1 = 
      """
        |case rev x of {
        |  Nil : Nil;
        |  Cons a1 b1 : app (rev b1) (Cons a1 Nil);
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
        |  Cons a1 b1 : app (rev z) (Cons a1 Nil);
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
