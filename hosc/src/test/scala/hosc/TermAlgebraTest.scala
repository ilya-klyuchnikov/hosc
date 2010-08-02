package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import HLanguage.{Application => A, _}
import TermAlgebra._
import HE._
import MSG._
import TestUtils._
import Util._

class TermAlgebraTest {
  
  val inputFile = "examples/term_algebra/test.hs"
  
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
    assertTrue(equivalent(f1.body, f2.body))
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
    assertTrue(he(f1.body, f2.body))
  }
  
  @Test def he07(): Unit = {
    val program = programFromFile(inputFile)
    val f4 = program.getFunction("f4").get
    val f5 = program.getFunction("f5").get
    assertFalse(he(f4.body, f5.body))
  }
  
  @Test def he08(): Unit = {
    val program = programFromFile(inputFile)
    val f1 = program.getFunction("f1").get
    val f6 = program.getFunction("f6").get
    assertFalse(he(f1.body, f6.body))
  }
  
  @Test def he09(): Unit = {
    val program = programFromFile(inputFile)
    val f3 = program.getFunction("f3").get
    val f2 = program.getFunction("f2").get
    assertFalse(he(f3.body, f2.body))
  }
  
  @Test def he10(): Unit = {
    val program = programFromFile(inputFile)
    val f3 = program.getFunction("f3").get
    val f2 = program.getFunction("f2").get
    assertFalse(he(f2.body, f3.body))
  }
  
  @Test def he11(): Unit = {
    val program = programFromFile(inputFile)
    val f7 = program.getFunction("f7").get
    val f8 = program.getFunction("f8").get
    assertFalse(he(f7.body, f8.body))
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
  
  @Test def he14(): Unit = {
    val program = programFromFile(inputFile)
    val t01 = termFromString("rev xs", program)
    val t02 = termFromString("app (rev v3)", program)
    assertFalse(heByCoupling(t01, t02))
  }
  
  @Test def he15(): Unit = {
    val program = programFromFile(inputFile)
    val t01 = termFromString("""\x -> S x""", program)
    val t02 = termFromString("""\x -> S (S x)""", program)
    assertFalse(heByCoupling(t01, t02))
  }
  
  @Test def he16(): Unit = {
    val program = programFromFile(inputFile)
    val t01 = termFromString("""\x -> S x""", program)
    val t02 = termFromString("""\y -> (\z -> S z) (S y) """, program)
    assertTrue(he(t01, t02))
    
    val t03 = termFromString("""k""", program)
    val t04 = termFromString("""\y -> k (S y)""", program)
    assertTrue(he(t03, t04))
  }
  
  @Test def he17(): Unit = {
    val program = programFromFile(inputFile)
    val t01 = termFromString("""\x -> S x""", program)
    val t02 = termFromString("""\x -> S (S x) """, program)
    assertFalse(he(t01, t02))
    
    val e01 = termFromString("""a b""", program)
    val e02 = termFromString("""d e f""", program)
    assertFalse(he(e01, e02))
    
    val e03 = termFromString("""a b""", program)
    val e04 = termFromString("""d (e f)""", program)
    assertTrue(he(e03, e04))
    
    val e05 = termFromString("""\x -> a b x""", program)
    val e06 = termFromString("""\x -> d (e f) x""", program)
    assertTrue(he(e05, e06))
    
    val e07 = termFromString("""\x -> a b x""", program)
    val e08 = termFromString("""\x -> (d e) f x""", program)
    assertFalse(he(e07, e08))
    
    // interesting example -> lifting results of  non-embedding to be embedding!
    val e09 = termFromString("""\x -> a x""", program)
    val e10 = termFromString("""\x -> (a b) x """, program)
    assertFalse(he(e09, e10))
    
    val e11 = termFromString("""\x -> ((\y -> y) a) x""", program)
    val e12 = termFromString("""\x -> ((\y -> y) (a b)) x """, program)
    assertTrue(he(e11, e12))
    
    val e13 = termFromString("""(\fu -> \x -> fu x) a""", program)
    val e14 = termFromString("""(\fu -> \x -> fu x) (a b)""", program)
    assertTrue(he(e14, e14))
    
    val e15 = termFromString("""k""", program)
    val e16 = termFromString("""\x -> k x""", program)
    assertTrue(he(e15, e15))
    
    val e17 = termFromString("""k s""", program)
    val e18 = termFromString("""\x -> k s x""", program)
    assertFalse(he(e17, e18))
    
    // \x -> x (a b) !< (\y x -> x y) (a b)
    val e19 = termFromString("""\x -> x (a b)""", program)
    val e20 = termFromString("""(\y x -> x y) (a b)""", program)
    assertFalse(he(e19, e20))
  }
  
  @Test def msg1(): Unit = {
    val program = programFromFile(inputFile)
    val term = termFromString("app x", program)
    val msg_ = msgExt(term, term)
    println(msg_)
    assertTrue(equivalent(msg_.term, term))
  }
  
  @Test def msg2(): Unit = {
    val program = programFromFile(inputFile)
    val term1 = termFromString("app x", program)
    val term2 = termFromString("app y", program)
    val term = termFromString("app z", program)
    val msg_ = msgExt(term1, term2)
    println(msg_)
    assertTrue(equivalent(msg_.term, term))
  }
  
  
  @Test def msg4(): Unit = {
    val actualMsg = msgExt(Variable("a"), Variable("a"))
    println(actualMsg)
    
    val f1 = Variable("f1");   f1.global = true
    val f2 = Variable("f2");   f2.global = true
    val f3 = Variable("f3");   f3.global = true
    val f4 = Variable("f4");   f4.global = true
    val x = Variable("y");
    val y = Variable("y");
    val e1 = A(f1, A(f2, x));
    val e2 = A(f3, A(f1, A(f4, A (f2, y))));
    val msg1 = msgExt(e1, e2);
    println(msg1);
    //assertTrue(equivalent(actualMsg.term, term))
  }
  
  @Test def msg6(): Unit = {
    val program = programFromFile(inputFile)
    val input1 = 
      """
|case (case (a) of {Z  -> b; S v11 -> (S ((plus v11) b));}) of {
|	Z  -> case (((plus b) a)) of {Z  -> (True ); S v3 -> (False );}; 
|	S v4 -> case (((plus b) a)) of {Z  -> (False ); S v5 -> ((eqnum v4) v5);};
|}
      """.stripMargin;
      
    val input2 = 
      """
|case (case (x) of {Z  -> (S y); S v11 -> (S ((plus v11) (S y)));}) of {
|	Z  -> case (((plus y) (S x))) of {Z  -> (True ); S v3 -> (False );}; 
|	S v4 -> case (((plus y) (S x))) of {Z  -> (False ); S v5 -> ((eqnum v4) v5);};
|}
      """.stripMargin;     
    
    val term1 = termFromString(input1, program)
    val term2 = termFromString(input2, program)
    val actualMsg = msgExt(term1, term2)
    println(actualMsg)
  }
}
