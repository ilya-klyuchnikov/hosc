package hosc;

import org.junit.Test
import org.junit.Assert._
import HLanguage._

class HParsersTest {
  @Test def simpleApplication(): Unit = {
    val expected = Application(Application(Variable("x"), Variable("y")), Variable("z"))
    val termText ="""x y z""" 
    val termResult = TestUtils.termResultFromString(termText)    
    println(termResult)
    assertTrue(termResult.successful)
    assertEquals(expected, termResult.get)    
  }
  
  @Test def simpleCaseExpression(): Unit = {
    val expected = CaseExpression(Variable("x"), List(Branch(Pattern("Nil", Nil), Constructor("Nil", Nil))))
    val termText ="""case x of {Nil : Nil;}"""    
    val termResult = TestUtils.termResultFromString(termText)    
    println(termResult)
    assertTrue(termResult.successful)
    assertEquals(expected, termResult.get)  
  }
  
  @Test def simpleLambdaAbstraction(): Unit = {
    val expected = LambdaAbstraction(Variable("x"), Constructor("Cons", List(Variable("x"), Constructor("Nil", Nil))))
    val termText ="""%x {Cons x Nil}"""    
    val termResult = TestUtils.termResultFromString(termText)    
    println(termResult)
    assertTrue(termResult.successful)
    assertEquals(expected, termResult.get)  
  }
  
  @Test def simpleConstructor1(): Unit = {
    val expected = Constructor("Data1", Constructor("Data2", Variable("x") :: Variable("y") :: Variable("z") :: Nil) :: Nil)
    val termText ="""Data1 Data2 x y z"""    
    val termResult = TestUtils.termResultFromString(termText)    
    println(termResult)
    assertTrue(termResult.successful)
    assertEquals(expected, termResult.get)  
  }
  
  @Test def simpleConstructor2(): Unit = {
    val expected = Constructor("Data1", 
                               Constructor("Data2", Variable("x") :: Variable("y") :: Nil) :: Variable("z") :: Nil)
    val termText ="""Data1 (Data2 x y) z"""    
    val termResult = TestUtils.termResultFromString(termText)    
    println(termResult)
    assertTrue(termResult.successful)
    assertEquals(expected, termResult.get)  
  }
}
