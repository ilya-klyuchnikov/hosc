package hosc;

import org.junit.Test
import org.junit.Assert._
import HLanguage.{Application => A, Variable => V, CaseExpression => CE, Branch => B, Pattern => P,
  Constructor => C, LambdaAbstraction => L, TypeConstructor => TC, TypeVariable => TV, Arrow => Arr,
  TypeConstructorDefinition => TCD, ArrowDefinition => AD, DataConstructor => DC, _}

class HParsersTest {
  @Test def simpleApplication(): Unit = {
    val expected = A(A(V("x"), V("y")), V("z"))
    val termText ="""x y z""" 
    val termResult = TestUtils.termResultFromString(termText)    
    println(termResult)
    assertTrue(termResult.successful)
    assertEquals(expected, termResult.get)    
  }
  
  @Test def simpleCaseExpression(): Unit = {
    val expected = CE(V("x"), List(B(P("Nil", Nil), C("Nil", Nil))))
    val termText ="""case x of {Nil : Nil;}"""    
    val termResult = TestUtils.termResultFromString(termText)    
    println(termResult)
    assertTrue(termResult.successful)
    assertEquals(expected, termResult.get)  
  }
  
  @Test def simpleLambdaAbstraction(): Unit = {
    val expected = L(V("x"), C("Cons", List(V("x"), C("Nil", Nil))))
    val termText ="""%x {Cons x Nil}"""    
    val termResult = TestUtils.termResultFromString(termText)    
    println(termResult)
    assertTrue(termResult.successful)
    assertEquals(expected, termResult.get)  
  }
  
  @Test def simpleConstructor1(): Unit = {
    val expected = C("Data1", C("Data2", V("x") :: V("y") :: V("z") :: Nil) :: Nil)
    val termText ="""Data1 Data2 x y z"""    
    val termResult = TestUtils.termResultFromString(termText)    
    println(termResult)
    assertTrue(termResult.successful)
    assertEquals(expected, termResult.get)  
  }
  
  @Test def simpleConstructor2(): Unit = {
    val expected = C("Data1", C("Data2", V("x") :: V("y") :: Nil) :: V("z") :: Nil)
    val termText ="""Data1 (Data2 x y) z"""    
    val termResult = TestUtils.termResultFromString(termText)    
    println(termResult)
    assertTrue(termResult.successful)
    assertEquals(expected, termResult.get)  
  }
  
  @Test def simpleProgram(): Unit = {
    val listT = TCD("list", TV("$a") :: Nil, DC("Nil", Nil) :: DC("Cons", TV("$a") :: TC("list", TV("$a") :: Nil) :: Nil) :: Nil)
    val revT = AD("rev", Arr(TC("list", TV("$a") :: Nil),  TC("list", TV("$a") :: Nil)))
    val appT = AD("app", Arr(TC("list", TV("$a") :: Nil),  TC("list", TV("$a") :: Nil)))
    val revF = Function("rev", L(V("xs"), 
        CE(V("xs"),
            B(P("Nil", Nil), C("Nil", Nil)) :: 
            B(P("Cons", V("z") :: V("zs") :: Nil), C("Cons", A(V("app"), A(V("rev"), V("zs"))) :: V("z") :: Nil)) ::
            Nil
            )
        ))
    val appF = Function("app", L(V("xs"), L(V("ys"), 
        CE(V("xs"),
            B(P("Nil", Nil), V("ys")) :: 
            B(P("Cons", V("z") :: V("zs") :: Nil), C("Cons", V("z") :: A(A(V("app"), V("zs")), V("xs")) :: Nil)) ::
            Nil
            )
        )))
    val expected = Program(listT :: revT :: appT :: Nil, 
        revF :: appF :: Nil )
    val programResult = TestUtils.programResultFromFile("input/rev.hl")    
    println(programResult)
    assertTrue(programResult.successful)
    val program = programResult.get
    assertEquals(expected, program)  
  }
  
}
