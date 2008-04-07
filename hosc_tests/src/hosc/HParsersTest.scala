package hosc;

import org.junit.Test
import org.junit.Assert._
import HLanguage.{Application => A, Variable => V, CaseExpression => CE, Branch => B, Pattern => P,
  Constructor => C, LambdaAbstraction => L, TypeConstructor => TC, TypeVariable => TV, Arrow => Arr,
  TypeConstructorDefinition => TCD, DataConstructor => DC, _}

class HParsersTest {
  @Test def simpleApplication(): Unit = {
    val expected = A(A(V("x"), V("y")), V("z"))
    val termText ="""x 
    y 
    z""" 
    val termResult = TestUtils.termResultFromString(termText)    
    println(termResult)
    val a =termResult.get.asInstanceOf[A].arg
    println(a.pos.longString)
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
    val revF = Function("rev", L(V("xs"), 
        CE(V("xs"),
            B(P("Nil", Nil), C("Nil", Nil)) :: 
            B(P("Cons", V("z") :: V("zs") :: Nil), 
                A(A(V("app"), A(V("rev"), V("zs"))), C("Cons", V("z") :: C("Nil", Nil) :: Nil))) ::
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
    val expected = Program(listT :: Nil, 
        revF :: appF :: Nil )
    val programResult = TestUtils.programResultFromFile("input/rev.hl")    
    println(programResult)
    assertTrue(programResult.successful)
    val program = programResult.get
    assertEquals(expected, program)  
  }
  
  @Test def validator(): Unit = {
    val termResult1 = TestUtils.programResultFromFile("input/err01.hl")  
    println(termResult1)
    assertFalse("duplicate type definition should be reported", termResult1.successful)
    
    val termResult2 = TestUtils.programResultFromFile("input/err02.hl")  
    println(termResult2)
    assertFalse("undefined var should be reported", termResult2.successful)
    
    val termResult3 = TestUtils.programResultFromFile("input/err03.hl")  
    println(termResult3)
    assertFalse("wrong numbers of type parameters should be reported", termResult3.successful)
    
    val termResult4 = TestUtils.programResultFromFile("input/err04.hl")  
    println(termResult4)
    assertFalse("wrong numbers of type parameters should be reported", termResult4.successful)
    
    val termResult5 = TestUtils.programResultFromFile("input/err05.hl")  
    println(termResult5)
    assertFalse("undefined var should be reported", termResult5.successful)
    
    val termResult6 = TestUtils.programResultFromFile("input/err06.hl")  
    println(termResult6)
    assertFalse("unknown type list2 should be reported", termResult6.successful)
    
    val termResult7 = TestUtils.programResultFromFile("input/err07.hl")  
    println(termResult7)
    assertFalse("duplicate data constructor Cons should be reported", termResult7.successful)
    
    val termResult8 = TestUtils.programResultFromFile("input/err08.hl")  
    println(termResult8)
    assertFalse("duplicate type var $a should be reported", termResult8.successful)
    
    val termResult9 = TestUtils.programResultFromFile("input/err09.hl")  
    println(termResult9)
    assertFalse("useless type type var $b should be reported", termResult9.successful)
  }
  
}
