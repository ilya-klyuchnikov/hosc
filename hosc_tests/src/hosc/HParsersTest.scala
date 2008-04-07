package hosc;

import org.junit.Test
import org.junit.Assert._
import HLanguage.{Application => A, Variable => V, CaseExpression => CE, Branch => B, Pattern => P,
  Constructor => C, LambdaAbstraction => L, TypeConstructor => TC, TypeVariable => TV, Arrow => Arr,
  TypeConstructorDefinition => TCD, DataConstructor => DC, _}

class HParsersTest {
  @Test def application(): Unit = {
    testSTerm(
        "a", 
        V("a"));
    
    testSTerm(
        "a b", 
        A(V("a"), V("b")));
    
    testSTerm(
        "x y z", 
        A(A(V("x"), V("y")), V("z")));
    
    testSTerm(
        "(x y) z", 
        A(A(V("x"), V("y")), V("z")));
    
    testSTerm(
        "x (y z)", 
        A(V("x"), A(V("y"), V("z"))));
    
  }
  
  @Test def constructor(): Unit = {
    testSTerm(
        "A", 
        C("A", Nil));
    
    testSTerm(
        "A b", 
        C("A", V("b") :: Nil));
    
    testSTerm(
        "(A b)", 
        C("A", V("b") :: Nil));
    
    testSTerm(
        "A (b)", 
        C("A", V("b") :: Nil));
    
    testETerm("(A) b");
    
    testSTerm(
        "A B C", 
        C("A", C("B", Nil) :: C("C", Nil) :: Nil));
    
    testSTerm(
        "A (B C)", 
        C("A", C("B", C("C", Nil) :: Nil) :: Nil));
    
    testETerm("(A B) C")
    
    testSTerm(
        "Data1 Data2 x y z", 
        C("Data1", C("Data2", Nil) :: V("x") :: V("y") :: V("z") :: Nil));
    
    testSTerm(
        "Data1 (Data2 x y) z", 
        C("Data1", C("Data2", V("x") :: V("y") :: Nil) :: V("z") :: Nil));
    
  }
  
  @Test def termAssociativity(): Unit = {
    testSTerm(
        "a Cons b c", 
        A(A(A(V("a"), C("Cons", Nil)), V("b")), V("c")));
    
    testETerm("(A V) e");
    
    testSTerm(
        "x A y", 
        A(A(V("x"), C("A", Nil)), V("y")));
    
    testSTerm(
        "x y z v", 
        A(A(A(V("x"), V("y")), V("z")), V("v")) );
    
    testSTerm(
        "(x y) z v", 
        A(A(A(V("x"), V("y")), V("z")), V("v")) );
    
    testSTerm(
        "(x y) (z v)", 
        A(A(V("x"), V("y")), A(V("z"), V("v")))) ;
    
    testSTerm(
        "x Y z V", 
        A(A(A(V("x"), C("Y", Nil)), V("z")), C("V", Nil)) );
    
    testSTerm(
        "(x Y) z V", 
        A(A(A(V("x"), C("Y", Nil)), V("z")), C("V", Nil)) );
    
    testSTerm(
        "(x Y) (z V)", 
        A(A(V("x"), C("Y", Nil)), A(V("z"), C("V", Nil)))) ;
    
    testSTerm(
        "x (A y)", 
        A(V("x"), C("A", V("y") :: Nil)));
    
  }
  
  @Test def caseExpression(): Unit = {
    testSTerm(
        "case x of {Nil : Nil;}", 
        CE(V("x"), List(B(P("Nil", Nil), C("Nil", Nil))))) ; 
  }
  
  @Test def lambdaAbstraction(): Unit = {
    testSTerm(
        "%x {%y {Cons x y}}", 
        L(V("x"), L(V("y"), C("Cons", List(V("x"), V("y")))))) ; 
  }
  
  @Test def arrow(): Unit = {
    testSType(
        "$a -> $b -> $c", 
        Arr(TV("$a"), Arr(TV("$b"), TV("$c"))));
    
    testSType(
        "$a -> ($b -> $c)", 
        Arr(TV("$a"), Arr(TV("$b"), TV("$c"))));
    
    testSType(
        "($a -> $b) -> $c", 
        Arr(Arr(TV("$a"), TV("$b")), TV("$c")));
  }
  
  @Test def typeConstructor(): Unit = {
      // tc1 tc2 $a tc2 $a = tc1 (tc2) $a (tc2) $a
      testSType(
          "tc1 tc2 $a tc2 $a", 
          TC("tc1", TC("tc2", Nil) :: TV("$a") :: TC("tc2", Nil) :: TV("$a") :: Nil ));
      testEType("(tc1 tc2) $a tc2 $a")
      
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
  
  def testSTerm(input: String, expected: Term): Unit = {
    val r = TestUtils.termResultFromString(input)
    println(r)
    assertTrue(r.successful)
    assertEquals(expected, r.get)
  }
  
  def testETerm(input: String): Unit = {
    val r = TestUtils.termResultFromString(input)
    println(r)
    assertFalse(r.successful)
  }
  
  def testSType(input: String, expected: Type): Unit = {
    val r = TestUtils.typeExprResultFromString(input)
    println(r)
    assertTrue(r.successful)
    assertEquals(expected, r.get)
  }
  
  def testEType(input: String): Unit = {
    val r = TestUtils.typeExprResultFromString(input)
    println(r)
    assertFalse(r.successful)
  }
  
}
