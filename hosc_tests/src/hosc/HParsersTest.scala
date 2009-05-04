package hosc;

import org.junit.Test
import org.junit.Assert._
import HLanguage.{Application => A, Variable => V, CaseExpression => CE, Branch => B, Pattern => P,
  Constructor => C, LambdaAbstraction => L, _}

import hosc.{TypeConstructor => TC, TypeVariable => TV, Arrow => Arr,
  TypeConstructorDefinition => TCD, DataConstructor => DC}

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
        "case x of {Nil -> Nil;}", 
        CE(V("x"), List(B(P("Nil", Nil), C("Nil", Nil))))) ; 
  }
  
  @Test def lambdaAbstraction(): Unit = {
    testSTerm(
        "\\x -> (\\y -> (Cons x y))", 
        L(V("x"), L(V("y"), C("Cons", List(V("x"), V("y")))))) ; 
  }
  
  @Test def typeConstructor(): Unit = {
      // TC1 TC2 a TC2 a = TC1 (tC2) a (TC2) a
      testSType(
          "TC1 TC2 a TC2 a", 
          TC("TC1", TC("TC2", Nil) :: TV("a") :: TC("TC2", Nil) :: TV("a") :: Nil ));
      testEType("(TC1 TC2) a TC2 a")
      
  }
  
  @Test def typeArrow(): Unit = {
    testSType("a -> b", Arr(TV("a"), TV("b")));
    testSType("a -> b -> c", Arr(TV("a"), Arr(TV("b"), TV("c"))));
  }
  
  @Test def simpleProgram(): Unit = {
    val goal = A(V("rev"), V("x"))
    val listT = TCD("List", TV("a") :: Nil, DC("Nil", Nil) :: DC("Cons", TV("a") :: TC("List", TV("a") :: Nil) :: Nil) :: Nil)
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
            B(P("Cons", V("z") :: V("zs") :: Nil), C("Cons", V("z") :: A(A(V("app"), V("zs")), V("ys")) :: Nil)) ::
            Nil
            )
        )))
    val expected = Program(listT :: Nil, goal, revF :: appF :: Nil )
    val programResult = TestUtils.programResultFromFile("parser/rev.hs")    
    println(programResult)
    assertTrue(programResult.successful)
    val program = programResult.get
    assertEquals(expected, program)  
  }
  
  @Test def validator(): Unit = {
    assertValidationError("parser/err01.hs",
        "duplicate type definition should be reported")
    assertValidationError("parser/err02.hs",
        "undefined var should be reported")
    assertValidationError("parser/err03.hs",
        "wrong numbers of type parameters should be reported")
    assertValidationError("parser/err04.hs",
        "wrong numbers of type parameters should be reported")
    assertValidationError("parser/err05.hs",
        "undefined constructors should be reported")
    assertValidationError("parser/err06.hs",
        "unknown type List2 should be reported")
    assertValidationError("parser/err07.hs",
        "duplicate data constructor Cons should be reported")
    assertValidationError("parser/err08.hs",
        "duplicate type var a should be reported")
    assertValidationError("parser/err09.hs",
        "useless type type var b should be reported");
    assertValidationError("parser/err10.hs",
        "unbound var x should be reported");
    assertValidationError("parser/err11.hs",
        "wrong number of parameters for constructor Cons should be reported");
    assertValidationError("parser/err12.hs",
        "undefined constructor Cons2 should be reported");
    assertValidationError("parser/err13.hs",
        "undefined constructor Nil2 should be reported");
    assertValidationError("parser/err14.hs",
        "undefined (for type list) constructor Cons2 should be reported");
    assertValidationError("parser/err15.hs",
        "wrong number of parameters for constructor Cons should be reported");
    assertValidationError("parser/err16.hs",
        "duplicate var z should be reported");
    assertValidationError("parser/err17.hs",
        "non exhaustive should be reported");
  }
  
  @Test def caseError(): Unit = {
    assertSyntaxError("parser/case.hs", "missing ; should be reported")
  }
  
  @Test def parserErrors(): Unit = {
    assertSyntaxError("parser/syntax_err.hs", "")
  }
  
  def assertValidationError(fileName: String, msg: String)  = {
    println(fileName)
    val r = TestUtils.programResultFromFile(fileName)  
    println(r)
    assertFalse(msg, r.successful)
    r match {
      case x : HParsers.HError => ()
      case _ => fail("validation error is expected: " + msg)
    }
  }
  
  def assertSyntaxError(fileName: String, msg: String)  = {
    println(fileName)
    val r = TestUtils.programResultFromFile(fileName)  
    println(r)
    assertFalse(msg, r.successful)
    r match {
      case x : HParsers.HError => fail("syntax error is expected")
      case _ => 
    }
  }
  
  def testSTerm(input: String, expected: Expression): Unit = {
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
