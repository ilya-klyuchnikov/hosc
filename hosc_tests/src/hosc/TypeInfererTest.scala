package hosc

import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import HLanguage.{Application => A, Variable => V, CaseExpression => CE, Branch => B, Pattern => P,
  Constructor => C, LambdaAbstraction => L, TypeConstructor => TC, TypeVariable => TV, Arrow => Arr,
  TypeDefinition => TCD, DataConstructor => DC, _}

class TypeInfererTest {
  
  @Test def simpleApplications(): Unit = {
    val ti = new TypeInferer(null)
    val v = L(V("b"), L(V("a"), V("b")))
    val v0 = L(V("a"), V("a"))
    val v1 = L(V("a"), L(V("b"),A(V("a"), V("b"))))
    val v2 = L(V("a"), L(V("b"),V("b")))
    val v3 = L(V("a"), L(V("b"),V("a")))
    val r = ti.tc(TypeInferer.TypeEnv(Nil), v)
    println(r)
    val r0 = ti.tc(TypeInferer.TypeEnv(Nil), v0)
    println(r0)
    val r1 = ti.tc(TypeInferer.TypeEnv(Nil), v1)
    println(r1)
    val r2 = ti.tc(TypeInferer.TypeEnv(Nil), v2)
    println(r2)
    val r3 = ti.tc(TypeInferer.TypeEnv(Nil), v3)
    println(r3)
  }
  
  
  @Test def simpleConstructors(): Unit = {
    val programResult = TestUtils.programResultFromFile("input/rev1.hl")
    assertTrue(programResult.successful)
    val program = programResult.get
    val ti = new TypeInferer(program)
    
    val r0 = ti.tc(TypeInferer.TypeEnv(Nil), C("Nil", Nil))
    println(r0)
    
    val r1 = ti.tc(TypeInferer.TypeEnv(Nil), C("True", Nil))
    println(r1)
    
    val r2 = ti.tc(TypeInferer.TypeEnv(Nil), C("False", Nil))
    println(r2)
    
    val r3 = ti.tc(TypeInferer.TypeEnv(Nil), C("Cons", C("False", Nil) :: C("Nil", Nil)::Nil))
    println(r3)
  }
  
  
  @Test def simpleBranches(): Unit = {
    val programResult = TestUtils.programResultFromFile("input/rev1.hl")
    assertTrue(programResult.successful)
    val program = programResult.get
    val ti = new TypeInferer(program)
    
    val r0 = ti.tcBranch(TypeInferer.TypeEnv(Nil), B(P("Cons", List(V("x"), V("xs"))), V("xs")))
    println(r0)
    
    val r1 = ti.tcBranch(TypeInferer.TypeEnv(Nil), B(P("Cons", List(V("x"), V("xs"))), V("x")))
    println(r1)
    
    val r2 = ti.tcBranch(TypeInferer.TypeEnv(Nil), B(P("Cons", List(V("x"), V("xs"))), C("True", Nil)))
    println(r2)
    
    val r3 = ti.tcBranch(TypeInferer.TypeEnv(Nil), B(P("True", Nil), C("False", Nil)))
    println(r3)
    
    //val r4 = ti.tcBranch(TypeInferer.TypeEnv(Nil), B(P("Z", List(V("x"))), C("False", Nil)))
    //println(r4)
  }
  
  
  @Test def simpleCaseRaw(): Unit = {
    val programResult = TestUtils.programResultFromFile("input/rev1.hl")
    assertTrue(programResult.successful)
    val program = programResult.get
    val ti = new TypeInferer(program)
    
    {
      val b01 = B(P("True", Nil), C("False", Nil))
      val b02 = B(P("False", Nil), C("True", Nil))
    
      val case0 = CE(V("x"), List(b01, b02))
      val r0 = ti.tcCaseRaw(TypeInferer.TypeEnv(Nil), case0)
      println(r0)
    }
    
    println
    
    {
      val b11 = B(P("Cons", List(V("x"), V("xs"))), V("x"))
      val b12 = B(P("Nil", Nil), C("True", Nil))
    
      val case1 = CE(V("x"), List(b11, b12))
      val r1 = ti.tcCaseRaw(TypeInferer.TypeEnv(Nil), case1)
      println(r1)
    }
    
  }
  
  
  @Test def simpleCase(): Unit = {
    val programResult = TestUtils.programResultFromFile("input/rev1.hl")
    assertTrue(programResult.successful)
    val program = programResult.get
    val ti = new TypeInferer(program)
    
    {
      val b01 = B(P("True", Nil), C("False", Nil))
      val b02 = B(P("False", Nil), C("True", Nil))
    
      val case0 = CE(C("True", Nil), List(b01, b02))
      val r0 = ti.tc(TypeInferer.TypeEnv(Nil), case0)
      println(r0)
    }
    
    println
    
    {
      val b11 = B(P("Cons", List(V("x"), V("xs"))), V("x"))
      val b12 = B(P("Nil", Nil), C("True", Nil))
    
      val case1 = CE(C("True", Nil), List(b11, b12))
      //val r1 = ti.tcCase(TypeInferer.TypeEnv(Nil), case1)
      //println(r1)
      null
    }
    
  }
  
  @Test def simpleProgram(): Unit = {
    val programResult = TestUtils.programResultFromFile("input/rev1.hl")
    println(programResult)
    assertTrue(programResult.successful)
    val program = programResult.get
    val ti = new TypeInferer(program)
  
    {
      val pairs = program.fs map {f => (V(f.name), f.lam)}
      //val l = LetExpression(pairs, V("apps"))
      val l = LetRecExpression(pairs, V("rev"))
      val r0 = ti.tc(TypeInferer.TypeEnv(Nil), l)
      println(r0)
    }
  
    println
  
  }
}
