package hosc

import org.junit.Test
import org.junit.Assert._
import HLanguage.{Application => A, Variable => V, CaseExpression => CE, Branch => B, Pattern => P,
  Constructor => C, LambdaAbstraction => L, TypeConstructor => TC, TypeVariable => TV, Arrow => Arr,
  TypeConstructorDefinition => TCD, ArrowDefinition => AD, DataConstructor => DC, _}

class TypeInfererTest {
  @Test def simpleApplication(): Unit = {
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
  
  @Test def s(): Unit = {
    val programResult = TestUtils.programResultFromFile("input/rev.hl")
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
}
