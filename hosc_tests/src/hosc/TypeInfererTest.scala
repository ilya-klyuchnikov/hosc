package hosc

import org.junit.Test
import org.junit.Assert._
import HLanguage.{Application => A, Variable => V, CaseExpression => CE, Branch => B, Pattern => P,
  Constructor => C, LambdaAbstraction => L, TypeConstructor => TC, TypeVariable => TV, Arrow => Arr,
  TypeConstructorDefinition => TCD, ArrowDefinition => AD, DataConstructor => DC, _}

class TypeInfererTest {
  @Test def simpleApplication(): Unit = {
    val v = L(V("b"), L(V("a"), V("b")))
    val v1 = L(V("a"), L(V("b"),A(V("a"), V("b"))))
    val r = TypeInferer.tc(TypeInferer.TypeEnv(Nil), v)
    println(r)
    val r1 = TypeInferer.tc(TypeInferer.TypeEnv(Nil), v1)
    println(r1)
  }
}
