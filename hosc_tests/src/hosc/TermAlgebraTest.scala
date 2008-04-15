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
  
}
