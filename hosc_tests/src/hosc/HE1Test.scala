package hosc;
import org.junit.Test
import org.junit.Ignore
import org.junit.Assert._
import TermAlgebra1._
import HE1.he
import HLanguage1.{Application1 => A, Variable1 => V, CaseExpression1 => CE, Branch1 => B, Pattern1 => P,
  Constructor1 => C, LambdaAbstraction1 => LA, LetRecExpression1 => LR, _}

class HE1Test {

  @Test def he01(): Unit = {
    val appCall = V("app")
    appCall.call = true
    assertTrue(he(appCall, appCall))
  }
  
  @Test def he02(): Unit = {
    val appCall = V("app")
    appCall.call = true
    val revCall = V("rev")
    appCall.call = true
    assertFalse(he(appCall, revCall))
  }
  
  @Test def he03(): Unit = {
    val appCall = V("app")
    appCall.call = true
    assertFalse(he(appCall, V("x")))
  }
  
  @Test def he04(): Unit = {
    val appCall = V("app")
    appCall.call = true
    assertFalse(he(V("y"), appCall))
  }
  
  @Test def he05(): Unit = {
    assertTrue(he(V("y"), V("x")))
  }
  
  @Test def he06(): Unit = {
    val appCall = V("app")
    appCall.call = true
    appCall.label = Loop();
    val revCall = V("app")
    revCall.call = true
    assertFalse(he(appCall, revCall))
    assertFalse(he(revCall, appCall))
  }
  
  @Test def he07(): Unit = {
    val appCall1 = V("app")
    appCall1.call = true
    val appCall2 = V("app")
    appCall2.call = true
    
    val app1 = A(appCall1, V("x"))
    val app2 = A(appCall2, V("x"))
    assertTrue(he(app1, app2))
    assertTrue(he(app2, app1))
    
    app1.label = Repeat()
    assertFalse(he(app1, app2))
    assertFalse(he(app2, app1))
  }
  
  @Test def he08(): Unit = {
    val appCall1 = V("app")
    appCall1.call = true
    val appCall2 = V("app")
    appCall2.call = true
    
    val app1 = A(appCall1, V("x"))
    val app2 = A(appCall2, C("S", V("x")::Nil))
    assertTrue(he(app1, app2))
    
    app1.label = Repeat()
    assertFalse(he(app1, app2))
  }
  
  @Test def he09(): Unit = {
    val call1 = V("f$3")
    call1.call = true
    val case11 = CE(V("$168"), 
        B(P("Z", Nil), C("False", Nil)) ::
        B(P("S", V("$169") :: Nil), A(call1, V("$169"))):: 
        Nil)
    val case12= CE(V("$246"),
        B(P("Z", Nil), C("True", Nil)) ::
        B(P("S", V("$168") :: Nil), case11)::
        Nil)
    
    val case21 = CE(V("$315"), 
        B(P("Z", Nil), C("False", Nil)) ::
        B(P("S", V("$316") :: Nil), A(call1, V("$316"))):: 
        Nil)
    assertTrue(he(case11, case11))
    assertTrue(he(case12, case12))
    //assertTrue(he(case11, case12))
    assertTrue(he(case21, case11))
  }
}
